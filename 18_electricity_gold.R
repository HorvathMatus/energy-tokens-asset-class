# =============================================================================
# 18_electricity_gold.R   (JCM preparation)
#
#   (E) Do energy tokens load on electricity prices, the market their platforms
#       actually serve? Day-ahead prices for Germany-Luxembourg, Slovenia, and
#       northern Italy (Energy-Charts, CC BY 4.0) and for New South Wales
#       (AEMO NEM). Horse race of the crypto block against an electricity
#       block, energy-commodity futures, metals, and energy equities, at daily
#       (contemporaneous and lead-lag), weekly, and monthly frequency.
#   (G) Contrast with gold-backed tokens (PAXG, XAUT), which carry a
#       redeemable claim on physical gold: do they load on gold or on crypto?
#
# Electricity returns are log changes of daily (weekly, monthly) average
# prices. Daily changes are demeaned by weekday to remove the weekly cycle.
#
# Outputs: Output/jcm/horse_race_electricity.csv, block_r2_electricity.csv,
#          gold_tokens.csv, electricity_daily.csv
#          julia/Data/data_<SYS>_elec.txt, data_PAXG.txt, data_XAUT.txt
# =============================================================================

rm(list = ls())
suppressMessages({ library(tidyverse); library(sandwich); library(jsonlite) })

load("Data/returns_full30.RData")
dir.create("Output/jcm", showWarnings = FALSE, recursive = TRUE)
START <- as.Date("2020-12-07"); END <- as.Date("2026-01-31")

CRYPTO_BLOCK <- c(CRYPTO, CRYPTO_IX)                       # BTC ETH SOL AVAX SPTOP20 DPI IDEX
EN_FUT  <- c("B", "CL", "HH", "EUA")                       # energy-commodity futures
METALS  <- c("GC", "HG")
EQUITY  <- c("QCLN", "PBW", "PBD", "ENPH", "NEE", "ICLN", "FENY", "LIT")
GREEN   <- ESG
ELEC    <- c("EL_DE", "EL_SI", "EL_ITN", "EL_AU")

nw_lag <- function(n) floor(4 * (n / 100)^(2 / 9))
zs <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
lagv <- function(x, k) if (k >= 0) c(rep(NA, k), head(x, length(x) - k)) else c(tail(x, length(x) + k), rep(NA, -k))
wald_hac <- function(fit, cols) {
  nm <- names(coef(fit)); R <- matrix(0, length(cols), length(nm)); R[cbind(seq_along(cols), match(cols, nm))] <- 1
  V <- NeweyWest(fit, lag = nw_lag(nobs(fit)), prewhite = FALSE); d <- R %*% coef(fit)
  W <- as.numeric(t(d) %*% solve(R %*% V %*% t(R)) %*% d); pchisq(W, nrow(R), lower.tail = FALSE)
}

# ── Electricity: daily average day-ahead prices ──────────────────────────────
read_ec <- function(zone) {
  files <- list.files("Raw_Data/Electricity/energy_charts", pattern = paste0("^", zone, "_\\d{4}\\.json$"), full.names = TRUE)
  bind_rows(lapply(files, function(f) {
    j <- fromJSON(f)
    tibble(t = as.POSIXct(j$unix_seconds, origin = "1970-01-01", tz = "UTC"), p = as.numeric(j$price))
  })) %>%
    mutate(Date = as.Date(format(t, tz = "Europe/Berlin", usetz = FALSE))) %>%
    group_by(Date) %>% summarise(price = mean(p, na.rm = TRUE), .groups = "drop")
}
read_aemo <- function() {
  files <- list.files("Raw_Data/Electricity/aemo", pattern = "NSW1\\.csv$", full.names = TRUE)
  bind_rows(lapply(files, function(f) read_csv(f, show_col_types = FALSE, col_types = cols(.default = "c")))) %>%
    transmute(t = as.POSIXct(SETTLEMENTDATE, format = "%Y/%m/%d %H:%M:%S", tz = "Australia/Brisbane") - 1,
              p = as.numeric(RRP)) %>%
    mutate(Date = as.Date(format(t, tz = "Australia/Brisbane"))) %>%
    group_by(Date) %>% summarise(price = mean(p, na.rm = TRUE), .groups = "drop")
}
elec <- list(EL_DE = read_ec("DE-LU"), EL_SI = read_ec("SI"), EL_ITN = read_ec("IT-North"), EL_AU = read_aemo())
elec_daily <- reduce(imap(elec, ~ rename(.x, !!.y := price)), full_join, by = "Date") %>%
  filter(Date >= START - 40, Date <= END) %>% arrange(Date)
write_csv(elec_daily, "Output/jcm/electricity_daily.csv")

# business-day log changes, weekday-demeaned
bd <- R30$Date
E <- tibble(Date = bd)
for (z in ELEC) {
  p <- elec_daily[[z]][match(bd, elec_daily$Date)]
  prev <- elec_daily[[z]][match(c(bd[1] - 3, head(bd, -1)), elec_daily$Date)]
  r <- ifelse(p > 0 & prev > 0, log(p / prev), NA)
  wd <- weekdays(bd)
  E[[z]] <- r - ave(r, wd, FUN = function(x) mean(x, na.rm = TRUE))
}
D <- R30 %>% left_join(E, by = "Date")

# ── Gold-backed tokens: exchange closes shifted to the CoinGecko 00:00 UTC convention
read_gold_token <- function(f) {
  read_csv(f, show_col_types = FALSE) %>% transmute(Date = as.Date(date) + 1, price = as.numeric(close))
}
gt <- list(PAXG = read_gold_token("Raw_Data/GoldTokens/PAXG_binance.csv"),
           XAUT = read_gold_token("Raw_Data/GoldTokens/XAUT_bitfinex.csv"))
for (g in names(gt)) {
  p <- gt[[g]]$price[match(bd, gt[[g]]$Date)]
  prev <- gt[[g]]$price[match(c(bd[1] - 3, head(bd, -1)), gt[[g]]$Date)]
  D[[g]] <- log(p / prev)
}

# ── Align exchange-traded series with the token day (see 09b_align_returns.R):
#    the session dated t-1 closes within the UTC day covered by the token return
#    dated t. European day-ahead prices are not shifted, because the price of
#    delivery day t is published around noon CET on day t-1, inside the same
#    token day. The NSW daily average of local day t is complete only at 14:00 UTC
#    on day t, inside the token day dated t+1, so it is lagged like the exchange
#    series. D_raw keeps the same-dated returns. The aligned Julia inputs
#    (*_shift.txt) are written at the end of this script.
EXCH <- c("SPTOP20", ESG, COMMOD, ENERGY_EQ)
LAGGED <- c(EXCH, "EL_AU")
D_raw <- D
for (a in LAGGED) D[[a]] <- c(NA, head(D[[a]], -1))

# ── Aggregations ─────────────────────────────────────────────────────────────
weekly_returns <- function(cols) {
  D %>% mutate(wk = floor_date(Date, "week", week_start = 1)) %>% group_by(wk) %>%
    summarise(across(all_of(cols), ~ if (all(is.na(.x))) NA_real_ else sum(.x, na.rm = TRUE)), n = n(), .groups = "drop") %>%
    filter(n >= 3)
}
# Weekly electricity averages use the delivery days whose prices become known
# within the token week (snapshots from Friday to Friday 00:00 UTC): Saturday to
# Friday for the day-ahead zones, and Friday to Thursday for New South Wales,
# whose local day ends at 14:00 UTC. Monthly averages use calendar months.
WEEK_SHIFT <- c(EL_DE = 2, EL_SI = 2, EL_ITN = 2, EL_AU = 3)
avg_price_changes <- function(unit) {
  bind_cols(lapply(ELEC, function(z) {
    sh <- if (unit == "week") WEEK_SHIFT[[z]] else 0
    elec_daily %>% filter(Date >= START - 7, Date <= END) %>% mutate(per = floor_date(Date + sh, unit, week_start = 1)) %>%
      filter(per >= floor_date(START, unit, week_start = 1)) %>%
      group_by(per) %>% summarise(m = mean(.data[[z]], na.rm = TRUE), .groups = "drop") %>%
      mutate(!!z := c(NA, diff(log(pmax(m, 1e-6))))) %>% select(all_of(z))
  })) %>% bind_cols(elec_daily %>% filter(Date >= START - 7, Date <= END) %>%
    mutate(per = floor_date(Date + (if (unit == "week") 2 else 0), unit, week_start = 1)) %>%
    filter(per >= floor_date(START, unit, week_start = 1)) %>% distinct(per), .)
}
assets_all <- c(TOKENS_ALL, "PAXG", "XAUT", CRYPTO_BLOCK, EN_FUT, METALS, EQUITY, GREEN)
W <- weekly_returns(assets_all) %>% left_join(avg_price_changes("week") %>% rename(wk = per), by = "wk")
M <- D %>% mutate(mo = floor_date(Date, "month")) %>% group_by(mo) %>%
  summarise(across(all_of(assets_all), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
  left_join(avg_price_changes("month") %>% rename(mo = per), by = "mo") %>% filter(mo >= as.Date("2021-01-01"))

# ── Block regressions ────────────────────────────────────────────────────────
mk_ll <- function(X, LL = c(-1, 0, 1, 2)) as.data.frame(do.call(cbind, lapply(LL, function(k) {
  Z <- as.data.frame(lapply(X, lagv, k = k)); names(Z) <- paste0(names(X), "_L", ifelse(k < 0, paste0("m", -k), k)); Z })))
fit_blocks <- function(y, blocks, focus, others) {
  d <- na.omit(data.frame(y = zs(y), do.call(cbind, unname(blocks))))
  adj <- function(cols) summary(lm(reformulate(cols, "y"), data = d))$adj.r.squared
  bf <- lm(reformulate(unlist(lapply(blocks, names)), "y"), data = d)
  fcols <- names(blocks[[focus]]); ocols <- unlist(lapply(blocks[others], names))
  tibble(n = nrow(d),
         !!!setNames(lapply(names(blocks), function(b) adj(names(blocks[[b]]))), paste0("adjR2_", names(blocks))),
         adjR2_all = summary(bf)$adj.r.squared,
         incr_focus = summary(bf)$adj.r.squared - adj(ocols),
         p_focus = wald_hac(bf, fcols),
         incr_crypto = summary(bf)$adj.r.squared - adj(setdiff(unlist(lapply(blocks, names)), names(blocks$crypto))),
         p_crypto = wald_hac(bf, names(blocks$crypto)))
}
sc <- function(df, cols) as.data.frame(scale(df[, cols]))

rows_hr <- list(); rows_gold <- list()
for (tok in c("POWR", "EWT", "SNC", "WOZX")) {
  # daily, contemporaneous and lead-lag
  bl <- list(crypto = sc(D, CRYPTO_BLOCK), elec = sc(D, ELEC), enfut = sc(D, EN_FUT), metals = sc(D, METALS), equity = sc(D, EQUITY))
  rows_hr[[paste(tok, "daily")]] <- bind_cols(tibble(token = tok, spec = "daily"), fit_blocks(D[[tok]], bl, "elec", c("crypto", "enfut", "metals", "equity")))
  bl_ll <- lapply(bl, mk_ll)
  rows_hr[[paste(tok, "leadlag")]] <- bind_cols(tibble(token = tok, spec = "daily lead-lag"), fit_blocks(D[[tok]], bl_ll, "elec", c("crypto", "enfut", "metals", "equity")))
  # weekly
  blw <- list(crypto = sc(W, CRYPTO_BLOCK), elec = sc(W, ELEC), enfut = sc(W, EN_FUT), metals = sc(W, METALS), equity = sc(W, EQUITY))
  rows_hr[[paste(tok, "weekly")]] <- bind_cols(tibble(token = tok, spec = "weekly"), fit_blocks(W[[tok]], blw, "elec", c("crypto", "enfut", "metals", "equity")))
  # monthly: first principal components of each block (61 months)
  pc1 <- function(df, cols) { X <- scale(df[, cols]); s <- prcomp(X)$x[, 1]; if (cor(s, rowMeans(X)) < 0) s <- -s; s }
  blm <- list(crypto = data.frame(c1 = pc1(M, CRYPTO_BLOCK)), elec = data.frame(e1 = pc1(M, ELEC)),
              enfut = data.frame(f1 = pc1(M, EN_FUT)), metals = data.frame(m1 = pc1(M, METALS)), equity = data.frame(q1 = pc1(M, EQUITY)))
  rows_hr[[paste(tok, "monthly")]] <- bind_cols(tibble(token = tok, spec = "monthly (PC factors)"), fit_blocks(M[[tok]], blm, "elec", c("crypto", "enfut", "metals", "equity")))
}
hr <- bind_rows(rows_hr) %>% mutate(across(where(is.numeric), ~ round(., 4)))
write_csv(hr, "Output/jcm/horse_race_electricity.csv")

for (g in c("PAXG", "XAUT")) {
  others <- setdiff(c(EN_FUT, METALS), "GC")
  for (spec in c("daily", "daily lead-lag", "weekly")) {
    src <- if (spec == "weekly") W else D
    bl <- list(crypto = sc(src, CRYPTO_BLOCK), gold = sc(src, "GC"), othercom = sc(src, others))
    if (spec == "daily lead-lag") bl <- lapply(bl, mk_ll)
    rows_gold[[paste(g, spec)]] <- bind_cols(tibble(token = g, spec = spec), fit_blocks(src[[g]], bl, "gold", c("crypto", "othercom")))
  }
}
gold <- bind_rows(rows_gold) %>% mutate(across(where(is.numeric), ~ round(., 4)))
write_csv(gold, "Output/jcm/gold_tokens.csv")

# ── Julia inputs: token system with electricity, and gold-token systems ──────
ref <- c(colnames(read.csv("Data/Dataset.csv", nrows = 1, check.names = FALSE)))
order24 <- c("BTC","ETH","SOL","AVAX","DPI","IDEX","SPTOP20","EUA","NEE","ENPH","PBW","PBD","FENY","QCLN","ICLN","B","CL","HH","GC","HG","LIT","ESGD","ESGU","BGRN")
fill0 <- function(x) { x[!is.finite(x)] <- 0; x }
for (tok in c("POWR", "SNC", "EWT")) {
  X <- sapply(c(tok, order24, ELEC), function(a) fill0(D_raw[[a]]))
  write.table(X, sprintf("julia/Data/data_%s_elec.txt", c(POWR = "PWR", SNC = "SNC", EWT = "EWT")[tok]), sep = ",", row.names = FALSE, col.names = FALSE)
}
for (g in c("PAXG", "XAUT")) {
  X <- sapply(c(g, order24), function(a) fill0(D_raw[[a]]))
  write.table(X, sprintf("julia/Data/data_%s.txt", g), sep = ",", row.names = FALSE, col.names = FALSE)
}
# aligned inputs for the networks: the lagged series move down one row, first row 0
for (sys in c(PWR = "POWR", SNC = "SNC", EWT = "EWT", PAXG = "PAXG", XAUT = "XAUT")) {
  cols <- c(sys, order24, if (!sys %in% c("PAXG", "XAUT")) ELEC)
  X <- sapply(cols, function(a) fill0(if (a %in% LAGGED) c(0, head(D_raw[[a]], -1)) else D_raw[[a]]))
  tag <- names(which(c(PWR = "POWR", SNC = "SNC", EWT = "EWT", PAXG = "PAXG", XAUT = "XAUT") == sys))
  write.table(X, sprintf("julia/Data/data_%s%s_shift.txt", tag, if (sys %in% c("PAXG", "XAUT")) "" else "_elec"),
              sep = ",", row.names = FALSE, col.names = FALSE)
}

options(width = 220)
cat("\n==== Coverage ====\n"); print(sapply(c(ELEC, "PAXG", "XAUT"), function(z) sum(is.finite(D[[z]]))))
cat("\n==== HORSE RACE WITH ELECTRICITY (adjusted R2; incr_focus = electricity given all other blocks) ====\n")
print(hr %>% select(token, spec, n, adjR2_crypto, adjR2_elec, adjR2_enfut, adjR2_equity, incr_focus, p_focus, incr_crypto, p_crypto), n = 40)
cat("\n==== GOLD-BACKED TOKENS (incr_focus = gold given crypto and other commodities) ====\n")
print(gold %>% select(token, spec, n, adjR2_crypto, adjR2_gold, adjR2_othercom, incr_focus, p_focus, incr_crypto, p_crypto))
