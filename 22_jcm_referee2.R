# =============================================================================
# 22_jcm_referee2.R   (JCM, second round of referee checks)
#
#   (A) Timing audit: cross-correlations of daily returns between series from
#       different sources at offsets of -1 to +2 business days. A peak at +1
#       means the exchange-traded series must be lagged one day to cover the
#       token's UTC day. Also checks the CoinGecko price file (snapshot dated t)
#       against the CoinGecko volume file (event date t-1).
#   (B) Tail and nonlinear dependence on aligned data, with the electricity
#       block added (lower-tail dependence, exceedance correlation, Delta-CoVaR,
#       distance correlation).
#   (C) Frequency bands in the 29-asset networks: weight of each band in the
#       token's forecast-error variance and source shares within each band.
#   (D) Net transmission with credible bands: share of days on which the 95%
#       band of the token's short-run net connectedness lies above or below 0.
#   (E) Fund size at which the largest rebalancing trade reaches 10% of the
#       token's daily volume (participation scales linearly with fund size).
#   (F) Forward-filled days of WPR and SLR on the seven-day calendar.
#
# Requires 18_electricity_gold.R, 16_portfolio_frictions.R, julia/network_jcm.jl.
# Outputs in Output/jcm/
# =============================================================================

rm(list = ls())
suppressMessages({ library(tidyverse); library(quantreg) })
src18 <- readLines("18_electricity_gold.R")
eval(parse(text = src18[1:(grep("^# ── Aggregations", src18) - 1)]))   # D (aligned), D_raw, E
lagv <- function(x, k) if (k >= 0) c(rep(NA, k), head(x, length(x) - k)) else c(tail(x, length(x) + k), rep(NA, -k))

# ── (A) timing audit ────────────────────────────────────────────────────────
pairs <- list(c("BTC", "SPTOP20"), c("BTC", "ESGU"), c("POWR", "SPTOP20"), c("POWR", "QCLN"),
              c("PAXG", "GC"), c("XAUT", "GC"), c("BTC", "ETH"), c("PAXG", "XAUT"), c("ESGU", "QCLN"), c("B", "CL"))
xc <- bind_rows(lapply(pairs, function(p) {
  y <- D_raw[[p[1]]]; x <- D_raw[[p[2]]]
  tibble(y = p[1], x = p[2], !!!setNames(lapply(-1:2, function(k) cor(y, lagv(x, k), use = "complete.obs")),
                                          paste0("k", -1:2)))
}))
cg_check <- bind_rows(lapply(c("BTC", "POWR", "SNC", "EWT"), function(tk) {
  a <- read_csv(sprintf("Raw_Data/%s.csv", tk), show_col_types = FALSE) %>%
    transmute(Date = as.Date(substr(Date, 1, 10)), p = .data[[tk]]) %>% arrange(Date) %>% mutate(r = c(NA, diff(log(p))))
  b <- read_csv(sprintf("Raw_Data/CoinGecko_volume/%s.csv", tk), show_col_types = FALSE) %>%
    transmute(Date = as.Date(substr(event_date, 1, 10)), c = close_price_usd) %>% arrange(Date) %>% mutate(rc = c(NA, diff(log(c))))
  tibble(y = paste(tk, "price file"), x = paste(tk, "volume file"),
         !!!setNames(lapply(-1:2, function(k) {
           m <- inner_join(a, b %>% mutate(Date = Date + k), by = "Date") %>% filter(Date >= START, Date <= END)
           cor(m$r, m$rc, use = "complete.obs") }), paste0("k", -1:2)))
}))
xc <- bind_rows(xc, cg_check)
write_csv(xc %>% mutate(across(where(is.numeric), ~ round(., 3))), "Output/jcm/timing_xcorr.csv")

# ── (B) tail and nonlinear dependence, aligned data ─────────────────────────
ENERGY14 <- c(COMMOD, ENERGY_EQ)
blockF <- function(cols) rowMeans(scale(as.matrix(D[, cols])), na.rm = TRUE)
F <- list(crypto = blockF(CRYPTO_BLOCK), energy = blockF(ENERGY14), electricity = blockF(ELEC))
ltd <- function(x, y, u = 0.05, upper = FALSE) {
  ok <- complete.cases(x, y); x <- x[ok]; y <- y[ok]
  if (upper) { x <- -x; y <- -y }
  qx <- quantile(x, u); qy <- quantile(y, u); sum(x <= qx & y <= qy) / sum(y <= qy)
}
exc_cor <- function(x, y, u = 0.10) { ok <- complete.cases(x, y); x <- x[ok]; y <- y[ok]; s <- y <= quantile(y, u); cor(x[s], y[s]) }
dcovar <- function(x, f, tau = 0.05) {
  d <- na.omit(data.frame(y = x, f = f)); b <- coef(rq(y ~ f, tau = tau, data = d))[2]
  unname(b * (quantile(d$f, tau) - median(d$f)))
}
dcor <- function(x, y) {
  ok <- complete.cases(x, y); x <- x[ok]; y <- y[ok]
  A <- as.matrix(dist(x)); B <- as.matrix(dist(y))
  A <- A - rowMeans(A) - rep(colMeans(A), each = nrow(A)) + mean(A)
  B <- B - rowMeans(B) - rep(colMeans(B), each = nrow(B)) + mean(B)
  sqrt(mean(A * B) / sqrt(mean(A * A) * mean(B * B)))
}
tail_tab <- bind_rows(lapply(c(TOKENS_ALL, "PAXG", "XAUT"), function(tk) {
  x <- D[[tk]]
  bind_cols(tibble(token = tk),
            as_tibble(setNames(lapply(names(F), function(b) ltd(x, F[[b]])), paste0("ltd_", names(F)))),
            as_tibble(setNames(lapply(names(F), function(b) ltd(x, F[[b]], upper = TRUE)), paste0("utd_", names(F)))),
            as_tibble(setNames(lapply(names(F), function(b) exc_cor(x, F[[b]])), paste0("exc_", names(F)))),
            as_tibble(setNames(lapply(names(F), function(b) dcovar(x, F[[b]])), paste0("dcovar_", names(F)))),
            as_tibble(setNames(lapply(names(F), function(b) dcor(x, F[[b]])), paste0("dcor_", names(F)))),
            tibble(ltd_gold = ltd(x, D$GC), dcor_gold = dcor(x, D$GC)))
}))
write_csv(tail_tab %>% mutate(across(where(is.numeric), ~ round(., 3))), "Output/jcm/tail_aligned.csv")

# ── (C) and (D) frequency bands and credible net transmission ───────────────
ORDER24 <- c("BTC", "ETH", "SOL", "AVAX", "DPI", "IDEX", "SPTOP20", "EUA", "NEE", "ENPH", "PBW", "PBD",
             "FENY", "QCLN", "ICLN", "B", "CL", "HH", "GC", "HG", "LIT", "ESGD", "ESGU", "BGRN")
TOK <- c(PWR = "POWR", SNC = "SNC", EWT = "EWT")
rd <- function(f) t(read.csv(f, header = FALSE))[, -1] |> apply(2, as.numeric)
band_rows <- list(); net_rows <- list()
for (sys in names(TOK)) {
  nm <- c(TOK[[sys]], ORDER24, ELEC); N <- length(nm)
  tabs <- lapply(c(total = "total", long = "long", medium = "medium", short = "short"), function(b)
    as.matrix(read.csv(sprintf("julia/Output/avg_tab_%s_%s_elecs.csv", b, sys), header = TRUE)))
  tot <- sum(tabs$total[1, ])
  for (b in names(tabs)) {
    r <- tabs[[b]][1, ]; s <- r / sum(r); names(s) <- nm
    band_rows[[paste(sys, b)]] <- tibble(token = TOK[[sys]], band = b, band_weight = 100 * sum(r) / tot,
      own = 100 * s[1], crypto = 100 * sum(s[c(CRYPTO, CRYPTO_IX)]), electricity = 100 * sum(s[ELEC]),
      energy_futures = 100 * sum(s[EN_FUT]), gold = 100 * s["GC"], copper = 100 * s["HG"],
      equity = 100 * sum(s[EQUITY]), green = 100 * sum(s[ESG]),
      crypto_per_asset = 100 * mean(s[c(CRYPTO, CRYPTO_IX)]), electricity_per_asset = 100 * mean(s[ELEC]),
      energy_futures_per_asset = 100 * mean(s[EN_FUT]))
  }
  C <- rd(sprintf("julia/Output/Chorizon_%s_elecs.csv", sys))
  L <- rd(sprintf("julia/Output/CIhorizon1_%s_elecs.csv", sys)); U <- rd(sprintf("julia/Output/CIhorizon2_%s_elecs.csv", sys))
  for (b in c(long = 16, medium = 17, short = 18)) {
    k <- 8 + b * N; bn <- names(which(c(long = 16, medium = 17, short = 18) == b))
    net_rows[[paste(sys, bn)]] <- tibble(token = TOK[[sys]], band = bn, days = sum(is.finite(C[, k])),
      share_point_pos = mean(C[, k] > 0, na.rm = TRUE), share_ci_above0 = mean(L[, k] > 0, na.rm = TRUE),
      share_ci_below0 = mean(U[, k] < 0, na.rm = TRUE))
  }
}
bands <- bind_rows(band_rows); nets <- bind_rows(net_rows)
write_csv(bands %>% mutate(across(where(is.numeric), ~ round(., 2))), "Output/jcm/network_bands.csv")
write_csv(nets %>% mutate(across(where(is.numeric), ~ round(., 3))), "Output/jcm/net_credible.csv")

# ── (E) participation-constrained fund size ─────────────────────────────────
fr <- read_csv("Output/portfolio/frictions.csv", show_col_types = FALSE) %>%
  transmute(token, capacity_avg_position_musd = capacity_aum_10pct_adv_musd,
            max_trade_pct_at_10m = participation_10m_max_pct, median_trade_pct_at_10m = participation_10m_median_pct,
            aum_max_trade_10pct_musd = 10 * 10 / participation_10m_max_pct)
write_csv(fr, "Output/jcm/participation.csv")

# ── (F) forward-filled days on the seven-day calendar ───────────────────────
ff <- bind_rows(lapply(c("WPR", "SLR"), function(tk) {
  p <- read_csv(sprintf("Raw_Data/%s.csv", tk), show_col_types = FALSE) %>%
    transmute(Date = as.Date(substr(Date, 1, 10)), p = .data[[tk]])
  cal <- tibble(Date = seq(START, END, by = "day")) %>% left_join(p, by = "Date")
  tibble(token = tk, days = nrow(cal), missing = sum(is.na(cal$p)), share_missing = mean(is.na(cal$p)))
}))
write_csv(ff, "Output/jcm/token_gaps.csv")

options(width = 220)
print(xc %>% mutate(across(where(is.numeric), ~ round(., 3))))
print(as.data.frame(tail_tab %>% mutate(across(where(is.numeric), ~ round(., 3)))))
print(as.data.frame(bands %>% mutate(across(where(is.numeric), ~ round(., 2)))))
print(nets); print(fr); print(ff)
