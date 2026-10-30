# =============================================================================
# 15_liquidity_identity.R   (IRFA revision, editor point 1 + formal identity tests)
#
#   (L) Liquidity diagnostics per token (and major cryptocurrencies as a yardstick)
#       from CoinGecko daily volume and market capitalisation: median volume,
#       low-volume days, Amihud (2002) illiquidity, zero-return share, and the
#       Roll (1984) effective-spread proxy.
#   (T) Thin-trading robustness of the asset-class identity result:
#       contemporaneous vs Dimson (1979) lead-lag regressions, weekly returns,
#       high-volume days only, and winsorised token returns.
#   (H) Formal horse race: incremental R^2 and HAC Wald tests of each block
#       given the other.
#   (S) Mean-variance spanning tests (Huberman-Kandel 1987, Kan-Zhou 2012
#       step-down) against the non-token benchmark, the crypto block, and the
#       energy and green block, with HAC-robust Wald statistics.
#
# Outputs: Output/liquidity/liquidity_table.csv, liquidity_monthly.csv
#          Output/identity/thin_trading.csv, horse_race.csv, spanning.csv
# =============================================================================

rm(list = ls())
suppressMessages({ library(tidyverse); library(sandwich) })

load("Data/returns_full30_aligned.RData")   # R30 with exchange-traded series aligned (09b), blocks
dir.create("Output/liquidity", showWarnings = FALSE, recursive = TRUE)
dir.create("Output/identity",  showWarnings = FALSE, recursive = TRUE)

START <- as.Date("2020-12-07"); END <- as.Date("2026-01-31")
CG_DIR <- "Raw_Data/CoinGecko_volume"
CG_ASSETS <- c("POWR","EWT","WOZX","SNC","WPR","BTC","ETH","SOL","AVAX")

CRYPTO_BLOCK <- c(CRYPTO, CRYPTO_IX)            # 7
ENERGY_BLOCK <- c(COMMOD, ENERGY_EQ)            # 14
GREEN_BLOCK  <- ESG                             # 3
NONTOKEN     <- c(CRYPTO_BLOCK, ENERGY_BLOCK, GREEN_BLOCK)

nw_lag <- function(n) floor(4 * (n / 100)^(2 / 9))

# ── CoinGecko loader (7-day calendar) ────────────────────────────────────────
read_cg <- function(tk) {
  read_csv(file.path(CG_DIR, paste0(tk, ".csv")), show_col_types = FALSE) %>%
    transmute(Date = as.Date(substr(event_date, 1, 10)),
              price = close_price_usd, mcap = market_cap_usd, vol = volume_usd) %>%
    distinct(Date, .keep_all = TRUE) %>% arrange(Date) %>%
    mutate(r = c(NA, diff(log(price))), rs = exp(r) - 1) %>%
    filter(Date >= START, Date <= END)
}
CG <- setNames(lapply(CG_ASSETS, read_cg), CG_ASSETS)

# Roll (1984): 2*sqrt(-cov(r_t, r_{t-1})), positive covariances set to zero
roll_spread <- function(x) {
  x <- na.omit(x); if (length(x) < 15) return(NA_real_)
  c1 <- cov(x[-1], x[-length(x)]); if (c1 >= 0) 0 else 2 * sqrt(-c1)
}

# ── (L) Liquidity table ──────────────────────────────────────────────────────
monthly <- bind_rows(lapply(CG_ASSETS, function(tk) {
  CG[[tk]] %>% mutate(month = floor_date(Date, "month")) %>%
    group_by(month) %>%
    summarise(asset = tk, med_vol = median(vol, na.rm = TRUE),
              roll = roll_spread(rs), sd_d = sd(rs, na.rm = TRUE),
              .groups = "drop")
}))
write_csv(monthly, "Output/liquidity/liquidity_monthly.csv")

liq <- bind_rows(lapply(CG_ASSETS, function(tk) {
  d <- CG[[tk]] %>% filter(!is.na(r))
  m <- monthly %>% filter(asset == tk)
  tibble(
    asset        = tk,
    n_days       = nrow(d),
    med_vol_usd  = median(d$vol, na.rm = TRUE),
    p10_vol_usd  = quantile(d$vol, 0.10, na.rm = TRUE),
    share_vol_lt50k = mean(d$vol < 5e4, na.rm = TRUE),
    med_mcap_usd = median(d$mcap[d$mcap > 0], na.rm = TRUE),
    # Amihud: % price move per USD 1 million traded (days with positive volume)
    amihud_pct_per_1m = mean(abs(d$rs[d$vol > 0]) / d$vol[d$vol > 0], na.rm = TRUE) * 1e6 * 100,
    zero_ret_share = mean(d$r == 0, na.rm = TRUE),
    roll_bps     = mean(m$roll, na.rm = TRUE) * 1e4,
    daily_vol_pct = sd(d$rs, na.rm = TRUE) * 100
  )
}))
write_csv(liq, "Output/liquidity/liquidity_table.csv")

# ── Business-day volume aligned to R30 (sum over calendar days each return spans)
# The return dated t runs from the 00:00 UTC snapshot dated at the previous
# business day to the one dated t, so it spans the UTC days prev, ..., t-1,
# which carry the event dates of the CoinGecko volume file.
bd_volume <- function(tk) {
  d <- CG[[tk]] %>% select(Date, vol)
  bd <- R30$Date
  prev <- c(bd[1] - 1, bd[-length(bd)])
  sapply(seq_along(bd), function(i) sum(d$vol[d$Date >= prev[i] & d$Date < bd[i]], na.rm = TRUE))
}

# ── Helpers for the identity regressions ─────────────────────────────────────
zs <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
pc1_series <- function(M) {
  ok <- complete.cases(M); X <- scale(M[ok, , drop = FALSE])
  sc <- as.numeric(X %*% prcomp(X, center = FALSE)$rotation[, 1])
  if (cor(sc, rowMeans(X)) < 0) sc <- -sc
  out <- rep(NA_real_, nrow(M)); out[ok] <- as.numeric(scale(sc)); out
}
lagv <- function(x, k) {
  if (k >= 0) c(rep(NA, k), head(x, length(x) - k)) else c(tail(x, length(x) + k), rep(NA, -k))
}

# HAC Wald test of the linear restriction R b = q
wald_hac <- function(fit, R, q = rep(0, nrow(R))) {
  V <- NeweyWest(fit, lag = nw_lag(nobs(fit)), prewhite = FALSE)
  b <- coef(fit); d <- R %*% b - q
  W <- as.numeric(t(d) %*% solve(R %*% V %*% t(R)) %*% d)
  c(W = W, df = nrow(R), p = pchisq(W, nrow(R), lower.tail = FALSE))
}

cryptoF <- pc1_series(as.matrix(R30[, CRYPTO_BLOCK]))
energyF <- pc1_series(as.matrix(R30[, ENERGY_BLOCK]))

TOKENS_LIQ <- c("POWR","EWT","WOZX","SNC","WPR")      # SLR has no usable volume history

# Liquidity screen over the first twelve months and over the full sample
screen <- bind_rows(lapply(TOKENS_LIQ, function(tk) {
  d <- CG[[tk]]
  f <- d %>% filter(Date < START + 365)
  tibble(asset = tk,
         med_vol_first_year = median(f$vol, na.rm = TRUE),
         share_lt50k_first_year = mean(f$vol < 5e4, na.rm = TRUE),
         med_vol_full = median(d$vol, na.rm = TRUE),
         share_lt50k_full = mean(d$vol < 5e4, na.rm = TRUE),
         share_lt50k_2024_25 = mean(d$vol[d$Date >= as.Date("2024-01-01")] < 5e4, na.rm = TRUE))
}))
write_csv(screen, "Output/liquidity/liquidity_screen.csv")

# Block regressions for one specification: R^2 of each block, incremental
# adjusted R^2 of each block over the other, and HAC Wald tests of each block
spec_eval <- function(y, Xc, Xe) {
  d <- na.omit(data.frame(y = y, Xc, Xe)); n <- nrow(d)
  if (n < 60) return(NULL)
  cc <- names(Xc); ee <- names(Xe)
  sc <- summary(lm(y ~ ., data = d[, c("y", cc)]))
  se <- summary(lm(y ~ ., data = d[, c("y", ee)]))
  fb <- lm(y ~ ., data = d); sb <- summary(fb); nm <- names(coef(fb))
  Rsel <- function(cols) {
    R <- matrix(0, length(cols), length(nm)); R[cbind(seq_along(cols), match(cols, nm))] <- 1; R
  }
  wE <- wald_hac(fb, Rsel(ee)); wC <- wald_hac(fb, Rsel(cc))
  tibble(n = n, R2_crypto = sc$r.squared, adjR2_crypto = sc$adj.r.squared,
         R2_energy = se$r.squared, adjR2_energy = se$adj.r.squared,
         adjR2_both = sb$adj.r.squared,
         incr_adj_energy = sb$adj.r.squared - sc$adj.r.squared,
         incr_adj_crypto = sb$adj.r.squared - se$adj.r.squared,
         p_energy_given_crypto = wE[["p"]], p_crypto_given_energy = wC[["p"]])
}

thin_rows <- list(); hr_rows <- list(); sp_rows <- list()

for (tok in TOKENS_ALL) {
  y  <- R30[[tok]]
  Xc <- as.data.frame(scale(R30[, CRYPTO_BLOCK])); Xe <- as.data.frame(scale(R30[, ENERGY_BLOCK]))

  add <- function(spec, res, extra = list()) {
    if (is.null(res)) return(invisible())
    row <- bind_cols(tibble(token = tok, spec = spec), res)
    if (length(extra)) row <- bind_cols(row, as_tibble(extra))
    thin_rows[[paste(tok, spec)]] <<- row
  }

  # (T1) contemporaneous baseline
  add("contemporaneous", spec_eval(zs(y), Xc, Xe))

  # (T2) Dimson lead-lag: blocks with leads and lags (-1, 0, +1, +2 days)
  LL <- c(-1, 0, 1, 2)
  mk <- function(X) as.data.frame(do.call(cbind, lapply(LL, function(k) {
    Z <- as.data.frame(lapply(X, lagv, k = k))
    names(Z) <- paste0(names(X), "_L", ifelse(k < 0, paste0("m", -k), k)); Z })))
  dres <- spec_eval(zs(y), mk(Xc), mk(Xe))
  # summed Dimson betas on the two PC factors, with HAC t-stat on each sum
  Cl <- sapply(LL, function(k) lagv(cryptoF, k)); colnames(Cl) <- paste0("c", seq_along(LL))
  El <- sapply(LL, function(k) lagv(energyF, k)); colnames(El) <- paste0("e", seq_along(LL))
  D  <- data.frame(y = zs(y), Cl, El)
  fD <- lm(y ~ ., data = na.omit(D)); VD <- NeweyWest(fD, lag = nw_lag(nobs(fD)), prewhite = FALSE)
  ic <- colnames(Cl); ie <- colnames(El)
  sc <- sum(coef(fD)[ic]); se <- sum(coef(fD)[ie])
  tc <- sc / sqrt(sum(VD[ic, ic])); te <- se / sqrt(sum(VD[ie, ie]))
  f0 <- lm(y ~ cf + ef, data = na.omit(data.frame(y = zs(y), cf = cryptoF, ef = energyF)))
  add("dimson_leadlag", dres, list(beta_crypto_sum = sc, t_crypto_sum = tc,
                                   beta_energy_sum = se, t_energy_sum = te,
                                   beta_crypto_contemp = coef(f0)[["cf"]],
                                   beta_energy_contemp = coef(f0)[["ef"]]))

  # (T3) weekly returns (sum of daily log returns within each Monday-based week)
  W <- R30 %>% mutate(wk = floor_date(Date, "week", week_start = 1)) %>%
    group_by(wk) %>%
    summarise(across(all_of(c(tok, CRYPTO_BLOCK, ENERGY_BLOCK)),
                     ~ if (all(is.na(.x))) NA_real_ else sum(.x, na.rm = TRUE)),
              n_d = n(), .groups = "drop") %>%
    filter(n_d >= 3)
  add("weekly", spec_eval(zs(W[[tok]]), as.data.frame(scale(W[, CRYPTO_BLOCK])),
                          as.data.frame(scale(W[, ENERGY_BLOCK]))))

  # (T4) winsorised token return (1% / 99%)
  q <- quantile(y, c(.01, .99), na.rm = TRUE); yw <- pmin(pmax(y, q[1]), q[2])
  add("winsorised_1pct", spec_eval(zs(yw), Xc, Xe))

  # (T5) high-volume days only (token volume at or above its median)
  if (tok %in% TOKENS_LIQ) {
    v <- bd_volume(tok); keep <- v >= median(v[v > 0], na.rm = TRUE)
    yk <- ifelse(keep, y, NA)
    add("high_volume_days", spec_eval(zs(yk), Xc, Xe))
  }

  # (H) horse race: incremental R^2 and HAC Wald tests
  d <- na.omit(data.frame(y = zs(y), Xc, Xe))
  fit <- lm(y ~ ., data = d); nm <- names(coef(fit))
  Rsel <- function(cols) {
    R <- matrix(0, length(cols), length(nm)); R[cbind(seq_along(cols), match(cols, nm))] <- 1; R
  }
  wE <- wald_hac(fit, Rsel(ENERGY_BLOCK)); wC <- wald_hac(fit, Rsel(CRYPTO_BLOCK))
  r2c  <- summary(lm(y ~ ., data = d[, c("y", CRYPTO_BLOCK)]))$r.squared
  r2e  <- summary(lm(y ~ ., data = d[, c("y", ENERGY_BLOCK)]))$r.squared
  r2ce <- summary(fit)$r.squared
  hr_rows[[tok]] <- tibble(token = tok, n = nrow(d), R2_crypto = r2c, R2_energy = r2e,
    R2_both = r2ce, incr_energy_given_crypto = r2ce - r2c, incr_crypto_given_energy = r2ce - r2e,
    W_energy = wE[["W"]], p_energy = wE[["p"]], W_crypto = wC[["W"]], p_crypto = wC[["p"]])

  # (S) spanning tests on simple returns: r_tok = a + B r_bench + e
  #     H0: a = 0 and sum(B) = 1 (joint); step-down F1: a = 0, F2: sum(B) = 1 | a = 0
  for (bn in c("all_nontoken", "crypto_block", "energy_green_block")) {
    bench <- switch(bn, all_nontoken = NONTOKEN, crypto_block = CRYPTO_BLOCK,
                    energy_green_block = c(ENERGY_BLOCK, GREEN_BLOCK))
    S <- na.omit(data.frame(y = exp(y) - 1, exp(R30[, bench]) - 1))
    fs <- lm(y ~ ., data = S); k <- length(coef(fs))
    Ra <- matrix(c(1, rep(0, k - 1)), 1)             # alpha = 0
    Rb <- matrix(c(0, rep(1, k - 1)), 1)             # sum(beta) = 1
    joint <- wald_hac(fs, rbind(Ra, Rb), c(0, 1))
    f1 <- wald_hac(fs, Ra, 0)
    fs0 <- lm(y ~ . - 1, data = S)                   # F2 imposes alpha = 0
    f2 <- wald_hac(fs0, matrix(1, 1, k - 1), 1)
    sp_rows[[paste(tok, bn)]] <- tibble(token = tok, benchmark = bn, n = nrow(S), K = k - 1,
      alpha_ann_pct = coef(fs)[1] * 252 * 100, sum_beta = sum(coef(fs)[-1]),
      W_joint = joint[["W"]], p_joint = joint[["p"]],
      W_F1_alpha = f1[["W"]], p_F1_alpha = f1[["p"]],
      W_F2_beta = f2[["W"]], p_F2_beta = f2[["p"]])
  }
}

thin <- bind_rows(thin_rows) %>% mutate(across(where(is.numeric), ~ round(., 4)))
hr   <- bind_rows(hr_rows)   %>% mutate(across(where(is.numeric), ~ round(., 4)))
span <- bind_rows(sp_rows)   %>% mutate(across(where(is.numeric), ~ round(., 4)))
write_csv(thin, "Output/identity/thin_trading.csv")
write_csv(hr,   "Output/identity/horse_race.csv")
write_csv(span, "Output/identity/spanning.csv")

options(width = 200)
cat("\n==== (L) LIQUIDITY ====\n")
print(liq %>% mutate(across(where(is.numeric), ~ signif(., 4))), width = 200)
cat("\n==== (L) LIQUIDITY SCREEN ====\n")
print(screen %>% mutate(across(where(is.numeric), ~ signif(., 4))), width = 200)
cat("\n==== (T) THIN TRADING ====\n")
print(thin %>% select(token, spec, n, R2_crypto, adjR2_crypto, R2_energy, adjR2_energy,
                      incr_adj_energy, p_energy_given_crypto, incr_adj_crypto,
                      p_crypto_given_energy), n = 60, width = 200)
print(thin %>% filter(spec == "dimson_leadlag") %>%
        select(token, beta_crypto_contemp, beta_crypto_sum, t_crypto_sum,
               beta_energy_contemp, beta_energy_sum, t_energy_sum), width = 200)
cat("\n==== (H) HORSE RACE ====\n");   print(hr, width = 200)
cat("\n==== (S) SPANNING ====\n");     print(span, n = 30, width = 200)
