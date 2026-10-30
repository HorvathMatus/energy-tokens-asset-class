# =============================================================================
# 19_jcm_tables.R   (Journal of Commodity Markets version)
#
#   (A) One identity table for the energy tokens and the gold-backed tokens:
#       adjusted R^2 of each block on its own (crypto, electricity, energy
#       futures, gold, clean-energy equities), and the increments of the crypto
#       block over all commodity blocks and of the commodity blocks over crypto,
#       with Newey-West Wald p-values. Weekly returns and daily lead-lag returns.
#   (B) Commodity-investor view: does adding an energy token to a long-only
#       global minimum-variance portfolio of commodity futures (Brent, WTI,
#       Henry Hub gas, EU carbon, gold, copper) improve its Sharpe ratio?
#
# Requires 18_electricity_gold.R (builds the electricity and gold-token series).
# Outputs: Output/jcm/identity_table.csv, commodity_portfolio.csv
# =============================================================================

rm(list = ls())
suppressMessages({ library(tidyverse); library(sandwich); library(quadprog) })
source_lines <- readLines("18_electricity_gold.R")
# re-use the data construction of 18 (everything before the block regressions)
eval(parse(text = source_lines[1:(grep("^# ── Block regressions", source_lines) - 1)]))

COMMOD_ALL <- c(ELEC, EN_FUT, METALS)
mk <- function(df, cols) { X <- as.data.frame(scale(as.data.frame(df)[, cols, drop = FALSE])); names(X) <- cols; X }
ll <- function(X, LL = c(-1, 0, 1, 2)) as.data.frame(do.call(cbind, lapply(LL, function(k) {
  Z <- as.data.frame(lapply(X, lagv, k = k)); names(Z) <- paste0(names(X), "_L", ifelse(k < 0, paste0("m", -k), k)); Z })))

identity_row <- function(y, src, leadlag) {
  B <- list(crypto = mk(src, CRYPTO_BLOCK), elec = mk(src, ELEC), enfut = mk(src, EN_FUT),
            gold = mk(src, "GC"), copper = mk(src, "HG"), equity = mk(src, EQUITY))
  if (leadlag) B <- lapply(B, ll)
  d <- na.omit(data.frame(y = zs(y), do.call(cbind, unname(B))))
  adj <- function(cols) summary(lm(reformulate(cols, "y"), data = d))$adj.r.squared
  cn <- lapply(B, names)
  com <- c(cn$elec, cn$enfut, cn$gold, cn$copper)
  full <- lm(reformulate(c(cn$crypto, com), "y"), data = d)
  a_full <- summary(full)$adj.r.squared
  tibble(n = nrow(d), crypto = adj(cn$crypto), electricity = adj(cn$elec), energy_futures = adj(cn$enfut),
         gold = adj(cn$gold), equity = adj(cn$equity),
         crypto_over_commodities = a_full - adj(com), p_crypto = wald_hac(full, cn$crypto),
         commodities_over_crypto = a_full - adj(cn$crypto), p_commodities = wald_hac(full, com))
}

# Moving-block bootstrap of the gain from adding the commodity blocks to the crypto
# block. Resampled blocks repeat observations, which inflates the in-sample fit of
# the larger model, so the bootstrap distribution is re-centred on the estimate
# (bounds = estimate +/- distance of the 5th and 95th percentiles from the median).
# The upper bound is a one-sided 95% bound for an equivalence test.
boot_incr <- function(y, src, leadlag, B = 999, block = if (leadlag) 20 else 8, seed = 2026) {
  B0 <- list(crypto = mk(src, CRYPTO_BLOCK), elec = mk(src, ELEC), enfut = mk(src, EN_FUT),
             gold = mk(src, "GC"), copper = mk(src, "HG"))
  if (leadlag) B0 <- lapply(B0, ll)
  d <- na.omit(data.frame(y = zs(y), do.call(cbind, unname(B0))))
  cc <- names(B0$crypto); kk <- setdiff(names(d), c("y", cc))
  Y <- d$y; Xc <- cbind(1, as.matrix(d[, cc])); Xa <- cbind(Xc, as.matrix(d[, kk])); n <- length(Y)
  adj <- function(X, yy) { r <- lm.fit(X, yy)$residuals; 1 - (sum(r^2) / (n - ncol(X))) / var(yy) }
  set.seed(seed); nb <- ceiling(n / block)
  draws <- replicate(B, { i <- as.vector(outer(0:(block - 1), sample.int(n - block + 1, nb, replace = TRUE), "+"))[1:n]
                          adj(Xa[i, ], Y[i]) - adj(Xc[i, ], Y[i]) })
  est <- adj(Xa, Y) - adj(Xc, Y); q <- quantile(draws, c(0.05, 0.5, 0.95))
  tibble(incr_lo90 = est - (q[[2]] - q[[1]]), incr_hi90 = est + (q[[3]] - q[[2]]))
}

rows <- list()
for (a in c("POWR", "EWT", "SNC", "WOZX", "PAXG", "XAUT")) {
  rows[[paste(a, "w")]]  <- bind_cols(tibble(asset = a, spec = "weekly"), identity_row(W[[a]], W, FALSE), boot_incr(W[[a]], W, FALSE))
  rows[[paste(a, "d")]]  <- bind_cols(tibble(asset = a, spec = "daily aligned"), identity_row(D[[a]], D, FALSE), boot_incr(D[[a]], D, FALSE, block = 20))
  rows[[paste(a, "ll")]] <- bind_cols(tibble(asset = a, spec = "daily lead-lag"), identity_row(D[[a]], D, TRUE), boot_incr(D[[a]], D, TRUE))
}
idt <- bind_rows(rows) %>% mutate(across(c(crypto:equity, crypto_over_commodities, commodities_over_crypto, incr_lo90, incr_hi90), ~ round(100 * ., 1)),
                                  across(starts_with("p_"), ~ round(., 3)))
write_csv(idt, "Output/jcm/identity_table.csv")

# ── (B) commodity-futures portfolio ─────────────────────────────────────────
COMMOD_PORT <- c("B", "CL", "HH", "EUA", "GC", "HG")
WINDOW <- 252; REBAL <- 21; FREQ <- 252
IMPL_LAG <- 1   # weights from returns up to t0-2, held from t0 (see 23_portfolio_timing.R)
make_pd <- function(S) { ev <- min(eigen(S, symmetric = TRUE, only.values = TRUE)$values); if (ev <= 0) S <- S + (-ev + 1e-7) * diag(nrow(S)); S }
gmv_w <- function(S) { n <- nrow(S)
  sol <- tryCatch(solve.QP(2 * S + diag(1e-8, n), rep(0, n), cbind(rep(1, n), diag(n)), c(1, rep(0, n)), meq = 1), error = function(e) NULL)
  if (is.null(sol)) return(rep(1 / n, n)); w <- pmax(sol$solution, 0); w / sum(w) }
roll <- function(R) {
  n <- nrow(R); port <- rep(NA_real_, n); wt <- c()
  for (t0 in seq(WINDOW + 1 + IMPL_LAG, n - 1, by = REBAL)) {
    Rw <- na.omit(R[(t0 - WINDOW - IMPL_LAG):(t0 - 1 - IMPL_LAG), , drop = FALSE]); w <- gmv_w(make_pd(cov(Rw)))
    tn <- min(t0 + REBAL - 1, n); Rn <- R[t0:tn, , drop = FALSE]
    port[t0:tn] <- as.numeric(ifelse(is.na(Rn), 0, Rn) %*% w); wt <- c(wt, w[1])
  }
  list(r = port, w_first = wt)
}
SR <- function(x) { x <- na.omit(x); mean(x) / sd(x) * sqrt(FREQ) }
lw_test <- function(ra, rb) { ok <- complete.cases(ra, rb); x <- ra[ok]; y <- rb[ok]; n <- length(x)
  m1 <- mean(x); m2 <- mean(y); g1 <- mean(x^2); g2 <- mean(y^2); s1 <- sqrt(g1 - m1^2); s2 <- sqrt(g2 - m2^2)
  d <- m1 / s1 - m2 / s2; grad <- c(g1 / s1^3, -g2 / s2^3, -m1 / (2 * s1^3), m2 / (2 * s2^3))
  Y <- cbind(x - m1, y - m2, x^2 - g1, y^2 - g2); L <- floor(4 * (n / 100)^(2 / 9)); Psi <- crossprod(Y) / n
  for (l in seq_len(L)) { G <- crossprod(Y[(l + 1):n, , drop = FALSE], Y[1:(n - l), , drop = FALSE]) / n; Psi <- Psi + (1 - l / (L + 1)) * (G + t(G)) }
  2 * pnorm(-abs(d / sqrt(as.numeric(t(grad) %*% Psi %*% grad) / n))) }

base <- roll(as.matrix(R30[, COMMOD_PORT]))
prow <- list()
for (a in c("POWR", "EWT", "SNC", "WOZX")) {
  aug <- roll(as.matrix(R30[, c(a, COMMOD_PORT)]))
  prow[[a]] <- tibble(token = a, SR_base = SR(base$r), SR_aug = SR(aug$r), dSR = SR(aug$r) - SR(base$r),
                      p_LW = lw_test(aug$r, base$r), mean_token_weight_pct = 100 * mean(aug$w_first))
}
cport <- bind_rows(prow) %>% mutate(across(where(is.numeric), ~ round(., 3)))
write_csv(cport, "Output/jcm/commodity_portfolio.csv")

options(width = 200)
print(idt, n = 20, width = 200)
print(cport)
