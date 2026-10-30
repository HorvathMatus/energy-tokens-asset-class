# =============================================================================
# 24_bootstrap_coverage.R   (JCM, referee check on the bounds of Table 3)
#
# How well do the one-sided 95% upper bounds on the commodity gain
# Delta adjR2(K|C) cover the true gain? Three constructions are compared:
#   (P) percentile:   upper = q95 of the moving-block bootstrap distribution
#   (R) re-centred:   upper = estimate + (q95 - q50)   [used in Table 3]
#   (N) noncentrality: the HAC Wald statistic W of the commodity coefficients is
#       treated as noncentral chi2(q, lambda). The upper 95% bound lambda_U solves
#       P(chi2(q, lambda_U) <= W) = 0.05 and is mapped into the gain as
#       lambda_U / n * (1 - R2_full) (Steiger and Fouladi, 1997).
# Design: the regressors of POWR are kept fixed, y* = Xc bc + Xk (g bk) + e*, with
# bc and bk the full-sample estimates, e* moving-block resamples of the full-model
# residuals, and g chosen so that the population gain Var(Mc Xk g bk) / Var(y*)
# equals 0, 1, 2.5, or 5 percentage points. Coverage is the share of replications
# in which the upper bound is at or above the true gain.
#
# Output: Output/jcm/bootstrap_coverage.csv, bootstrap_bounds.csv
# =============================================================================

rm(list = ls())
suppressMessages({ library(tidyverse); library(sandwich); library(parallel) })
src18 <- readLines("18_electricity_gold.R")
eval(parse(text = src18[1:(grep("^# ── Block regressions", src18) - 1)]))
mk <- function(df, cols) { X <- as.data.frame(scale(as.data.frame(df)[, cols, drop = FALSE])); names(X) <- cols; X }
ll <- function(X, LL = c(-1, 0, 1, 2)) as.data.frame(do.call(cbind, lapply(LL, function(k) {
  Z <- as.data.frame(lapply(X, lagv, k = k)); names(Z) <- paste0(names(X), "_L", ifelse(k < 0, paste0("m", -k), k)); Z })))

design <- function(y, src, leadlag) {
  B0 <- list(crypto = mk(src, CRYPTO_BLOCK), elec = mk(src, ELEC), enfut = mk(src, EN_FUT),
             gold = mk(src, "GC"), copper = mk(src, "HG"))
  if (leadlag) B0 <- lapply(B0, ll)
  # the equity block enters only the sample definition, so that the sample matches Table 3 (19_jcm_tables.R)
  E0 <- mk(src, EQUITY); if (leadlag) E0 <- ll(E0)
  d <- na.omit(data.frame(y = zs(y), do.call(cbind, unname(B0)), E0))
  cc <- names(B0$crypto); kk <- setdiff(names(d), c("y", cc, names(E0)))
  list(y = d$y, Xc = cbind(1, as.matrix(d[, cc])), Xk = as.matrix(d[, kk]))
}
adjR2 <- function(X, y) { r <- lm.fit(X, y)$residuals; n <- length(y); 1 - (sum(r^2) / (n - ncol(X))) / var(y) }
blocks_idx <- function(n, L) { nb <- ceiling(n / L); as.vector(outer(0:(L - 1), sample.int(n - L + 1, nb, replace = TRUE), "+"))[1:n] }

bounds <- function(y, Xc, Xk, L, B) {
  Xa <- cbind(Xc, Xk); n <- length(y)
  est <- adjR2(Xa, y) - adjR2(Xc, y)
  dr <- replicate(B, { i <- blocks_idx(n, L); adjR2(Xa[i, ], y[i]) - adjR2(Xc[i, ], y[i]) })
  q <- quantile(dr, c(0.05, 0.5, 0.95), names = FALSE)
  fit <- lm(y ~ Xa - 1); kcols <- (ncol(Xc) + 1):ncol(Xa)
  V <- NeweyWest(fit, lag = nw_lag(n), prewhite = FALSE)[kcols, kcols]; b <- coef(fit)[kcols]
  W <- as.numeric(t(b) %*% solve(V, b)); q_df <- length(kcols)
  lamU <- if (pchisq(W, q_df) <= 0.05) 0 else uniroot(function(l) pchisq(W, q_df, ncp = l) - 0.05, c(0, 10 * W + 100))$root
  r2f <- 1 - sum(fit$residuals^2) / sum((y - mean(y))^2)
  c(est = est, q05 = q[1], q50 = q[2], q95 = q[3], up_pct = q[3], up_rec = est + q[3] - q[2],
    lo_rec = est - (q[2] - q[1]), up_ncp = lamU / n * (1 - r2f), W = W)
}

# ── (1) bounds for all tokens and specifications, with the three constructions
specs <- list(weekly = list(src = W, ll = FALSE, L = 8), `daily aligned` = list(src = D, ll = FALSE, L = 20),
              `daily lead-lag` = list(src = D, ll = TRUE, L = 20))
set.seed(2026)
bb <- bind_rows(lapply(c("POWR", "EWT", "SNC", "WOZX", "PAXG", "XAUT"), function(a) bind_rows(lapply(names(specs), function(s) {
  sp <- specs[[s]]; d <- design(sp$src[[a]], sp$src, sp$ll)
  as_tibble(as.list(100 * bounds(d$y, d$Xc, d$Xk, sp$L, 999)[-9])) %>% mutate(asset = a, spec = s, .before = 1)
}))))
write_csv(bb %>% mutate(across(where(is.numeric), ~ round(., 2))), "Output/jcm/bootstrap_bounds.csv")
print(as.data.frame(bb %>% mutate(across(where(is.numeric), ~ round(., 2)))))

# ── (2) Monte Carlo coverage on the POWR design
NREP <- as.integer(Sys.getenv("NREP", "300")); BMC <- as.integer(Sys.getenv("BMC", "299"))
TARGETS <- c(0, 0.01, 0.025, 0.05)
cov_rows <- list()
for (s in names(specs)) {
  sp <- specs[[s]]; d <- design(sp$src[["POWR"]], sp$src, sp$ll)
  Xa <- cbind(d$Xc, d$Xk); fit <- lm.fit(Xa, d$y); b <- fit$coefficients
  bc <- b[1:ncol(d$Xc)]; bk <- b[-(1:ncol(d$Xc))]; e <- fit$residuals; n <- length(d$y)
  Mk <- lm.fit(d$Xc, d$Xk %*% bk)$residuals                     # commodity signal net of crypto
  sig_c <- d$Xc %*% bc
  pop_gain <- function(g) var(g * Mk) / (var(sig_c + g * d$Xk %*% bk) + var(e))
  for (tau in TARGETS) {
    g <- if (tau == 0) 0 else uniroot(function(g) pop_gain(g) - tau, c(0, 100))$root
    t0 <- Sys.time()
    res <- do.call(rbind, mclapply(seq_len(NREP), function(r) {
      set.seed(10000 * which(TARGETS == tau) + r + 1e6 * match(s, names(specs)))
      ys <- as.numeric(sig_c + g * d$Xk %*% bk + e[blocks_idx(n, sp$L)])
      bounds(ys, d$Xc, d$Xk, sp$L, BMC)
    }, mc.cores = as.integer(Sys.getenv("CORES", "6"))))
    cov_rows[[paste(s, tau)]] <- tibble(spec = s, true_gain = 100 * tau, reps = NREP,
      mean_est = 100 * mean(res[, "est"]), mean_shift = 100 * mean(res[, "q50"] - res[, "est"]),
      cover_percentile = mean(res[, "up_pct"] >= tau), cover_recentred = mean(res[, "up_rec"] >= tau),
      cover_ncp = mean(res[, "up_ncp"] >= tau),
      mean_up_percentile = 100 * mean(res[, "up_pct"]), mean_up_recentred = 100 * mean(res[, "up_rec"]),
      mean_up_ncp = 100 * mean(res[, "up_ncp"]))
    cat(s, tau, "done in", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), "s\n")
    print(cov_rows[[paste(s, tau)]])
  }
}
cv <- bind_rows(cov_rows) %>% mutate(across(where(is.numeric), ~ round(., 3)))
write_csv(cv, "Output/jcm/bootstrap_coverage.csv")
print(as.data.frame(cv))
