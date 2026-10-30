# =============================================================================
# 23_portfolio_timing.R   (JCM, referee check on portfolio timing)
#
# In the baseline the weights are estimated on returns up to day t-1 and held
# from day t. Tokens are priced at 00:00 UTC, so the token return of day t starts
# before the last exchange close in the estimation window. Here the weights are
# estimated on returns up to day t-2 instead (one-day implementation lag), which
# removes any overlap, for the commodity-futures GMV portfolio of 19_jcm_tables.R
# and for the 24-asset GMV portfolio.
#
# Output: Output/jcm/portfolio_timing.csv
# =============================================================================

rm(list = ls())
suppressMessages({ library(tidyverse); library(quadprog) })
load("Data/returns_full30.RData")
src19 <- readLines("19_jcm_tables.R")
i0 <- grep("^make_pd <- ", src19); i1 <- grep("^lw_test <- ", src19)
eval(parse(text = src19[i0:(i1 + 5)]))                    # make_pd, gmv_w, roll, SR, lw_test
WINDOW <- 252; REBAL <- 21; FREQ <- 252

roll_lag <- function(R, lag) {
  n <- nrow(R); port <- rep(NA_real_, n)
  for (t0 in seq(WINDOW + 1 + lag, n - 1, by = REBAL)) {
    Rw <- na.omit(R[(t0 - WINDOW - lag):(t0 - 1 - lag), , drop = FALSE]); w <- gmv_w(make_pd(cov(Rw)))
    tn <- min(t0 + REBAL - 1, n); Rn <- R[t0:tn, , drop = FALSE]
    port[t0:tn] <- as.numeric(ifelse(is.na(Rn), 0, Rn) %*% w)
  }
  port
}
COMMOD_PORT <- c("B", "CL", "HH", "EUA", "GC", "HG")
NONTOK <- c(CRYPTO, CRYPTO_IX, ESG, COMMOD, ENERGY_EQ)
rows <- list()
for (lag in 0:1) for (u in c("commodity", "multi-asset")) {
  base_cols <- if (u == "commodity") COMMOD_PORT else NONTOK
  b <- roll_lag(as.matrix(R30[, base_cols]), lag)
  for (a in c("POWR", "EWT", "SNC", "WOZX")) {
    g <- roll_lag(as.matrix(R30[, c(a, base_cols)]), lag)
    ok <- !is.na(b) & !is.na(g) & seq_along(b) >= WINDOW + 2   # common evaluation period
    rows[[paste(lag, u, a)]] <- tibble(lag = lag, universe = u, token = a, SR_base = SR(b[ok]), dSR = SR(g[ok]) - SR(b[ok]),
                                       p_LW = lw_test(g[ok], b[ok]))
  }
}
out <- bind_rows(rows) %>% mutate(across(where(is.numeric), ~ round(., 3)))
write_csv(out, "Output/jcm/portfolio_timing.csv")
print(out %>% pivot_wider(names_from = lag, values_from = c(SR_base, dSR, p_LW)), width = 200)
