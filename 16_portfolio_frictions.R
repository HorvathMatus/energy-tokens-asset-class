# =============================================================================
# 16_portfolio_frictions.R   (IRFA revision, editor point 2)
# Practical relevance of the token allocation in the baseline GMV portfolio:
#
#   (1) Break-even one-way trading cost on the token at which the Sharpe-ratio
#       gain disappears (all other assets pay 10 bps).
#   (2) Volume-based, time-varying token costs: half-spread plus square-root
#       market impact, sigma_t * sqrt(Q_t / V_t), with trailing 30-day volatility
#       and median dollar volume from CoinGecko, for several fund sizes.
#   (3) Capacity: fund size at which the average token position equals 10% of
#       the token's median daily volume, and the fund size at which the net
#       Sharpe-ratio gain reaches zero under the impact model.
#   (4) Custody, delisting, and regulatory loss: the annual probability of a
#       total loss of the token position that would offset the return gain.
#   (5) Excess returns over the 3-month US Treasury bill.
#   (6) WOZX added as a fourth token.
#
# Sharpe-ratio differences are tested with Ledoit & Wolf (2008).
# Output: Output/portfolio/frictions.csv, frictions_costgrid.csv
# =============================================================================

rm(list = ls())
suppressMessages({ library(tidyverse); library(quadprog) })

WINDOW <- 252; REBAL <- 21; FREQ <- 252
# one-day implementation lag: weights use returns up to t0-2 and are held from t0, so the
# first token return held (which starts at 00:00 UTC of day t0-1) begins after the last
# exchange close in the estimation window
IMPL_LAG <- 1
C_OTHER <- 0.0010                       # one-way cost on the 24 non-token assets
HS_TOKEN <- 0.0050                      # token half-spread in the impact model
AUM_GRID <- c(1e6, 1e7, 1e8)
TOKENS <- c(POWR = "PWR", SNC = "SNC", EWT = "EWT", WOZX = "WOZX")

load_returns <- function(p) { load(p); ret$Date <- as.Date(ret$Date); ret }
DAT <- lapply(TOKENS, function(s) load_returns(sprintf("Data/return_%s.RData", s)))

# ── GMV engine (same construction as 08 and 12) ──────────────────────────────
make_pd <- function(S) { ev <- min(eigen(S, symmetric = TRUE, only.values = TRUE)$values)
  if (ev <= 0) S <- S + (-ev + 1e-7) * diag(nrow(S)); S }
gmv_w <- function(S) {
  n <- nrow(S)
  sol <- tryCatch(solve.QP(2 * S + diag(1e-8, n), rep(0, n), cbind(rep(1, n), diag(n)),
                           c(1, rep(0, n)), meq = 1), error = function(e) NULL)
  if (is.null(sol)) return(rep(1 / n, n)); w <- pmax(sol$solution, 0); w / sum(w)
}
roll_gmv <- function(ret_df, token_col, include_tok) {
  R_all <- as.matrix(ret_df[, -1])
  R <- if (include_tok) R_all else R_all[, colnames(R_all) != token_col, drop = FALSE]
  n <- nrow(R); port <- rep(NA_real_, n)
  rebal_idx <- seq(WINDOW + 1 + IMPL_LAG, n - 1, by = REBAL)
  W_tar <- W_dri <- matrix(0, length(rebal_idx), ncol(R), dimnames = list(NULL, colnames(R)))
  w_prev <- rep(0, ncol(R)); t_prev <- NA
  for (r in seq_along(rebal_idx)) {
    t0 <- rebal_idx[r]; te <- t0 - 1; ts <- te - IMPL_LAG - WINDOW + 1; tn <- min(t0 + REBAL - 1, n)
    Rw <- R[ts:(te - IMPL_LAG), , drop = FALSE]; ok <- which(colMeans(is.na(Rw)) < 0.10)
    Rw <- na.omit(Rw[, ok, drop = FALSE])
    w_full <- rep(0, ncol(R)); w_full[ok] <- gmv_w(make_pd(cov(Rw)))
    w_drift <- if (sum(w_prev) > 0) {
      gr <- apply(exp(R[t_prev:te, , drop = FALSE]), 2, function(x) { x[is.na(x)] <- 1; prod(x) })
      wd <- w_prev * gr; wd / sum(wd) } else rep(0, ncol(R))
    W_tar[r, ] <- w_full; W_dri[r, ] <- w_drift; w_prev <- w_full; t_prev <- t0
    Rn <- R[t0:tn, , drop = FALSE]; port[t0:tn] <- as.numeric(ifelse(is.na(Rn), 0, Rn) %*% w_full)
  }
  list(dates = ret_df$Date, gross = port, W_tar = W_tar, W_dri = W_dri, rebal_idx = rebal_idx)
}

# cost_fun(i, dW) returns the proportional cost charged at rebalance i
net_returns <- function(res, cost_fun) {
  g <- res$gross
  for (i in seq_along(res$rebal_idx)) {
    t0 <- res$rebal_idx[i]; dW <- abs(res$W_tar[i, ] - res$W_dri[i, ])
    g[t0] <- g[t0] - cost_fun(i, dW)
  }
  g
}
SR <- function(x) { x <- na.omit(x); mean(x) / sd(x) * sqrt(FREQ) }

lw_test <- function(ra, rb) {
  ok <- complete.cases(ra, rb); x <- ra[ok]; y <- rb[ok]; n <- length(x)
  m1 <- mean(x); m2 <- mean(y); g1 <- mean(x^2); g2 <- mean(y^2)
  s1 <- sqrt(g1 - m1^2); s2 <- sqrt(g2 - m2^2); d <- m1 / s1 - m2 / s2
  grad <- c(g1 / s1^3, -g2 / s2^3, -m1 / (2 * s1^3), m2 / (2 * s2^3))
  Y <- cbind(x - m1, y - m2, x^2 - g1, y^2 - g2); L <- floor(4 * (n / 100)^(2 / 9))
  Psi <- crossprod(Y) / n
  for (l in seq_len(L)) {
    G <- crossprod(Y[(l + 1):n, , drop = FALSE], Y[1:(n - l), , drop = FALSE]) / n
    Psi <- Psi + (1 - l / (L + 1)) * (G + t(G))
  }
  2 * pnorm(-abs(d / sqrt(as.numeric(t(grad) %*% Psi %*% grad) / n)))
}

# ── Token liquidity inputs from CoinGecko (7-day calendar, trailing 30 days) ─
cg_inputs <- function(tk) {
  read_csv(sprintf("Raw_Data/CoinGecko_volume/%s.csv", tk), show_col_types = FALSE) %>%
    transmute(Date = as.Date(substr(event_date, 1, 10)), price = close_price_usd,
              vol = volume_usd) %>%
    distinct(Date, .keep_all = TRUE) %>% arrange(Date) %>%
    mutate(rs = price / lag(price) - 1,
           sig30 = zoo::rollapplyr(rs, 30, sd, na.rm = TRUE, fill = NA),
           vol30 = zoo::rollapplyr(vol, 30, median, na.rm = TRUE, fill = NA))
}
# value on the last calendar day strictly before each rebalance date
at_dates <- function(cg, dates, col) {
  sapply(dates, function(d) { v <- cg[[col]][cg$Date < d]; if (length(v)) tail(v, 1) else NA })
}

# ── Risk-free rate: 3-month T-bill, daily on the business-day calendar ───────
tb <- read_csv("Raw_Data/DTB3.csv", show_col_types = FALSE, na = c("", ".")) %>%
  transmute(Date = as.Date(observation_date), rf = as.numeric(DTB3) / 100 / FREQ) %>%
  arrange(Date) %>% fill(rf)
rf_on <- function(dates) sapply(dates, function(d) tail(tb$rf[tb$Date <= d], 1))

# ── Main loop ────────────────────────────────────────────────────────────────
rows <- list(); grid_rows <- list()
for (tk in names(TOKENS)) {
  D <- DAT[[tk]]
  base <- roll_gmv(D, tk, include_tok = FALSE)
  aug  <- roll_gmv(D, tk, include_tok = TRUE)
  ti   <- match(tk, colnames(aug$W_tar))
  w_tok  <- aug$W_tar[, ti]
  dw_tok <- abs(aug$W_tar[, ti] - aug$W_dri[, ti])

  flat <- function(ct) function(i, dW) {
    c_vec <- rep(C_OTHER, length(dW)); if (!is.na(ti) && length(dW) == ncol(aug$W_tar)) c_vec[ti] <- ct
    sum(dW * c_vec)
  }
  base_net <- net_returns(base, function(i, dW) sum(dW * C_OTHER))
  sr_base_net <- SR(base_net)
  dsr_flat <- function(ct) SR(net_returns(aug, flat(ct))) - sr_base_net

  # (1) break-even flat token cost
  d0 <- dsr_flat(C_OTHER)
  be_bps <- if (d0 <= 0) NA_real_ else if (dsr_flat(1) > 0) Inf else
    uniroot(dsr_flat, c(C_OTHER, 1))$root * 1e4
  for (ct in c(10, 20, 50, 100, 200, 500) / 1e4)
    grid_rows[[paste(tk, ct)]] <- tibble(token = tk, token_cost_bps = ct * 1e4, dSR_net = dsr_flat(ct))

  # (2) volume-based costs with square-root impact
  cg <- cg_inputs(tk)
  rdates <- aug$dates[aug$rebal_idx]
  # the token trade happens at the 00:00 UTC snapshot that starts the first held return,
  # which carries the date of the previous business day
  tdates <- aug$dates[aug$rebal_idx - 1]
  sig_i <- at_dates(cg, tdates, "sig30"); vol_i <- at_dates(cg, tdates, "vol30")
  impact_cost <- function(aum) function(i, dW) {
    c_vec <- rep(C_OTHER, length(dW))
    q <- dW[ti] * aum
    c_vec[ti] <- HS_TOKEN + ifelse(is.na(vol_i[i]) || vol_i[i] <= 0, 1, sig_i[i] * sqrt(q / vol_i[i]))
    sum(dW * c_vec)
  }
  dsr_aum <- function(aum) SR(net_returns(aug, impact_cost(aum))) - sr_base_net
  dsr_by_aum <- sapply(AUM_GRID, dsr_aum)

  # (3) capacity
  w_mean <- mean(w_tok[rowSums(aug$W_tar) > 0])
  cap_aum <- 0.10 * median(cg$vol[cg$Date >= min(rdates) & cg$Date <= max(rdates)], na.rm = TRUE) / w_mean
  f_log <- function(la) dsr_aum(10^la)
  be_aum <- if (f_log(3) <= 0) NA_real_ else if (f_log(12) > 0) Inf else 10^uniroot(f_log, c(3, 12))$root
  part_10m <- dw_tok * 1e7 / vol_i

  # (4) loss hazard offsetting the gross return gain
  ok <- complete.cases(aug$gross, base$gross)
  dret_ann <- mean(aug$gross[ok] - base$gross[ok]) * FREQ
  hazard <- if (dret_ann > 0) dret_ann / w_mean else NA_real_

  # (5) excess returns over the T-bill
  rf <- rf_on(aug$dates)
  sr_ex_b <- SR(base$gross - rf); sr_ex_a <- SR(aug$gross - rf)

  rows[[tk]] <- tibble(
    token = tk,
    w_mean_pct = 100 * w_mean, w_max_pct = 100 * max(w_tok),
    token_turnover_ann_pct = 100 * mean(dw_tok) * FREQ / REBAL,
    dSR_gross = SR(aug$gross) - SR(base$gross), p_LW = lw_test(aug$gross, base$gross),
    dSR_net_10bps = d0, breakeven_token_cost_bps = be_bps,
    dSR_impact_1m = dsr_by_aum[1], dSR_impact_10m = dsr_by_aum[2], dSR_impact_100m = dsr_by_aum[3],
    breakeven_aum_musd = be_aum / 1e6, capacity_aum_10pct_adv_musd = cap_aum / 1e6,
    participation_10m_median_pct = 100 * median(part_10m, na.rm = TRUE),
    participation_10m_max_pct = 100 * max(part_10m, na.rm = TRUE),
    dret_ann_bps = 1e4 * dret_ann, breakeven_loss_hazard_pct = 100 * hazard,
    SR_base_excess = sr_ex_b, SR_aug_excess = sr_ex_a, dSR_excess = sr_ex_a - sr_ex_b,
    p_LW_excess = lw_test(aug$gross - rf, base$gross - rf),
    mean_tbill_pct = 100 * mean(rf[!is.na(aug$gross)]) * FREQ)
}

fr <- bind_rows(rows) %>% mutate(across(where(is.numeric), ~ signif(., 4)))
cg_grid <- bind_rows(grid_rows) %>% mutate(dSR_net = round(dSR_net, 4))
write_csv(fr, "Output/portfolio/frictions.csv")
write_csv(cg_grid, "Output/portfolio/frictions_costgrid.csv")
options(width = 220)
print(as.data.frame(t(fr %>% column_to_rownames("token"))))
print(cg_grid %>% pivot_wider(names_from = token, values_from = dSR_net))
