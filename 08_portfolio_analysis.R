# =============================================================================
# 08_portfolio_analysis.R
# Policy Implications: Portfolio Diversification Analysis
#
# Tests whether including POWR, SNC, or EWT in a multi-asset portfolio
# improves risk-adjusted performance relative to a baseline of 24 non-token
# assets (BTC, ETH, SOL, AVAX, DPI, IDEX, SPTOP20, EUA, NEE, ENPH, PBW,
# PBD, FENY, QCLN, ICLN, B, CL, HH, GC, HG, LIT, ESGD, ESGU, BGRN).
#
# Three strategies : EW (equal-weighted), GMV (global minimum variance),
#                    MVE (max Sharpe / mean-variance efficient)
# Three horizons   : total (rolling sample covariance),
#                    short-run (bandpass 1–5 days),
#                    long-run  (bandpass >20 days)
# Sub-periods      : Pre-crisis | Crypto turmoil | Energy crisis | Recovery
#
# Outputs          : CSV tables  → Output/portfolio/
#                    PNG figures → figures/portfolio/
#
# Required packages — install once if missing:
#   install.packages(c("quadprog", "mFilter", "patchwork", "ggnewscale"))
# =============================================================================

rm(list = ls())

# ── packages ─────────────────────────────────────────────────────────────────
library(tidyverse)
library(lubridate)
library(quadprog)   # constrained QP
library(mFilter)    # Baxter-King bandpass filter
library(ggplot2)
library(scales)
library(patchwork)

set.seed(2026)

# ── 0. Global parameters ─────────────────────────────────────────────────────

WINDOW <- 252          # rolling estimation window (trading days)
REBAL  <- 21           # rebalancing frequency (~1 month)
RF     <- 0            # annualised risk-free rate (set to 0 for comparability)
FREQ   <- 252          # annualisation factor
NBOOT  <- 5000         # unused since the Ledoit-Wolf (2008) HAC test replaced the bootstrap

# Sub-period boundaries
PERIODS <- list(
  "Pre-crisis"      = c("2020-12-08", "2021-12-31"),
  "Crypto turmoil"  = c("2022-01-01", "2022-06-30"),
  "Energy crisis"   = c("2022-07-01", "2022-12-31"),
  "Recovery"        = c("2023-01-01", "2026-01-31")
)

# Event shading for plots (matching the rest of the paper)
EVENTS <- data.frame(
  xmin  = as.Date(c("2021-01-01", "2022-04-01")),
  xmax  = as.Date(c("2021-04-01", "2022-10-01")),
  event = c("Crypto surge", "Energy crisis"),   # 2021 band is the crypto surge, distinct from the 2022 crypto-turmoil sub-period
  stringsAsFactors = FALSE
)

# Token colours (red / blue / green → consistent with net-connectedness plots)
TOKEN_COLS <- c(
  Baseline = "black",
  POWR     = "#e63946",
  SNC      = "#457b9d",
  EWT      = "#2a9d8f"
)

# ── 1. Load return data ───────────────────────────────────────────────────────
# Each RData file contains 'ret': data.frame with Date + 25 columns of
# log-returns (decimal, NOT ×100).  Column 1 = energy token; columns 2–25 =
# the 24 non-token assets.

load_returns <- function(path) {
  load(path)
  stopifnot(exists("ret"))
  ret$Date <- as.Date(ret$Date)
  ret
}

r_pwr <- load_returns("Data/return_PWR.RData")
r_ewt <- load_returns("Data/return_EWT.RData")
r_snc <- load_returns("Data/return_SNC.RData")

# Convenience list
TOKENS <- list(
  list(name = "POWR", data = r_pwr),
  list(name = "SNC",  data = r_snc),
  list(name = "EWT",  data = r_ewt)
)


# ── 2. Portfolio weight functions ─────────────────────────────────────────────

# Equal-weighted
ew_weights <- function(n) rep(1 / n, n)

# Global Minimum Variance  (long-only, fully invested)
gmv_weights <- function(Sigma) {
  n    <- nrow(Sigma)
  Dmat <- 2 * Sigma + diag(1e-8, n)   # tiny ridge for numerical stability
  dvec <- rep(0, n)
  Amat <- cbind(rep(1, n), diag(n))   # equality: sum=1; inequality: w≥0
  bvec <- c(1, rep(0, n))
  sol  <- tryCatch(
    solve.QP(Dmat, dvec, Amat, bvec, meq = 1),
    error = function(e) NULL
  )
  if (is.null(sol)) return(ew_weights(n))
  w <- pmax(sol$solution, 0)
  w / sum(w)
}

# Max-Sharpe (tangency portfolio, long-only)
mve_weights <- function(mu, Sigma, rf = 0) {
  n      <- nrow(Sigma)
  excess <- mu - rf / FREQ       # daily excess return
  if (all(excess <= 0)) return(gmv_weights(Sigma))
  Dmat <- 2 * Sigma + diag(1e-8, n)
  dvec <- excess
  Amat <- cbind(rep(1, n), diag(n))
  bvec <- c(1, rep(0, n))
  sol  <- tryCatch(
    solve.QP(Dmat, dvec, Amat, bvec, meq = 1),
    error = function(e) NULL
  )
  if (is.null(sol)) return(gmv_weights(Sigma))
  w <- pmax(sol$solution, 0)
  if (sum(w) < 1e-10) return(gmv_weights(Sigma))
  w / sum(w)
}


# ── 3. Spectral covariance via Baxter-King bandpass filter ───────────────────
# Decomposes each return series into a frequency band and returns the
# covariance matrix of the filtered series.
#   horizon = "short" : cycles of 1–5 trading days
#   horizon = "long"  : cycles of 20–252 trading days

spectral_cov <- function(R, horizon = c("short", "long"), bk_lags = 6) {
  horizon <- match.arg(horizon)
  n_obs   <- nrow(R)
  n_vars  <- ncol(R)

  # Band limits (in periods / days)
  if (horizon == "short") {
    pl <- 2; pu <- 5
  } else {
    pl <- 20; pu <- min(n_obs %/% 3, 252)
  }

  # Minimum window needed: 2*bk_lags + pl
  min_obs <- 2 * bk_lags + pl + 5
  if (n_obs < min_obs) return(cov(R, use = "complete.obs"))

  filt <- matrix(NA_real_, nrow = n_obs, ncol = n_vars)
  for (j in seq_len(n_vars)) {
    bk <- tryCatch(
      bkfilter(R[, j], pl = pl, pu = pu, drift = FALSE)$cycle,
      error = function(e) rep(NA_real_, n_obs)
    )
    filt[, j] <- bk
  }

  filt <- filt[complete.cases(filt), , drop = FALSE]
  if (nrow(filt) < 10) return(cov(R, use = "complete.obs"))
  cov(filt)
}

# Helper: ensure covariance matrix is positive definite (add ridge if needed)
make_pd <- function(Sigma) {
  ev <- min(eigen(Sigma, symmetric = TRUE, only.values = TRUE)$values)
  if (ev <= 0) Sigma <- Sigma + (-ev + 1e-7) * diag(nrow(Sigma))
  Sigma
}


# ── 4. Rolling portfolio engine ───────────────────────────────────────────────
# Produces out-of-sample daily portfolio returns.
# 'ret_df'     : data.frame with Date + numeric return columns
# 'token_col'  : column name of the energy token (NULL = already excluded)
# 'include_tok': TRUE = use all columns; FALSE = drop token column (baseline)

rolling_portfolio <- function(ret_df,
                              token_col    = NULL,
                              include_tok  = TRUE,
                              strategy     = c("EW", "GMV", "MVE"),
                              horizon      = c("total", "short", "long"),
                              window       = WINDOW,
                              rebal        = REBAL) {
  strategy <- match.arg(strategy)
  horizon  <- match.arg(horizon)

  dates <- ret_df$Date
  R_all <- as.matrix(ret_df[, -1])   # drop Date column

  # Select asset columns
  if (!is.null(token_col) && !include_tok) {
    keep_cols <- which(colnames(R_all) != token_col)
  } else {
    keep_cols <- seq_len(ncol(R_all))
  }
  R <- R_all[, keep_cols, drop = FALSE]
  n_obs <- nrow(R)

  # Storage
  port_ret <- rep(NA_real_, n_obs)

  # Rebalancing dates (first rebalance after a full window)
  rebal_idx <- seq(window + 1, n_obs - 1, by = rebal)

  for (t0 in rebal_idx) {
    t_end  <- t0 - 1
    t_start <- t_end - window + 1
    t_next  <- min(t0 + rebal - 1, n_obs)

    R_win <- R[t_start:t_end, , drop = FALSE]

    # Drop columns with >10% missing
    na_frac <- colMeans(is.na(R_win))
    ok_cols <- which(na_frac < 0.10)
    if (length(ok_cols) < 2) next
    R_win <- na.omit(R_win[, ok_cols, drop = FALSE])
    if (nrow(R_win) < 50) next

    n_a <- ncol(R_win)
    mu  <- colMeans(R_win)

    # Covariance matrix (total or frequency-filtered)
    Sigma <- if (horizon == "total") {
      cov(R_win)
    } else {
      spectral_cov(R_win, horizon = horizon)
    }
    Sigma <- make_pd(Sigma)

    # Portfolio weights
    w_trim <- switch(strategy,
      EW  = ew_weights(n_a),
      GMV = gmv_weights(Sigma),
      MVE = mve_weights(mu, Sigma)
    )

    # Map weights back to full asset set (zero for dropped columns)
    w_full <- rep(0, ncol(R))
    w_full[ok_cols] <- w_trim

    # Apply weights to out-of-sample window
    R_next <- R[t0:t_next, , drop = FALSE]
    port_ret[t0:t_next] <- as.numeric(R_next %*% w_full)
  }

  data.frame(Date = dates, port_ret = port_ret)
}


# ── 5. Performance metrics ────────────────────────────────────────────────────

perf_metrics <- function(ret_vec, freq = FREQ) {
  r <- na.omit(ret_vec)                  # already in decimal form
  if (length(r) < 20) return(NULL)

  ann_ret  <- mean(r) * freq
  ann_vol  <- sd(r)   * sqrt(freq)
  SR       <- ann_ret / ann_vol

  neg_r    <- r[r < 0]
  sortino  <- if (length(neg_r) > 0)
    ann_ret / (sd(neg_r) * sqrt(freq)) else NA_real_

  cum_r  <- cumprod(1 + r)
  peak   <- cummax(cum_r)
  mdd    <- min((cum_r - peak) / peak)
  calmar <- ann_ret / abs(mdd)

  q5   <- quantile(r, 0.05)
  var5 <- q5 * 100
  cvar5 <- mean(r[r <= q5]) * 100

  data.frame(
    ann_ret = round(ann_ret * 100, 2),  # in %
    ann_vol = round(ann_vol * 100, 2),  # in %
    SR      = round(SR,      3),
    Sortino = round(sortino, 3),
    MDD     = round(mdd * 100, 2),      # in %
    Calmar  = round(calmar,  3),
    VaR5    = round(var5,    3),        # daily, in %
    CVaR5   = round(cvar5,   3)         # daily, in %
  )
}

# Diversification ratio: weighted-avg vol / portfolio vol
div_ratio <- function(R_win, weights) {
  sigma_i  <- apply(R_win, 2, sd, na.rm = TRUE)
  Sigma    <- cov(R_win, use = "complete.obs")
  port_vol <- sqrt(as.numeric(t(weights) %*% Sigma %*% weights))
  if (port_vol == 0) return(NA_real_)
  sum(weights * sigma_i) / port_vol
}

# Test of equal Sharpe ratios: HAC delta-method test of Ledoit & Wolf (2008,
# J. Empirical Finance), Newey-West kernel. The two return series are paired
# day by day, so their strong correlation is accounted for. (An earlier version
# compared an uncentred bootstrap distribution with the observed difference,
# which bounds the p-value near 0.5 or above whatever the data; replaced.)
boot_sr_diff <- function(r_aug, r_base, B = NBOOT) {
  ok <- complete.cases(r_aug, r_base)
  x <- r_aug[ok]; y <- r_base[ok]; n <- length(x)
  m1 <- mean(x); m2 <- mean(y); g1 <- mean(x^2); g2 <- mean(y^2)
  s1 <- sqrt(g1 - m1^2); s2 <- sqrt(g2 - m2^2)
  d  <- m1 / s1 - m2 / s2
  grad <- c(g1 / s1^3, -g2 / s2^3, -m1 / (2 * s1^3), m2 / (2 * s2^3))
  Y <- cbind(x - m1, y - m2, x^2 - g1, y^2 - g2)
  L <- floor(4 * (n / 100)^(2 / 9))
  Psi <- crossprod(Y) / n
  for (l in seq_len(L)) {
    G <- crossprod(Y[(l + 1):n, , drop = FALSE], Y[1:(n - l), , drop = FALSE]) / n
    Psi <- Psi + (1 - l / (L + 1)) * (G + t(G))
  }
  se <- sqrt(as.numeric(t(grad) %*% Psi %*% grad) / n)
  obs_diff <- (mean(x) / sd(x) - mean(y) / sd(y)) * sqrt(FREQ)   # as reported in the tables
  p_val <- 2 * pnorm(-abs(d / se))
  list(diff = round(obs_diff, 3), p_value = round(p_val, 3))
}

# Stars for significance
sig_stars <- function(p) {
  if (is.na(p))      return("")
  if (p < 0.01)      return("***")
  if (p < 0.05)      return("**")
  if (p < 0.10)      return("*")
  return("")
}


# ── 6. Run all portfolios ─────────────────────────────────────────────────────

STRATEGIES <- c("EW", "GMV", "MVE")
HORIZONS   <- c("total", "short", "long")

message("Running portfolio analysis — this may take several minutes...")

all_results <- list()

for (tok in TOKENS) {
  tok_name <- tok$name
  ret_df   <- tok$data

  message("  Token: ", tok_name)

  for (strat in STRATEGIES) {
    for (hor in HORIZONS) {
      key <- paste(tok_name, strat, hor, sep = "_")
      message("    ", key)

      p_base <- rolling_portfolio(ret_df,
                                  token_col   = tok_name,
                                  include_tok = FALSE,
                                  strategy    = strat,
                                  horizon     = hor)

      p_aug  <- rolling_portfolio(ret_df,
                                  token_col   = tok_name,
                                  include_tok = TRUE,
                                  strategy    = strat,
                                  horizon     = hor)

      all_results[[key]] <- list(
        token    = tok_name,
        strategy = strat,
        horizon  = hor,
        base     = p_base,
        aug      = p_aug
      )
    }
  }
}

message("Portfolio simulation complete.")


# ── 7. Full-sample performance tables ─────────────────────────────────────────

build_full_table <- function(results,
                             strategies = STRATEGIES,
                             horizon    = "total") {
  rows <- list()

  # Baseline row (common 24-asset portfolio, use POWR file which determines ordering)
  for (strat in strategies) {
    key    <- paste("POWR", strat, horizon, sep = "_")
    res    <- results[[key]]
    m_base <- perf_metrics(res$base$port_ret)
    if (is.null(m_base)) next
    rows[[paste("Baseline", strat)]] <- data.frame(
      Token    = "Baseline",
      Strategy = strat,
      m_base,
      Delta_SR  = NA_real_,
      Delta_vol = NA_real_,
      p_value   = NA_real_,
      Stars     = ""
    )
  }

  # Augmented rows
  for (tok_name in c("POWR", "SNC", "EWT")) {
    for (strat in strategies) {
      key   <- paste(tok_name, strat, horizon, sep = "_")
      res   <- results[[key]]
      m_aug <- perf_metrics(res$aug$port_ret)
      if (is.null(m_aug)) next

      # Compare against THIS token's own baseline (same 24-asset universe,
      # same date coverage — each file's drop_na() may differ slightly)
      base_key   <- paste(tok_name, strat, horizon, sep = "_")
      r_base_vec <- results[[base_key]]$base$port_ret
      boot       <- boot_sr_diff(res$aug$port_ret, r_base_vec)

      rows[[paste(tok_name, strat)]] <- data.frame(
        Token     = paste0("+", tok_name),
        Strategy  = strat,
        m_aug,
        Delta_SR  = round(boot$diff, 3),
        Delta_vol = round(
          perf_metrics(r_base_vec)$ann_vol - m_aug$ann_vol, 2),
        p_value   = boot$p_value,
        Stars     = sig_stars(boot$p_value)
      )
    }
  }

  bind_rows(rows)
}

tab_total <- build_full_table(all_results, horizon = "total")
tab_short <- build_full_table(all_results, horizon = "short")
tab_long  <- build_full_table(all_results, horizon = "long")

# Horizon-specific table (GMV only, short vs long side-by-side)
tab_horizon <- bind_rows(
  tab_short %>% filter(Strategy == "GMV") %>% mutate(Horizon = "Short-run"),
  tab_long  %>% filter(Strategy == "GMV") %>% mutate(Horizon = "Long-run")
) %>% select(Horizon, Token, everything(), -Strategy)

# Sub-period table
build_subperiod_table <- function(results, strategy = "GMV", horizon = "total") {
  rows <- list()
  for (period_name in names(PERIODS)) {
    p_start <- as.Date(PERIODS[[period_name]][1])
    p_end   <- as.Date(PERIODS[[period_name]][2])

    for (tok_name in c("POWR", "SNC", "EWT")) {
      key    <- paste(tok_name, strategy, horizon, sep = "_")
      res    <- results[[key]]

      sub_base <- res$base$port_ret[res$base$Date >= p_start &
                                    res$base$Date <= p_end]
      sub_aug  <- res$aug$port_ret[ res$aug$Date  >= p_start &
                                    res$aug$Date  <= p_end]

      if (sum(!is.na(sub_base)) < 20) next

      m_b <- perf_metrics(sub_base)
      m_a <- perf_metrics(sub_aug)
      if (is.null(m_b) || is.null(m_a)) next

      boot <- boot_sr_diff(sub_aug, sub_base)

      rows[[paste(period_name, tok_name)]] <- data.frame(
        Period    = period_name,
        Token     = tok_name,
        SR_base   = m_b$SR,
        SR_aug    = m_a$SR,
        Delta_SR  = boot$diff,
        Vol_base  = m_b$ann_vol,
        Vol_aug   = m_a$ann_vol,
        Delta_vol = m_b$ann_vol - m_a$ann_vol,
        MDD_base  = m_b$MDD,
        MDD_aug   = m_a$MDD,
        p_value   = boot$p_value,
        Stars     = sig_stars(boot$p_value)
      )
    }
  }
  bind_rows(rows)
}

tab_subperiod <- build_subperiod_table(all_results, strategy = "GMV", horizon = "total")

# Save CSVs
dir.create("Output/portfolio", showWarnings = FALSE, recursive = TRUE)
write_csv(tab_total,     "Output/portfolio/perf_total_all.csv")
write_csv(tab_short,     "Output/portfolio/perf_short_all.csv")
write_csv(tab_long,      "Output/portfolio/perf_long_all.csv")
write_csv(tab_horizon,   "Output/portfolio/perf_horizon_GMV.csv")
write_csv(tab_subperiod, "Output/portfolio/perf_subperiod_GMV.csv")

message("Performance tables saved to Output/portfolio/")


# ── 8. Figures ────────────────────────────────────────────────────────────────

fig_dir <- "Output/figures/portfolio"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# ── 8a. Cumulative return comparison (GMV, total covariance) ─────────────────

make_cumret_plot <- function(results,
                             strategy = "GMV",
                             horizon  = "total",
                             title    = NULL) {
  cum_data <- list()

  # Baseline from POWR file
  base_ret <- results[[paste("POWR", strategy, horizon, sep = "_")]]$base
  cum_data[["Baseline"]] <- base_ret %>%
    filter(!is.na(port_ret)) %>%
    mutate(Label = "Baseline",
           cum   = cumprod(1 + port_ret))

  for (tok_name in c("POWR", "SNC", "EWT")) {
    aug_ret <- results[[paste(tok_name, strategy, horizon, sep = "_")]]$aug
    cum_data[[tok_name]] <- aug_ret %>%
      filter(!is.na(port_ret)) %>%
      mutate(Label = paste0("+", tok_name),
             cum   = cumprod(1 + port_ret))
  }

  plot_df <- bind_rows(cum_data) %>%
    mutate(Label = factor(Label, levels = c("Baseline", "+POWR", "+SNC", "+EWT")))

  lw <- setNames(c(1.0, 0.7, 0.7, 0.7), c("Baseline", "+POWR", "+SNC", "+EWT"))
  lt <- setNames(c("solid", "solid", "dashed", "dotted"),
                 c("Baseline", "+POWR", "+SNC", "+EWT"))

  col_map <- c(Baseline = "black", `+POWR` = "#e63946",
               `+SNC`   = "#457b9d", `+EWT`  = "#2a9d8f")

  if (is.null(title))
    title <- paste0("Cumulative portfolio returns — ", strategy, " strategy")

  y_lo <- min(plot_df$cum, na.rm = TRUE) * 0.90
  y_hi <- max(plot_df$cum, na.rm = TRUE) * 1.10

  ggplot() +
    annotate("rect", xmin = as.Date("2021-01-01"), xmax = as.Date("2021-04-01"),
             ymin = y_lo, ymax = y_hi, fill = "lightgreen", alpha = 0.25) +
    annotate("rect", xmin = as.Date("2022-04-01"), xmax = as.Date("2022-10-01"),
             ymin = y_lo, ymax = y_hi, fill = "orange", alpha = 0.25) +
    geom_line(data = plot_df,
              aes(x = Date, y = cum, colour = Label,
                  linewidth = Label, linetype = Label)) +
    scale_colour_manual(values = col_map, name = "Portfolio") +
    scale_linewidth_manual(values = lw,   name = "Portfolio") +
    scale_linetype_manual( values = lt,   name = "Portfolio") +
    scale_y_log10(labels = label_percent(accuracy = 1,
                                          scale    = 100)) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(x = NULL, y = "Cumulative return (log scale)", title = title) +
    theme_classic(base_size = 11) +
    theme(legend.position  = "bottom",
          plot.title       = element_text(size = 11, face = "bold"),
          panel.grid.major = element_line(colour = "grey92"),
          panel.grid.minor = element_blank())
}

# Try with ggnewscale if available, else fallback without event shading
p_cumret <- tryCatch(
  make_cumret_plot(all_results, strategy = "GMV", horizon = "total"),
  error = function(e) {
    message("Cumret plot error: ", e$message)

    cum_data <- list()
    base_ret <- all_results[["POWR_GMV_total"]]$base
    cum_data[["Baseline"]] <- base_ret %>% filter(!is.na(port_ret)) %>%
      mutate(Label = "Baseline", cum = cumprod(1 + port_ret))
    for (tn in c("POWR", "SNC", "EWT")) {
      aug_ret <- all_results[[paste(tn, "GMV", "total", sep = "_")]]$aug
      cum_data[[tn]] <- aug_ret %>% filter(!is.na(port_ret)) %>%
        mutate(Label = paste0("+", tn), cum = cumprod(1 + port_ret))
    }
    plot_df <- bind_rows(cum_data) %>%
      mutate(Label = factor(Label, levels = c("Baseline", "+POWR", "+SNC", "+EWT")))

    col_map <- c(Baseline = "black", `+POWR` = "#e63946",
                 `+SNC`   = "#457b9d", `+EWT`  = "#2a9d8f")
    y_lo <- min(plot_df$cum, na.rm = TRUE) * 0.90
    y_hi <- max(plot_df$cum, na.rm = TRUE) * 1.10
    ggplot(plot_df, aes(x = Date, y = cum, colour = Label)) +
      annotate("rect", xmin = as.Date("2021-01-01"), xmax = as.Date("2021-04-01"),
               ymin = y_lo, ymax = y_hi, fill = "lightgreen", alpha = 0.25) +
      annotate("rect", xmin = as.Date("2022-04-01"), xmax = as.Date("2022-10-01"),
               ymin = y_lo, ymax = y_hi, fill = "orange", alpha = 0.25) +
      geom_line(linewidth = 0.7) +
      scale_colour_manual(values = col_map, name = "Portfolio") +
      scale_y_log10(labels = label_percent(accuracy = 1, scale = 100)) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      labs(x = NULL, y = "Cumulative return (log scale)",
           title = "Cumulative portfolio returns — GMV strategy") +
      theme_classic(base_size = 11) +
      theme(legend.position = "bottom",
            panel.grid.major = element_line(colour = "grey92"))
  }
)

ggsave(file.path(fig_dir, "cumret_gmv.png"),
       p_cumret, width = 10, height = 5, dpi = 300)
message("Saved: figures/portfolio/cumret_gmv.png")

p_cumret_t <- p_cumret + 
  theme(
    panel.background  = element_rect(fill = "transparent", colour = NA),
    plot.background   = element_rect(fill = "transparent", colour = NA),
    panel.grid.major  = element_line(colour = "grey90"),  # optional
    panel.grid.minor  = element_blank(),                  # optional
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA)
  )

ggsave(file.path(fig_dir, "cumret_gmv_t.png"),
       p_cumret_t, width = 10, height = 5, dpi = 300, bg = "transparent")
message("Saved: figures/portfolio/cumret_gmv_t.png")

# ── 8b. Rolling Sharpe ratio gain (ΔSR) over time — GMV ─────────────────────

make_rolling_sr_plot <- function(results,
                                 strategy = "GMV",
                                 horizon  = "total",
                                 roll_win = 126) {   # 6-month rolling SR

  rolling_sr <- function(r, w) {
    n <- length(r)
    if (n < w) return(rep(NA_real_, n))
    sr <- rep(NA_real_, n)
    for (i in w:n) {
      sub <- r[(i - w + 1):i]
      if (any(is.na(sub))) next
      sr[i] <- mean(sub) / sd(sub) * sqrt(FREQ)
    }
    sr
  }

  plot_list <- list()
  base_ret  <- all_results[[paste("POWR", strategy, horizon, sep = "_")]]$base

  for (tok_name in c("POWR", "SNC", "EWT")) {
    key     <- paste(tok_name, strategy, horizon, sep = "_")
    aug_ret <- all_results[[key]]$aug

    n    <- min(nrow(base_ret), nrow(aug_ret))
    r_b  <- base_ret$port_ret[seq_len(n)]
    r_a  <- aug_ret$port_ret[ seq_len(n)]
    dates <- aug_ret$Date[seq_len(n)]

    sr_b   <- rolling_sr(r_b, roll_win)
    sr_a   <- rolling_sr(r_a, roll_win)
    delta  <- sr_a - sr_b

    plot_list[[tok_name]] <- data.frame(
      Date  = dates,
      Delta = delta,
      Token = tok_name
    )
  }

  plot_df <- bind_rows(plot_list) %>%
    mutate(Token = factor(Token, levels = c("POWR", "SNC", "EWT")))

  col_map <- c(POWR = "#e63946", SNC = "#457b9d", EWT = "#2a9d8f")

  ggplot(plot_df, aes(x = Date, y = Delta, colour = Token)) +
    annotate("rect", xmin = as.Date("2021-01-01"), xmax = as.Date("2021-04-01"),
             ymin = -Inf, ymax = Inf, fill = "lightgreen", alpha = 0.25) +
    annotate("rect", xmin = as.Date("2022-04-01"), xmax = as.Date("2022-10-01"),
             ymin = -Inf, ymax = Inf, fill = "orange", alpha = 0.25) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
    geom_line(linewidth = 0.75, na.rm = TRUE) +
    scale_colour_manual(values = col_map, name = "Token added") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(x = NULL,
         y = expression(paste(Delta, "SR (annualised)")),
         title = paste0("Rolling Sharpe ratio gain from adding energy token — ",
                        strategy, " strategy")) +
    facet_wrap(~ Token, ncol = 1, scales = "free_y") +
    theme_classic(base_size = 10) +
    theme(legend.position = "none",
          strip.text = element_text(face = "bold"),
          panel.grid.major = element_line(colour = "grey92"))
}

p_sr <- tryCatch(
  make_rolling_sr_plot(all_results),
  error = function(e) { message("SR plot error: ", e$message); NULL }
)
if (!is.null(p_sr)) {
  ggsave(file.path(fig_dir, "rolling_sr_gain_gmv.png"),
         p_sr, width = 9, height = 8, dpi = 300)
  message("Saved: figures/portfolio/rolling_sr_gain_gmv.png")
}

p_sr_t <- p_sr + 
  theme(
    panel.background  = element_rect(fill = "transparent", colour = NA),
    plot.background   = element_rect(fill = "transparent", colour = NA),
    panel.grid.major  = element_line(colour = "grey90"),  # optional
    panel.grid.minor  = element_blank(),                  # optional
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA)
  )

ggsave(file.path(fig_dir, "rolling_sr_gain_gmv_t.png"),
       p_sr_t, width = 10, height = 5, dpi = 300, bg = "transparent")
message("Saved: figures/portfolio/rolling_sr_gain_gmv_t.png")

# ── 8c. Horizon comparison: ΔSR bar chart (short vs long) ───────────────────

sr_bar_data <- tab_horizon %>%
  filter(!is.na(Delta_SR), Token != "Baseline") %>%
  mutate(
    Token   = factor(Token, levels = c("+POWR", "+SNC", "+EWT")),
    Horizon = factor(Horizon, levels = c("Short-run", "Long-run"))
  )

p_bar <- ggplot(sr_bar_data,
                aes(x = Token, y = Delta_SR, fill = Horizon)) +
  geom_col(position = position_dodge(width = 0.6), width = 0.55) +
  geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.4) +
  geom_text(aes(label = paste0(ifelse(Delta_SR > 0, "+", ""), Delta_SR)),
            position = position_dodge(width = 0.6),
            vjust = ifelse(sr_bar_data$Delta_SR >= 0, -0.4, 1.2),
            size = 3) +
  scale_fill_manual(values = c("Short-run" = "#457b9d",
                                "Long-run"  = "#2a9d8f"),
                    name = "Horizon") +
  labs(x = NULL,
       y = expression(paste(Delta, "SR (Sharpe ratio gain, GMV)")),
       title = "") +
  theme_classic(base_size = 11) +
  theme(legend.position = "bottom",
        panel.grid.major.y = element_line(colour = "grey92"))

ggsave(file.path(fig_dir, "horizon_sr_bar.png"),
       p_bar, width = 7, height = 4.5, dpi = 300)
message("Saved: figures/portfolio/horizon_sr_bar.png")

p_bar_t <- p_bar + 
  theme(
    panel.background  = element_rect(fill = "transparent", colour = NA),
    plot.background   = element_rect(fill = "transparent", colour = NA),
    panel.grid.major  = element_line(colour = "grey90"),  # optional
    panel.grid.minor  = element_blank(),                  # optional
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA)
  )

ggsave(file.path(fig_dir, "horizon_sr_bar_t.png"),
       p_bar_t, width = 10, height = 5, dpi = 300, bg = "transparent")
message("Saved: figures/portfolio/horizon_sr_bar_t.png")


# ── 9. Print summary to console ──────────────────────────────────────────────

cat("\n", strrep("=", 70), "\n")
cat("FULL-SAMPLE GMV PERFORMANCE (total covariance)\n")
cat(strrep("=", 70), "\n")
print(tab_total %>% filter(Strategy == "GMV") %>%
        select(Token, ann_ret, ann_vol, SR, MDD, Delta_SR, Stars))

cat("\n", strrep("=", 70), "\n")
cat("HORIZON-SPECIFIC GMV PERFORMANCE\n")
cat(strrep("=", 70), "\n")
print(tab_horizon %>% select(Horizon, Token, SR, ann_vol, Delta_SR, Stars))

cat("\n", strrep("=", 70), "\n")
cat("SUB-PERIOD SHARPE RATIO GAINS (GMV, total covariance)\n")
cat(strrep("=", 70), "\n")
print(tab_subperiod %>% select(Period, Token, SR_base, SR_aug, Delta_SR, Stars))


# ── 10. Export LaTeX-formatted numbers ───────────────────────────────────────
# Produces a helper CSV with the exact numbers to paste into the paper tables.

latex_table_full <- tab_total %>%
  filter(Strategy == "GMV") %>%
  mutate(
    SR_str    = ifelse(is.na(Delta_SR), as.character(SR),
                       paste0(SR, " & ", Delta_SR, Stars)),
    vol_str   = paste0(ann_vol, "\\%"),
    ret_str   = paste0(ann_ret, "\\%"),
    mdd_str   = paste0(MDD, "\\%"),
    var_str   = paste0(VaR5),
    cvar_str  = paste0(CVaR5)
  ) %>%
  select(Token, ret_str, vol_str, SR, Sortino, mdd_str, Calmar,
         var_str, cvar_str, Delta_SR, Stars)

write_csv(latex_table_full, "Output/portfolio/latex_numbers_full_GMV.csv")

message("\nAll outputs saved.")
message("Tables : Output/portfolio/")
message("Figures: ", fig_dir)

