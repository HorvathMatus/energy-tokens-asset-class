# =============================================================================
# 10_beta_decomposition.R   (Referee point #21)
# "Crypto beta versus energy beta": does each energy token's realised return
# variation load on crypto-market factors or on energy/green factors?
#
# Three complementary views, all on standardised daily returns:
#   (A) Named-factor regression on the referee's factor list
#       (BTC, ETH, crypto index, oil, gas, EU carbon, clean-energy ETF, green bond)
#       with Newey-West (HAC) t-statistics.
#   (B) Block R^2 decomposition: variance explained by the crypto block vs the
#       energy/green block, plus incremental R^2 of each block over the other.
#   (C) Two-factor model on the first principal component of each block,
#       giving directly comparable standardised crypto vs energy betas.
#
# Run for ALL SIX tokens so the asset-class conclusion does not depend on the
# three-token selection (addresses #9).
#
# Outputs: Output/identity/beta_named.csv, beta_blocks.csv, beta_pcfactor.csv
# =============================================================================

rm(list = ls())
suppressMessages({
  library(tidyverse)
  library(sandwich)   # Newey-West HAC covariance
  library(lmtest)     # coeftest
})

load("Data/returns_full30.RData")   # R30, GROUP, PAPER30, block vectors
dir.create("Output/identity", showWarnings = FALSE, recursive = TRUE)

# ── Factor blocks ─────────────────────────────────────────────────────────────
CRYPTO_BLOCK <- c(CRYPTO, CRYPTO_IX)                 # 7 assets
ENERGY_BLOCK <- c(COMMOD, ENERGY_EQ)                 # 14 assets (oil/gas/metals/carbon + clean-energy equity)
GREEN_BLOCK  <- ESG                                  # 3 assets

# Referee's named factors (one representative per channel)
NAMED <- c(BTC = "BTC", ETH = "ETH", CryptoIdx = "SPTOP20",
           Oil = "CL", Gas = "HH", Carbon = "EUA",
           CleanEnergyETF = "ICLN", GreenBond = "BGRN")

# ── Helpers ──────────────────────────────────────────────────────────────────
zscore <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

# HAC lag (Newey-West automatic rule)
nw_lag <- function(n) floor(4 * (n / 100)^(2 / 9))

# R^2 of token on a set of standardised factor columns (complete cases)
block_r2 <- function(y, X) {
  d <- na.omit(data.frame(y = y, X))
  if (nrow(d) < 50) return(NA_real_)
  summary(lm(y ~ ., data = d))$r.squared
}

# ── (A) Named-factor regression with HAC t-stats ─────────────────────────────
named_rows <- list()
for (tok in TOKENS_ALL) {
  df <- R30[, c(tok, NAMED)]
  colnames(df) <- c("y", names(NAMED))
  df <- as.data.frame(scale(df))           # standardise everything
  d  <- na.omit(df)
  n  <- nrow(d)
  if (n < 50) next
  fit <- lm(y ~ ., data = d)
  ct  <- coeftest(fit, vcov. = NeweyWest(fit, lag = nw_lag(n), prewhite = FALSE))
  beta <- ct[-1, "Estimate"]; tval <- ct[-1, "t value"]
  named_rows[[tok]] <- tibble(
    token = tok, n = n, R2 = summary(fit)$r.squared,
    factor = names(NAMED), beta = beta, t = tval
  )
}
beta_named <- bind_rows(named_rows)
write_csv(beta_named, "Output/identity/beta_named.csv")

# ── (B) Block R^2 decomposition ──────────────────────────────────────────────
blk_rows <- list()
for (tok in TOKENS_ALL) {
  y  <- zscore(R30[[tok]])
  Xc <- as.data.frame(scale(R30[, CRYPTO_BLOCK]))
  Xe <- as.data.frame(scale(R30[, ENERGY_BLOCK]))
  Xg <- as.data.frame(scale(R30[, GREEN_BLOCK]))

  r2_c  <- block_r2(y, Xc)
  r2_e  <- block_r2(y, Xe)
  r2_g  <- block_r2(y, Xg)
  r2_ce <- block_r2(y, cbind(Xc, Xe))
  n     <- sum(complete.cases(cbind(y, Xc, Xe)))

  blk_rows[[tok]] <- tibble(
    token            = tok,
    n                = n,
    R2_crypto_only   = r2_c,
    R2_energy_only   = r2_e,
    R2_green_only    = r2_g,
    R2_crypto_energy = r2_ce,
    incr_crypto      = r2_ce - r2_e,   # crypto adds over energy
    incr_energy      = r2_ce - r2_c,   # energy adds over crypto
    crypto_share     = r2_c / (r2_c + r2_e)
  )
}
beta_blocks <- bind_rows(blk_rows) %>% mutate(across(where(is.numeric), ~round(., 4)))
write_csv(beta_blocks, "Output/identity/beta_blocks.csv")

# ── (C) PC-factor two-factor model (comparable standardised betas) ───────────
pc1 <- function(cols) {
  X <- na.omit(scale(R30[, cols]))
  pr <- prcomp(X, center = FALSE, scale. = FALSE)
  sc <- as.numeric(X %*% pr$rotation[, 1])
  # sign so factor positively correlates with its block mean
  if (cor(sc, rowMeans(X)) < 0) sc <- -sc
  data.frame(Date = R30$Date[complete.cases(scale(R30[, cols]))], f = scale(sc))
}
fC <- pc1(CRYPTO_BLOCK); colnames(fC) <- c("Date", "cryptoF")
fE <- pc1(ENERGY_BLOCK); colnames(fE) <- c("Date", "energyF")

pc_rows <- list()
for (tok in TOKENS_ALL) {
  d <- R30[, c("Date", tok)] %>% inner_join(fC, "Date") %>% inner_join(fE, "Date")
  d$y <- scale(d[[tok]])
  d <- na.omit(d[, c("y", "cryptoF", "energyF")])
  n <- nrow(d); if (n < 50) next
  fit <- lm(y ~ cryptoF + energyF, data = d)
  ct  <- coeftest(fit, vcov. = NeweyWest(fit, lag = nw_lag(n), prewhite = FALSE))
  pc_rows[[tok]] <- tibble(
    token = tok, n = n, R2 = summary(fit)$r.squared,
    beta_crypto = ct["cryptoF", "Estimate"], t_crypto = ct["cryptoF", "t value"],
    beta_energy = ct["energyF", "Estimate"], t_energy = ct["energyF", "t value"],
    beta_ratio  = ct["cryptoF", "Estimate"] / ct["energyF", "Estimate"]
  )
}
beta_pcfactor <- bind_rows(pc_rows) %>% mutate(across(where(is.numeric), ~round(., 4)))
write_csv(beta_pcfactor, "Output/identity/beta_pcfactor.csv")

# ── Console summary ──────────────────────────────────────────────────────────
cat("\n", strrep("=", 70), "\n", sep = "")
cat("(B) BLOCK R^2 DECOMPOSITION — crypto vs energy/green\n")
cat(strrep("=", 70), "\n", sep = "")
print(beta_blocks %>% select(token, n, R2_crypto_only, R2_energy_only,
                             R2_green_only, crypto_share), n = 30)

cat("\n", strrep("=", 70), "\n", sep = "")
cat("(C) PC-FACTOR TWO-FACTOR MODEL — standardised betas\n")
cat(strrep("=", 70), "\n", sep = "")
print(beta_pcfactor, n = 30)

cat("\n", strrep("=", 70), "\n", sep = "")
cat("(A) NAMED-FACTOR BETAS (HAC t in parentheses), key factors\n")
cat(strrep("=", 70), "\n", sep = "")
print(beta_named %>%
        filter(factor %in% c("BTC", "ETH", "CryptoIdx", "Oil", "Carbon", "CleanEnergyETF")) %>%
        mutate(beta = round(beta, 3), t = round(t, 2)) %>%
        select(token, factor, beta, t), n = 60)

message("\nSaved: Output/identity/{beta_named,beta_blocks,beta_pcfactor}.csv")
