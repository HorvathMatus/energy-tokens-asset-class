# =============================================================================
# 09_build_returns.R
# Build a single business-day-aligned log-return matrix for all 30 paper assets
# (6 energy tokens + 24 non-token assets), to be reused by the asset-class
# identity analyses (beta decomposition, classification, robustness).
#
# Calendar logic
#   The portfolio analysis (08) uses per-token return files aligned to a
#   1,282-day business-day calendar. We adopt the SAME reference dates as the
#   master calendar, so every new result is consistent with the portfolio
#   numbers already reported. Prices come from Data/Dataset.csv; returns are
#   log-differences over consecutive reference dates (weekends fold into the
#   following business day, the standard convention when mixing 24/7 crypto
#   with 5-day equities/commodities).
#
# Output: Data/returns_full30.RData  ->  data.frame R30 (Date + 30 assets)
# =============================================================================

rm(list = ls())
suppressMessages(library(tidyverse))

# ── Asset groups (paper universe) ────────────────────────────────────────────
TOKENS_ALL <- c("POWR", "EWT", "WOZX", "SNC", "WPR", "SLR")
CRYPTO     <- c("BTC", "ETH", "SOL", "AVAX")
CRYPTO_IX  <- c("SPTOP20", "DPI", "IDEX")
ESG        <- c("ESGU", "ESGD", "BGRN")
COMMOD     <- c("B", "EUA", "CL", "GC", "HG", "HH", "LIT")
ENERGY_EQ  <- c("QCLN", "PBW", "PBD", "ENPH", "NEE", "ICLN", "FENY")
PAPER30    <- c(TOKENS_ALL, CRYPTO, CRYPTO_IX, ESG, COMMOD, ENERGY_EQ)

# Group lookup (used downstream for classification colouring)
GROUP <- tibble(
  asset = PAPER30,
  group = c(rep("Energy token", length(TOKENS_ALL)),
            rep("Cryptocurrency", length(CRYPTO)),
            rep("Crypto index", length(CRYPTO_IX)),
            rep("ESG / green finance", length(ESG)),
            rep("Commodity", length(COMMOD)),
            rep("Energy equity", length(ENERGY_EQ)))
)

# ── 1. Master business-day calendar from the stored return file ──────────────
load("Data/return_PWR.RData")          # provides 'ret'
ref_dates <- as.Date(ret$Date)
non_token_cols <- setdiff(colnames(ret), c("Date", "POWR"))   # 24 non-token
cat("Reference calendar:", length(ref_dates), "business days,",
    as.character(min(ref_dates)), "to", as.character(max(ref_dates)), "\n")

# ── 2. Prices from Dataset.csv, restricted to the reference calendar ─────────
px <- read.csv("Data/Dataset.csv", check.names = FALSE)
px$Date <- as.Date(px$Date)
px <- px[match(ref_dates, px$Date), c("Date", PAPER30)]   # align to ref dates
stopifnot(all(px$Date == ref_dates))

# ── 3. Log returns over consecutive reference dates ──────────────────────────
logret <- function(p) c(NA, diff(log(p)))
R30 <- px
R30[PAPER30] <- lapply(px[PAPER30], logret)

# Drop the first row (NA from differencing)
R30 <- R30[-1, ]
rownames(R30) <- NULL
cat("Return matrix:", nrow(R30), "rows x", length(PAPER30), "assets\n")
cat("Complete rows:", sum(complete.cases(R30[PAPER30])), "\n")

# ── 4. Verify reconstruction against stored return files ─────────────────────
verify_one <- function(file, col) {
  e <- new.env(); load(file, envir = e); s <- e$ret
  s$Date <- as.Date(s$Date)
  m <- merge(R30[, c("Date", col)], s[, c("Date", col)],
             by = "Date", suffixes = c("_new", "_stored"))
  r <- cor(m[[paste0(col, "_new")]], m[[paste0(col, "_stored")]],
           use = "complete.obs")
  cat(sprintf("  %-5s correlation new vs stored: %.4f (n=%d)\n", col, r, nrow(m)))
}
cat("Verification (reconstructed vs stored returns):\n")
verify_one("Data/return_PWR.RData", "POWR")
verify_one("Data/return_SNC.RData", "SNC")
verify_one("Data/return_EWT.RData", "EWT")

# ── 5. Save ──────────────────────────────────────────────────────────────────
attr(R30, "groups") <- GROUP
save(R30, GROUP, PAPER30,
     TOKENS_ALL, CRYPTO, CRYPTO_IX, ESG, COMMOD, ENERGY_EQ,
     file = "Data/returns_full30.RData")
cat("Saved Data/returns_full30.RData\n")
