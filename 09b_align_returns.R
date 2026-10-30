# =============================================================================
# 09b_align_returns.R   (JCM)
# Aligns the exchange-traded series with the token day.
#
#   Token and cryptocurrency prices (CoinGecko) are snapshots at 00:00 UTC, so
#   the return dated t covers the UTC calendar day t-1. Exchange-traded series
#   close between about 16:00 and 22:00 UTC, so their return dated t covers the
#   session that ends on day t. The session that closes within the token's day
#   is therefore the one dated t-1. We lag every exchange-traded column by one
#   business day. The cross-correlations of 22_jcm_referee2.R confirm the shift.
#
# Output: Data/returns_full30_aligned.RData  ->  R30 (same layout as 09) and EXCH
# =============================================================================

rm(list = ls())
load("Data/returns_full30.RData")
EXCH <- c("SPTOP20", ESG, COMMOD, ENERGY_EQ)       # S&P, Investing.com, and FirstRate series
for (a in EXCH) R30[[a]] <- c(NA, head(R30[[a]], -1))
save(R30, GROUP, PAPER30, TOKENS_ALL, CRYPTO, CRYPTO_IX, ESG, COMMOD, ENERGY_EQ, EXCH,
     file = "Data/returns_full30_aligned.RData")
cat("Aligned", length(EXCH), "exchange-traded columns:", paste(EXCH, collapse = " "), "\n")
