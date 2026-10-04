# =============================================================================
# 14_network_summaries.R   (Referee points #32 and #29)
#   #32  Compact numerical summaries behind the network graphs:
#        net transmitters / receivers, strongest bilateral net links, and the
#        token's FROM-block decomposition (token-to-crypto / energy / green).
#   #29  Per-regime (sub-period) connectedness: total connectedness and each
#        token's net directional status, alongside the portfolio contribution.
#
# Inputs : julia/Output/avg_tab_total_{PWR,SNC,EWT}.csv  (25x25 averaged FEVD)
#          julia/Output/C_{PWR,SNC,EWT}.csv              (126 x 1281 dynamic)
#          Data/return_{PWR,SNC,EWT}.RData               (asset order + dates)
#          Output/portfolio/perf_subperiod_GMV.csv       (portfolio dSR)
# Element convention: M[i,j] = share of asset i's FEV explained by j
#                     (spillover FROM column j TO row i, as in the heatmaps).
# Output : Output/identity/net_summary.csv, from_block.csv, regime_conn.csv
# =============================================================================

rm(list = ls())
suppressMessages(library(tidyverse))
dir.create("Output/identity", showWarnings = FALSE, recursive = TRUE)

CRYPTO <- c("BTC","ETH","SOL","AVAX","DPI","IDEX","SPTOP20")
ENERGY <- c("EUA","NEE","ENPH","PBW","PBD","FENY","QCLN","ICLN","B","CL","HH","GC","HG","LIT")
GREEN  <- c("ESGD","ESGU","BGRN")

TKEY <- c(POWR="PWR", SNC="SNC", EWT="EWT")   # file suffixes
asset_order <- function(tok) {
  e <- new.env(); load(sprintf("Data/return_%s.RData", TKEY[tok]), envir = e)
  list(names = setdiff(colnames(e$ret), "Date"), dates = as.Date(e$ret$Date))
}

# ── #32: net directional + FROM-block decomposition ──────────────────────────
net_rows <- list(); from_rows <- list(); link_rows <- list()
for (tok in names(TKEY)) {
  ao <- asset_order(tok); nm <- ao$names
  M  <- as.matrix(read.csv(sprintf("julia/Output/avg_tab_total_%s.csv", TKEY[tok])))
  dimnames(M) <- list(nm, nm)
  diagM <- diag(M)
  FROM <- rowSums(M) - diagM           # received by i (from all others)
  TO   <- colSums(M) - diagM           # transmitted by i (to all others)
  NET  <- TO - FROM
  net_rows[[tok]] <- tibble(system = tok, asset = nm,
                            TO = round(100*TO,2), FROM = round(100*FROM,2),
                            NET = round(100*NET,2))
  # token FROM-block decomposition (share of token FEV from each block)
  tr <- M[tok, ]
  from_rows[[tok]] <- tibble(
    token = tok,
    own        = round(100*diagM[tok],1),
    from_crypto= round(100*sum(tr[CRYPTO]),1),
    from_energy= round(100*sum(tr[ENERGY]),1),
    from_green = round(100*sum(tr[GREEN]),1))
  # strongest net pairwise links involving the token: net_{token,k}=M[k,tok]-M[tok,k]
  netpair <- sapply(nm, function(k) M[k,tok] - M[tok,k]); netpair[tok] <- NA
  top <- sort(abs(netpair), decreasing = TRUE)[1:3]
  link_rows[[tok]] <- tibble(token = tok, partner = names(top),
                             net_pair = round(100*netpair[names(top)],2))
}
net_summary <- bind_rows(net_rows)
from_block  <- bind_rows(from_rows)
write_csv(net_summary, "Output/identity/net_summary.csv")
write_csv(from_block,  "Output/identity/from_block.csv")

# ── #29: per-regime total connectedness + token net status ───────────────────
REGIMES <- list(
  "Crypto turmoil" = c("2022-01-01","2022-06-30"),
  "Energy crisis"  = c("2022-07-01","2022-12-31"),
  "Recovery"       = c("2023-01-01","2026-01-31"))

regime_rows <- list()
for (tok in names(TKEY)) {
  ao <- asset_order(tok)
  Cmat <- as.matrix(read.csv(sprintf("julia/Output/C_%s.csv", TKEY[tok])))
  # C has 1281 cols aligned to dates[-1] (one obs lost to the VAR lag)
  dts <- ao$dates[-1]
  stopifnot(ncol(Cmat) == length(dts))
  total_row <- as.numeric(Cmat[1, ])           # row 1 = total connectedness
  net_token <- as.numeric(Cmat[101 + 1, ])     # NDCT block, token is asset 1
  for (rg in names(REGIMES)) {
    sel <- dts >= as.Date(REGIMES[[rg]][1]) & dts <= as.Date(REGIMES[[rg]][2])
    regime_rows[[paste(tok,rg)]] <- tibble(
      regime = rg, token = tok,
      total_conn = round(mean(total_row[sel]),1),
      token_net  = round(mean(net_token[sel]),2),
      status     = ifelse(mean(net_token[sel])>0,"transmitter","receiver"))
  }
}
regime_conn <- bind_rows(regime_rows)

# merge portfolio dSR per regime/token
psub <- read_csv("Output/portfolio/perf_subperiod_GMV.csv", show_col_types = FALSE) %>%
  mutate(regime = recode(Period,
    "Crypto turmoil"="Crypto turmoil","Energy crisis"="Energy crisis","Recovery"="Recovery")) %>%
  select(regime, token = Token, dSR = Delta_SR)
regime_conn <- regime_conn %>% left_join(psub, by = c("regime","token"))
write_csv(regime_conn, "Output/identity/regime_conn.csv")

# ── Console ──────────────────────────────────────────────────────────────────
cat("\n==== #32 TOKEN FROM-BLOCK DECOMPOSITION (share of token FEV, %) ====\n")
print(from_block)
cat("\n==== #32 TOP NET TRANSMITTERS / RECEIVERS (POWR system) ====\n")
ns <- net_summary %>% filter(system=="POWR") %>% arrange(desc(NET))
cat("Top transmitters:\n"); print(head(ns,5) %>% select(asset,TO,FROM,NET))
cat("Top receivers:\n");    print(tail(ns,5) %>% select(asset,TO,FROM,NET))
cat("\n==== #32 STRONGEST NET PAIRWISE LINKS WITH TOKEN ====\n")
print(bind_rows(link_rows))
cat("\n==== #29 PER-REGIME CONNECTEDNESS + PORTFOLIO ====\n")
print(regime_conn %>% arrange(regime, token), n = 30)
message("\nSaved net_summary.csv, from_block.csv, regime_conn.csv to Output/identity/")
