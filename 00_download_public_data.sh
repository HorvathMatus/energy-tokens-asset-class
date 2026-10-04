#!/usr/bin/env bash
# Downloads the publicly available inputs used by 15_liquidity_identity.R and
# 16_portfolio_frictions.R:
#   - CoinGecko daily price, market capitalisation and volume exports for the
#     energy tokens and the major cryptocurrencies (Raw_Data/CoinGecko_volume/)
#   - the 3-month US Treasury bill rate DTB3 from FRED (Raw_Data/DTB3.csv)
# CoinGecko's export endpoint is not an official API and may change. Data are
# subject to CoinGecko's terms of use.
set -euo pipefail
cd "$(dirname "$0")"
mkdir -p Raw_Data/CoinGecko_volume
UA="Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/126 Safari/537.36"
for pair in POWR:power-ledger EWT:energy-web-token WOZX:efforce SNC:suncontract WPR:wepower \
            BTC:bitcoin ETH:ethereum SOL:solana AVAX:avalanche-2; do
  t=${pair%%:*}; id=${pair#*:}
  curl -fsSL -A "$UA" -o "Raw_Data/CoinGecko_volume/${t}.csv" \
    "https://www.coingecko.com/price_charts/export/${id}/usd.csv"
  echo "CoinGecko ${t}: $(($(wc -l < Raw_Data/CoinGecko_volume/${t}.csv) - 1)) days"
  sleep 3
done
curl -fsSL -o Raw_Data/DTB3.csv \
  "https://fred.stlouisfed.org/graph/fredgraph.csv?id=DTB3&cosd=2020-11-01&coed=2026-02-28"
echo "FRED DTB3 saved"
