#!/usr/bin/env bash
# Downloads the publicly available inputs:
#   - CoinGecko daily price, market capitalisation and volume exports for the
#     energy tokens and the major cryptocurrencies (Raw_Data/CoinGecko_volume/)
#   - the 3-month US Treasury bill rate DTB3 from FRED (Raw_Data/DTB3.csv)
#   - day-ahead electricity prices for Germany-Luxembourg, Slovenia, and northern
#     Italy from Energy-Charts (Fraunhofer ISE, CC BY 4.0) and NSW spot prices
#     from AEMO (Raw_Data/Electricity/)
#   - daily candles of the gold-backed tokens, PAXG/USDT from Binance and
#     XAUT/USD from Bitfinex (Raw_Data/GoldTokens/)
# CoinGecko's export endpoint is not an official API and may refuse automated
# requests (HTTP 403). In that case download the same CSV exports manually from
# the CoinGecko coin pages. Data are subject to the providers' terms of use.
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

# ── Electricity ──────────────────────────────────────────────────────────────
mkdir -p Raw_Data/Electricity/energy_charts Raw_Data/Electricity/aemo
for z in DE-LU SI IT-North; do
  for y in 2020 2021 2022 2023 2024 2025 2026; do
    s="$y-01-01"; e="$y-12-31"; [ "$y" = 2020 ] && s="2020-11-01"; [ "$y" = 2026 ] && e="2026-02-28"
    curl -fsS -m 120 -o "Raw_Data/Electricity/energy_charts/${z}_${y}.json" \
      "https://api.energy-charts.info/price?bzn=${z}&start=${s}&end=${e}" \
      && echo "Energy-Charts ${z} ${y}" || echo "Energy-Charts ${z} ${y} failed, run again later"
    sleep 20   # the API limits the request rate
  done
done
for y in 2020 2021 2022 2023 2024 2025 2026; do
  for m in 01 02 03 04 05 06 07 08 09 10 11 12; do
    ym="$y$m"; [ "$ym" -lt 202011 ] && continue; [ "$ym" -gt 202602 ] && continue
    curl -fsSL -m 120 -A "$UA" -o "Raw_Data/Electricity/aemo/PRICE_AND_DEMAND_${ym}_NSW1.csv" \
      "https://www.aemo.com.au/aemo/data/nem/priceanddemand/PRICE_AND_DEMAND_${ym}_NSW1.csv"
    sleep 1
  done
done
echo "AEMO NSW1 saved"

# ── Gold-backed tokens ───────────────────────────────────────────────────────
mkdir -p Raw_Data/GoldTokens
python3 - <<'PY'
import json, urllib.request, time, csv, datetime as dt
start = int(dt.datetime(2020, 11, 1, tzinfo=dt.timezone.utc).timestamp() * 1000)
end = int(dt.datetime(2026, 2, 28, tzinfo=dt.timezone.utc).timestamp() * 1000)
day = lambda ms: dt.datetime.fromtimestamp(ms / 1000, dt.timezone.utc).date()
# PAXG: Binance daily klines, one row per UTC day
rows, t = [], start
while t < end:
    url = f"https://api.binance.com/api/v3/klines?symbol=PAXGUSDT&interval=1d&startTime={t}&endTime={end}&limit=1000"
    k = json.load(urllib.request.urlopen(url, timeout=60))
    if not k:
        break
    rows += k; t = k[-1][0] + 86400000; time.sleep(0.5)
with open("Raw_Data/GoldTokens/PAXG_binance.csv", "w", newline="") as f:
    w = csv.writer(f); w.writerow(["date", "open", "high", "low", "close", "volume_quote_usd"])
    for r in rows:
        w.writerow([day(r[0]), r[1], r[2], r[3], r[4], r[7]])
# XAUT: Bitfinex daily candles [MTS, OPEN, CLOSE, HIGH, LOW, VOLUME]
url = f"https://api-pub.bitfinex.com/v2/candles/trade:1D:tXAUT:USD/hist?start={start}&end={end}&limit=10000&sort=1"
c = json.load(urllib.request.urlopen(url, timeout=60))
with open("Raw_Data/GoldTokens/XAUT_bitfinex.csv", "w", newline="") as f:
    w = csv.writer(f); w.writerow(["date", "open", "high", "low", "close", "volume_base"])
    for r in c:
        w.writerow([day(r[0]), r[1], r[3], r[4], r[2], r[5]])
print("Gold-backed tokens saved:", len(rows), "PAXG days,", len(c), "XAUT days")
PY
