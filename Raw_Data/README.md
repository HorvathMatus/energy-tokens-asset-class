# Raw data

The raw data are not distributed with this repository. FirstRate Data is licensed, and the remaining series can be downloaded from their public sources. Place the files as follows before running `01_Read_data.R`. `00_download_public_data.sh` downloads the public files that have a stable download link.

## Daily close prices, `Raw_Data/<TICKER>.csv`

Two columns: `Date` and the price, with the ticker as the column name. Dates may carry a time stamp, for example `2020-12-07 00:00:00 UTC`. CoinGecko values are snapshots at 00:00 UTC, so the value dated t is the close of the UTC day t-1.

| File | Asset | Source |
|---|---|---|
| `POWR.csv`, `EWT.csv`, `WOZX.csv`, `SNC.csv`, `WPR.csv`, `SLR.csv` | Energy tokens | CoinGecko |
| `BTC.csv`, `ETH.csv`, `SOL.csv`, `AVAX.csv`, `IDEX.csv` | Cryptocurrencies | CoinGecko |
| `DPI.csv` | DeFi Pulse Index token | CoinGecko |
| `SPTOP20.csv` | S&P Cryptocurrency Top 20 Index | S&P Dow Jones Indices |
| `EUA.csv` | EU Allowance, continuous year futures | Investing.com (checked against EEX primary auction and spot prices) |

## Daily bars, `Raw_Data/FRD/<TICKER>.txt` and `Raw_Data/FRD_unadjusted/`

FirstRate Data files (licensed) with columns `DateTime, Open, High, Low, Close, Volume` and optionally `OpenInterest`, with or without a header row. Tickers in `FRD/`: `B`, `CL`, `GC`, `HG`, `HH`, `LIT`, `QCLN`, `PBW`, `PBD`, `ENPH`, `NEE`, `ICLN`, `FENY`, `ESGU`, `ESGD`, `BGRN`. The futures files in `FRD/` are back-adjusted continuous contracts. `FRD_unadjusted/<TICKER>_full_1day_continuous_UNadjusted.txt` holds the unadjusted continuous front-month contracts for `B`, `CL`, `HH`, `GC`, and `HG`, which `01b_futures_roll_adjust.R` combines with the adjusted files to compute the returns of a rolled position.

## Public inputs downloaded by `00_download_public_data.sh`

| File | Content | Source |
|---|---|---|
| `CoinGecko_volume/<TICKER>.csv` | daily price, market capitalisation, and dollar volume, dated by UTC day | CoinGecko |
| `DTB3.csv` | 3-month US Treasury bill rate | FRED, Federal Reserve Bank of St. Louis |
| `Electricity/energy_charts/<ZONE>_<YEAR>.json` | hourly and quarter-hourly day-ahead prices for DE-LU, SI, and IT-North | Energy-Charts, Fraunhofer ISE (CC BY 4.0) |
| `Electricity/aemo/PRICE_AND_DEMAND_<YYYYMM>_NSW1.csv` | five-minute and half-hourly spot prices for New South Wales | Australian Energy Market Operator |
| `GoldTokens/PAXG_binance.csv` | daily PAXG/USDT candles, UTC days | Binance |
| `GoldTokens/XAUT_bitfinex.csv` | daily XAUT/USD candles, UTC days | Bitfinex |

The sample runs from 7 December 2020 to 31 January 2026.
