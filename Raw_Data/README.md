# Raw data

The raw data are not distributed with this repository. Some series are licensed (LSEG Workspace, FirstRate Data), and the remaining ones can be downloaded from their public sources. Place the files as follows before running `01_Read_data.R`.

## Daily close prices, `Raw_Data/<TICKER>.csv`

Two columns: `Date` and the price, with the ticker as the column name. Dates may carry a time stamp, for example `2020-12-07 00:00:00 UTC`.

| File | Asset | Source |
|---|---|---|
| `POWR.csv`, `EWT.csv`, `WOZX.csv`, `SNC.csv`, `WPR.csv`, `SLR.csv` | Energy tokens | CoinGecko, CoinMarketCap |
| `BTC.csv`, `ETH.csv`, `SOL.csv`, `AVAX.csv` | Cryptocurrencies | CoinGecko |
| `DPI.csv`, `IDEX.csv` | Crypto indices | CoinGecko |
| `SPTOP20.csv` | S&P Cryptocurrency Top 20 Index | LSEG Workspace (licensed) |
| `EUA.csv` | EU Emission Allowance futures | LSEG Workspace (licensed) |

## Daily bars, `Raw_Data/FRD/<TICKER>.txt`

FirstRate Data files (licensed) with columns `DateTime, Open, High, Low, Close, Volume` and optionally `OpenInterest`, with or without a header row. Tickers: `B`, `CL`, `GC`, `HG`, `HH`, `LIT`, `QCLN`, `PBW`, `PBD`, `ENPH`, `NEE`, `ICLN`, `FENY`, `ESGU`, `ESGD`, `BGRN`.

## Public inputs downloaded by `00_download_public_data.sh`

| File | Content | Source |
|---|---|---|
| `CoinGecko_volume/<TICKER>.csv` | daily price, market capitalisation, and dollar volume | CoinGecko |
| `DTB3.csv` | 3-month US Treasury bill rate | FRED, Federal Reserve Bank of St. Louis |

The sample runs from 7 December 2020 to 31 January 2026.
