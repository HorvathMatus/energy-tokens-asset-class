# Are Energy Tokens Really Energy Assets? Evidence from Cross-Market Spillovers

Replication code for

> Horváth, M. and Ben Amor, S. (2026). *Are Energy Tokens Really Energy Assets? Evidence from Cross-Market Spillovers.* Working paper.

The paper asks whether energy tokens (Power Ledger, Energy Web Token, Efforce, SunContract, WePower, SolarCoin) behave like energy assets or like cryptocurrencies. It combines the locally stationary TVP-VAR frequency connectedness of Baruník and Ellington (2024) with factor-model, clustering, tail-dependence, spanning, and portfolio tests on 30 assets, December 2020 to January 2026.

## Repository structure

```
00_download_public_data.sh   CoinGecko volume exports and the FRED T-bill rate
01_Read_data.R               builds the dataset and the TVP-VAR input files
03_ to 07_*.R                connectedness figures, heatmaps, network graphs
08_portfolio_analysis.R      EW, GMV, MVE portfolios (total, short and long horizon)
09_build_returns.R           30-asset business-day return matrix
10_ to 17_*.R                asset-class identity, robustness, liquidity, frictions, descriptives
Functions/Read_data.R        readers for the raw price files
julia/                       TVP-VAR estimation (notebooks and scripts)
Raw_Data/                    raw inputs, not distributed (see Raw_Data/README.md)
Data/, julia/Data/           intermediate files written by 01_Read_data.R
Output/, julia/Output/       results; the summary tables reported in the paper are included
```

## Software

- **R 4.4.2** with data.table 1.16.4, dplyr 1.1.4, ggplot2 4.0.2, ggraph 2.2.2, ggrepel 0.9.6, gridExtra 2.3, igraph 2.2.2, lmtest 0.9.40, lubridate 1.9.4, mFilter 0.1.5, moments 0.14.1, patchwork 1.3.2, purrr 1.0.2, quadprog 1.5.8, quantreg 5.99.1, readr 2.1.5, sandwich 3.1.1, scales 1.4.0, stringr 1.5.1, tidygraph 1.3.1, tidyr 1.3.1, tidyverse 2.0.0, zoo 1.8.12.
- **Julia 1.11 or later** with the packages listed in `julia/Project.toml` (CSV, DataFrames, Distributions, Plots, IJulia).
- The TVP-VAR connectedness functions of Ellington, Hanus and Baruník are taken from [barunik/DynamicNets.jl](https://github.com/barunik/DynamicNets.jl). They are not redistributed here. `julia/get_dynamicnets.sh` downloads the exact version used in the paper.

## Data

Price data come from CoinGecko and CoinMarketCap (energy tokens, cryptocurrencies, crypto indices), LSEG Workspace (EU carbon allowance, S&P Cryptocurrency Top 20), and FirstRate Data (commodities, ETFs, equities). LSEG and FirstRate Data are licensed and cannot be shared, so the raw and intermediate data are not part of this repository. `Raw_Data/README.md` lists every input file and its format. The CoinGecko volume data and the Treasury bill rate are public and can be downloaded with `00_download_public_data.sh`.

## Replication

Run all commands from the repository root.

1. Download the public inputs and place the licensed price files as described in `Raw_Data/README.md`.
   ```
   ./00_download_public_data.sh
   ```
2. Build the dataset, the return files, and the TVP-VAR inputs.
   ```
   Rscript 01_Read_data.R
   ```
3. Estimate the TVP-VAR connectedness in Julia.
   ```
   cd julia
   ./get_dynamicnets.sh
   julia --project=. -e 'using Pkg; Pkg.instantiate()'
   ```
   Then run the notebooks: `02_tokens.ipynb` (six-token system), `02_julia.ipynb` (25-asset system centred on one token, run once each with `Token_name` set to `PWR`, `SNC`, `EWT`, `WOZX`, and `SLR`), and `02_WPR.ipynb` (WePower system). Afterwards run the robustness scripts:
   ```
   julia --project=. robust_tvpvar.jl
   julia --project=. regime_pairwise.jl
   julia --project=. robust_irfa.jl
   cd ..
   ```
   One full estimation of a 25-asset system with 100 posterior draws takes about 15 to 20 minutes on a recent laptop.
4. Run the R scripts in numerical order, `03_graphs.R` to `17_descriptive_tables.R`.

## Where each result comes from

| Paper | Script | Output |
|---|---|---|
| Table 1 | `17_descriptive_tables.R` | `Output/descriptive/table1_data_quality.csv` |
| Table 2 | `15_liquidity_identity.R` | `Output/liquidity/liquidity_table.csv`, `liquidity_screen.csv` |
| Table 3 | `14_network_summaries.R` | `Output/identity/from_block.csv` |
| Table 4 | `10_beta_decomposition.R` | `Output/identity/beta_blocks.csv`, `beta_pcfactor.csv`, `beta_named.csv` |
| Table 5 | `13_tail_risk.R`, `11_classification.R` | `Output/identity/tail_risk.csv`, `dcor_tokens.csv` |
| Table 6 | `15_liquidity_identity.R` | `Output/identity/thin_trading.csv`, `horse_race.csv` |
| Table 7 | `julia/robust_irfa.jl` | `julia/Output/irfa_blocks.csv` |
| Table 8 | `julia/robust_tvpvar.jl` | `julia/Output/robust_summary.csv` |
| Tables 9 to 11 | `08_portfolio_analysis.R` | `Output/portfolio/perf_total_all.csv`, `perf_horizon_GMV.csv`, `perf_subperiod_GMV.csv` |
| Table 12 | `14_network_summaries.R` | `Output/identity/regime_conn.csv` |
| Table 13 | `julia/regime_pairwise.jl` | `julia/Output/regime_pairwise.csv` |
| Table 14 | `12_portfolio_robustness.R`, `16_portfolio_frictions.R` | `Output/portfolio/robust_*.csv`, `frictions.csv` |
| Table 15 | `16_portfolio_frictions.R` | `Output/portfolio/frictions.csv`, `frictions_costgrid.csv` |
| Table 16 | `17_descriptive_tables.R` | `Output/descriptive/table16_descriptive.csv` |
| Table 17 | `julia/robust_irfa.jl` | `julia/Output/irfa_draws.csv` |
| Table 18 | `15_liquidity_identity.R` | `Output/identity/spanning.csv` |
| Figures 2, 18, 19 | `04_graphs_frequencies.R` | `Output/Frequencies/NET_grid_with_events_*.png` |
| Figures 3, 4, 20 to 22 | `06_graphs_frequencies_tokens.R` | `Output/Frequencies/Total_*.png`, `TO_FROM_NET_*_*.png` |
| Figures 5, 6, 14 to 17 | `07_heatmaps.R` | `Output/Frequencies/heatmap_total_*.png`, `network_total_circular_*.png` |
| Figures 7, 8 | `11_classification.R` | `Output/figures/identity/` |
| Figures 9 to 11 | `08_portfolio_analysis.R` | `Output/figures/portfolio/` |
| Figures 12, 13 | `17_descriptive_tables.R` | `Output/figures/prices/`, `Output/figures/returns/` |

`03_graphs.R` and `05_graphs_tokens.R` produce total-band versions of the connectedness figures that are not shown in the paper. Figure 1 is drawn in the manuscript itself.

## Notes

- Sharpe-ratio differences are tested with the HAC test of Ledoit and Wolf (2008).
- The horizon-specific covariance matrices of the portfolio exercise are estimated with the Baxter and King (1999) band-pass filter (`mFilter::bkfilter`) within each rolling 252-day window.
- Results can differ slightly from the paper if the publicly downloadable series have been revised by the data providers.

## Citation

```bibtex
@unpublished{horvath2026energytokens,
  title  = {Are Energy Tokens Really Energy Assets? Evidence from Cross-Market Spillovers},
  author = {Horv{\'a}th, Mat{\'u}{\v{s}} and Ben Amor, Souhir},
  year   = {2026},
  note   = {Working paper}
}
```

## Licence

The code in this repository is released under the MIT licence (see `LICENSE`). `DynamicNets_functions.jl`, which is downloaded separately, remains the work of its authors.
