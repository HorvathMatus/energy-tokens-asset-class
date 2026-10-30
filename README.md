# Does Tokenization Deliver Commodity Exposure? Evidence from Energy and Gold-Backed Tokens

Replication code for

> Horváth, M. and Ben Amor, S. (2026). *Does Tokenization Deliver Commodity Exposure? Evidence from Energy and Gold-Backed Tokens.* Working paper.

An earlier version of the paper was titled *Are Energy Tokens Really Energy Assets? Evidence from Cross-Market Spillovers*. The scripts of that version are kept in the repository and marked below.

The paper asks whether six energy tokens (Power Ledger, Energy Web Token, Efforce, SunContract, WePower, SolarCoin) carry exposure to electricity and other energy commodity markets, December 2020 to January 2026. It measures their exposure to wholesale electricity prices in Germany-Luxembourg, Slovenia, northern Italy, and New South Wales, to energy and metal futures, the EU carbon allowance, energy equities, and cryptocurrencies, with the trading sessions of tokens and exchange-traded assets aligned. Two gold-backed tokens (PAX Gold, Tether Gold) serve as a positive control. The tools are block regressions with bootstrap bounds, tail-dependence measures, the locally stationary TVP-VAR frequency connectedness of Baruník and Ellington (2024), and out-of-sample portfolio tests.

## Timing of the data

Token and cryptocurrency prices from CoinGecko are snapshots at 00:00 UTC, so the return dated t covers the UTC day t-1. Futures, equities, and ETFs close between about 16:00 and 23:00 UTC on day t. `09b_align_returns.R` and `18_electricity_gold.R` therefore lag every exchange-traded series, and the New South Wales electricity price, by one business day. European day-ahead prices for delivery day t are published on day t-1 and are not shifted. The Julia inputs `julia/Data/*_shift.txt` written by `18_electricity_gold.R` carry the same alignment. `22_jcm_referee2.R` documents it with cross-correlations at different offsets.

## Repository structure

```
00_download_public_data.sh   public inputs: CoinGecko, FRED, Energy-Charts, AEMO, Binance, Bitfinex
01_Read_data.R               builds the dataset and the 25-asset TVP-VAR input files
01b_futures_roll_adjust.R    returns of rolled futures positions from adjusted and unadjusted contracts
08_portfolio_analysis.R      EW, GMV, MVE portfolios (total, short and long horizon)
09_build_returns.R           30-asset business-day return matrix
09b_align_returns.R          the same matrix with exchange-traded series aligned to the token day
11_classification.R          PCA, clustering, Louvain communities, distance correlation
12_portfolio_robustness.R    portfolio robustness (costs, caps, shrinkage, no-crypto benchmark)
15_liquidity_identity.R      liquidity, thin-trading horse race, spanning tests
16_portfolio_frictions.R     trading costs, market impact, participation, custody risk
17_descriptive_tables.R      data-quality and descriptive tables
18_electricity_gold.R        electricity and gold-token series, block regressions, aligned Julia inputs
19_jcm_tables.R              exposure table and the commodity-futures portfolio
20_jcm_network_figures.R     figures of the 29-asset networks
21_rolling_exposure.R        rolling exposure, selected electricity benchmarks, volume test
22_jcm_referee2.R            timing audit, tail dependence, band composition, credible net spillovers
23_portfolio_timing.R        portfolios with and without the one-day implementation gap
24_bootstrap_coverage.R      bootstrap bounds on the commodity gain and their Monte Carlo coverage
Functions/Read_data.R        readers for the raw price files
julia/                       TVP-VAR estimation (scripts and notebooks)
Raw_Data/                    raw inputs, not distributed (see Raw_Data/README.md)
Data/, julia/Data/           intermediate files written by the R scripts
Output/, julia/Output/       results, including the summary tables reported in the paper
```

Scripts of the earlier version that the current paper does not use: `03_graphs.R`, `05_graphs_tokens.R`, `06_graphs_frequencies_tokens.R`, `07_heatmaps.R`, `10_beta_decomposition.R`, `13_tail_risk.R`, `14_network_summaries.R`, `julia/robust_tvpvar.jl`, `julia/regime_pairwise.jl`, and `julia/robust_irfa.jl`.

## Software

- **R 4.4.2** with data.table 1.16.4, dplyr 1.1.4, ggplot2 4.0.2, ggnewscale 0.5.2, ggraph 2.2.2, ggrepel 0.9.6, gridExtra 2.3, igraph 2.2.2, jsonlite 1.8.9, lmtest 0.9.40, lubridate 1.9.4, mFilter 0.1.5, moments 0.14.1, patchwork 1.3.2, purrr 1.0.2, quadprog 1.5.8, quantreg 5.99.1, readr 2.1.5, sandwich 3.1.1, scales 1.4.0, stringr 1.5.1, tidygraph 1.3.1, tidyr 1.3.1, tidyverse 2.0.0, zoo 1.8.12. `24_bootstrap_coverage.R` uses the base package `parallel`.
- **Julia 1.11 or later** with the packages listed in `julia/Project.toml`.
- The TVP-VAR connectedness functions of Ellington, Hanus and Baruník are taken from [barunik/DynamicNets.jl](https://github.com/barunik/DynamicNets.jl). They are not redistributed here. `julia/get_dynamicnets.sh` downloads the exact version used in the paper.

## Data

Token, cryptocurrency, and DeFi Pulse Index prices come from CoinGecko, the S&P Cryptocurrency Top 20 Index from S&P Dow Jones Indices, and the EU Allowance from the continuous year-futures series of Investing.com, which was checked against EEX primary auction and spot prices. Futures, equities, and ETFs come from FirstRate Data, which is licensed and cannot be shared. Electricity prices come from Energy-Charts (Fraunhofer ISE, CC BY 4.0) and the Australian Energy Market Operator, the gold-backed tokens from Binance and Bitfinex, and the Treasury bill rate from FRED. The raw and intermediate data are not part of this repository. `Raw_Data/README.md` lists every input file and its format, and `00_download_public_data.sh` downloads the public files.

## Replication

Run all commands from the repository root.

1. Download the public inputs and place the other price files as described in `Raw_Data/README.md`.
   ```
   ./00_download_public_data.sh
   ```
2. Build the dataset, the roll-adjusted futures returns, and the return matrices.
   ```
   Rscript 01_Read_data.R
   Rscript 01b_futures_roll_adjust.R
   Rscript 09_build_returns.R
   Rscript 09b_align_returns.R
   Rscript 18_electricity_gold.R
   ```
3. Estimate the TVP-VAR connectedness in Julia.
   ```
   cd julia
   ./get_dynamicnets.sh
   julia --project=. -e 'using Pkg; Pkg.instantiate()'
   julia --project=. network_jcm.jl PWR      # also SNC and EWT, about 50 minutes each
   julia --project=. robust_jcm.jl
   julia --project=. robust_jcm_shift.jl
   julia --project=. robust_small_jcm.jl
   julia --project=. robust_tvpvar_jcm.jl
   julia --project=. robust_spec_jcm.jl
   julia --project=. robust_irfa_shift.jl
   cd ..
   ```
   The six-token system of Figure 2 is estimated in `julia/02_tokens.ipynb`.
4. Run the R scripts that produce the tables and figures.
   ```
   Rscript 04_graphs_frequencies.R
   Rscript 08_portfolio_analysis.R
   Rscript 11_classification.R
   Rscript 12_portfolio_robustness.R
   Rscript 15_liquidity_identity.R
   Rscript 16_portfolio_frictions.R
   Rscript 17_descriptive_tables.R
   Rscript 19_jcm_tables.R
   Rscript 20_jcm_network_figures.R
   Rscript 21_rolling_exposure.R
   Rscript 22_jcm_referee2.R
   Rscript 23_portfolio_timing.R
   Rscript 24_bootstrap_coverage.R
   ```
   `24_bootstrap_coverage.R` runs a Monte Carlo experiment that takes about 15 minutes on six cores. Set `CORES`, `NREP`, and `BMC` in the environment to change its size.

## Where each result comes from

| Paper | Script | Output |
|---|---|---|
| Table 1 | issuers' documentation, see the table notes | |
| Table 2 | `15_liquidity_identity.R` | `Output/liquidity/liquidity_table.csv`, `liquidity_screen.csv` |
| Table 3 | `19_jcm_tables.R`, `24_bootstrap_coverage.R` | `Output/jcm/identity_table.csv`, `bootstrap_bounds.csv` |
| Figure 1, Table 4 | `21_rolling_exposure.R` | `Output/jcm/rolling_exposure.csv`, `home_market.csv`, `volume_channel.csv` |
| Monthly and electricity increments (Section 5.3) | `18_electricity_gold.R` | `Output/jcm/horse_race_electricity.csv` |
| Table 5 | `22_jcm_referee2.R` | `Output/jcm/tail_aligned.csv` |
| Table 6 | `15_liquidity_identity.R` | `Output/identity/thin_trading.csv` |
| Figure 2 | `julia/02_tokens.ipynb`, `04_graphs_frequencies.R` | `Output/Frequencies/NET_grid_with_events_short.png` |
| Figures 3 to 5, 8 to 11 | `julia/network_jcm.jl`, `20_jcm_network_figures.R` | `Output/figures/results_jcm/` |
| Band composition and credible net spillovers (Section 5.6) | `22_jcm_referee2.R` | `Output/jcm/network_bands.csv`, `net_credible.csv` |
| Table 7 | `julia/robust_jcm_shift.jl` (Panel A), `julia/robust_jcm.jl` (Panel B) | `julia/Output/jcm_blocks_shift.csv`, `jcm_blocks.csv` |
| Table 8 | `julia/robust_small_jcm.jl` | `julia/Output/jcm_small_systems.csv` |
| Figures 6, 7 | `11_classification.R` | `Output/figures/identity/` |
| Table 9 | `19_jcm_tables.R` | `Output/jcm/commodity_portfolio.csv` |
| Table 10 | `08_portfolio_analysis.R` | `Output/portfolio/perf_total_all.csv`, `perf_short_all.csv`, `perf_long_all.csv` |
| Table 11 | `16_portfolio_frictions.R`, `22_jcm_referee2.R` | `Output/portfolio/frictions.csv`, `Output/jcm/participation.csv` |
| Tables 12 to 14 | `17_descriptive_tables.R` | `Output/descriptive/` |
| Table 16 | `22_jcm_referee2.R` | `Output/jcm/timing_xcorr.csv` |
| Tables 17, 18 | `24_bootstrap_coverage.R` | `Output/jcm/bootstrap_bounds.csv`, `bootstrap_coverage.csv` |
| Tables 19, 21 | `julia/robust_irfa_shift.jl` | `julia/Output/jcm_irfa_blocks_shift.csv`, `jcm_irfa_draws_shift.csv` |
| Table 20 | `julia/robust_tvpvar_jcm.jl`, `julia/robust_spec_jcm.jl` | `julia/Output/jcm_tvpvar_robust.csv`, `jcm_spec_robust.csv` |
| Table 22 | `15_liquidity_identity.R` | `Output/identity/spanning.csv` |
| Table 23 | `12_portfolio_robustness.R`, `16_portfolio_frictions.R`, `23_portfolio_timing.R` | `Output/portfolio/robust_*.csv`, `frictions.csv`, `Output/jcm/portfolio_timing.csv` |

Table 15 documents the timing conventions of the data sources and is not computed.

## Notes

- Portfolio weights are estimated on returns up to day t-2 and held from day t, so the strategies are implementable when tokens are priced at 00:00 UTC. `23_portfolio_timing.R` compares them with weights estimated up to day t-1.
- Sharpe-ratio differences are tested with the HAC test of Ledoit and Wolf (2008).
- The horizon-specific covariance matrices of the portfolio exercise are estimated with the Baxter and King (1999) band-pass filter (`mFilter::bkfilter`) within each rolling 252-day window.
- Results can differ slightly from the paper if the publicly downloadable series have been revised by the data providers.

## Citation

```bibtex
@unpublished{horvath2026energytokens,
  title  = {Does Tokenization Deliver Commodity Exposure? Evidence from Energy and Gold-Backed Tokens},
  author = {Horv{\'a}th, Mat{\'u}{\v{s}} and Ben Amor, Souhir},
  year   = {2026},
  note   = {Working paper}
}
```

## Licence

The code in this repository is released under the MIT licence (see `LICENSE`). `DynamicNets_functions.jl`, which is downloaded separately, remains the work of its authors.
