# =============================================================================
# 17_descriptive_tables.R
# Data description: asset universe and data-quality screens (Table 1),
# descriptive statistics for prices and log-returns (Table 16), and the
# price and log-return panels of all 30 assets (Figures 12 and 13).
#
#   Table 1  uses the business-day return matrix of 09_build_returns.R.
#   Table 16 uses each asset's own trading days: prices are the observed closes
#            in the sample window and returns are log differences between
#            consecutive observed closes. Skewness and kurtosis are m3/s^3 and
#            m4/s^4 with the sample standard deviation s (kurtosis not in excess).
#
# Outputs: Output/descriptive/table1_data_quality.csv
#          Output/descriptive/table16_descriptive.csv
#          Output/figures/prices/<ASSET>.png, Output/figures/returns/<ASSET>.png
# =============================================================================

rm(list = ls())
suppressMessages(library(tidyverse))

load("Data/returns_full30.RData")       # R30, GROUP, PAPER30 (09_build_returns.R)
dir.create("Output/descriptive", showWarnings = FALSE, recursive = TRUE)
dir.create("Output/figures/prices", showWarnings = FALSE, recursive = TRUE)
dir.create("Output/figures/returns", showWarnings = FALSE, recursive = TRUE)

START <- as.Date("2020-12-07"); END <- as.Date("2026-01-31")

NAMES <- c(POWR = "Power Ledger", EWT = "Energy Web Token", WOZX = "Efforce", SNC = "SunContract",
           WPR = "WePower", SLR = "SolarCoin", BTC = "Bitcoin", ETH = "Ethereum", SOL = "Solana",
           AVAX = "Avalanche", SPTOP20 = "S&P Crypto Top 20", DPI = "DeFi Pulse Index", IDEX = "IDEX",
           ESGU = "iShares ESG MSCI USA", ESGD = "iShares ESG MSCI EAFE", BGRN = "iShares USD Green Bond",
           B = "Brent crude oil", EUA = "EU Emission Allowance", CL = "WTI crude oil", GC = "Gold",
           HG = "Copper", HH = "Henry Hub gas", LIT = "Global X Lithium & Battery",
           QCLN = "First Trust NASDAQ Clean Edge", PBW = "Invesco WilderHill Clean En.",
           PBD = "Invesco Global Clean En.", ENPH = "Enphase Energy", NEE = "NextEra Energy",
           ICLN = "iShares Global Clean Energy", FENY = "Fidelity MSCI Energy")

# ── Table 1: asset universe and data-quality screens ─────────────────────────
table1 <- tibble(Ticker = PAPER30) %>%
  left_join(GROUP, by = c("Ticker" = "asset")) %>%
  mutate(Name = NAMES[Ticker],
         Obs = map_int(Ticker, ~ sum(!is.na(R30[[.x]]))),
         Missing_pct = map_dbl(Ticker, ~ round(100 * mean(is.na(R30[[.x]])), 1)),
         ZeroRet_pct = map_dbl(Ticker, ~ round(100 * mean(R30[[.x]] == 0, na.rm = TRUE), 1))) %>%
  select(Ticker, Name, Class = group, Obs, Missing_pct, ZeroRet_pct)
write_csv(table1, "Output/descriptive/table1_data_quality.csv")

# ── Table 16: descriptive statistics on each asset's own trading days ────────
px <- read.csv("Data/Dataset.csv", check.names = FALSE)
px$Date <- as.Date(px$Date)
px <- px %>% filter(Date >= START, Date <= END)

describe <- function(x) {
  x <- x[is.finite(x)]
  s <- sd(x); m <- mean(x)
  q <- quantile(x, c(0.25, 0.5, 0.75), names = FALSE)
  c(Mean = m, SD = s, Min = min(x), Q25 = q[1], Median = q[2], Q75 = q[3], Max = max(x),
    Skewness = mean((x - m)^3) / s^3, Kurtosis = mean((x - m)^4) / s^4)
}
table16 <- bind_rows(lapply(PAPER30, function(a) {
  p <- px[[a]][!is.na(px[[a]])]
  r <- diff(log(p))
  bind_rows(c(Panel = "Prices", Ticker = a, round(describe(p), 4)),
            c(Panel = "Returns", Ticker = a, round(describe(r), 4)))
})) %>% arrange(desc(Panel == "Prices"))
write_csv(table16, "Output/descriptive/table16_descriptive.csv")

# ── Figures 12 and 13: price and log-return panels ───────────────────────────
COLS <- c("Energy token" = "#E41A1C", "Cryptocurrency" = "#7B4FA1", "Crypto index" = "black",
          "ESG / green finance" = "#2CA02C", "Commodity" = "#CC79A7", "Energy equity" = "#0072B2")
EVENTS <- tibble(xmin = as.Date(c("2021-01-01", "2022-04-01")),
                 xmax = as.Date(c("2021-04-01", "2022-10-01")),
                 fill = c("#C7E9C0", "#FDD0A2"))
panel <- function(d, a, ylab, file) {
  col <- COLS[GROUP$group[GROUP$asset == a]]
  g <- ggplot(d, aes(Date, y)) +
    geom_rect(data = EVENTS, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              inherit.aes = FALSE, fill = EVENTS$fill, alpha = 0.6) +
    geom_line(colour = col, linewidth = 0.3) +
    labs(title = a, x = "Date", y = ylab) +
    theme_bw(base_size = 8) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  ggsave(file, g, width = 3, height = 2.2, dpi = 300)
}
for (a in PAPER30) {
  dp <- px %>% select(Date, y = all_of(a)) %>% filter(!is.na(y))
  panel(dp, a, "Value", file.path("Output/figures/prices", paste0(a, ".png")))
  dr <- dp %>% mutate(y = c(NA, diff(log(y)))) %>% filter(!is.na(y))
  panel(dr, a, "Log-return", file.path("Output/figures/returns", paste0(a, ".png")))
}

print(table1, n = 30)
message("Saved Output/descriptive/{table1_data_quality,table16_descriptive}.csv and Output/figures/{prices,returns}/")
