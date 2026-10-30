# =============================================================================
# 21_rolling_exposure.R   (JCM, referee checks)
#
#   (A) Exposure over time: adjusted R^2 of the crypto block and of all commodity
#       blocks (electricity, energy futures, gold, copper) in rolling 52-week
#       regressions of weekly token returns, for the liquid energy tokens and the
#       gold-backed tokens. Shows whether energy exposure appeared in the 2022
#       energy crisis.
#   (B) Home electricity markets: each token against the market its platform
#       serves (POWR: New South Wales, SNC: Slovenia, WOZX: northern Italy,
#       EWT: Germany), weekly returns and daily lead-lag returns, with and
#       without the crypto block.
#   (C) Platform-activity channel: does electricity price volatility move token
#       trading volume? Weekly log volume growth on the change in weekly
#       electricity volatility, controlling for Bitcoin volume growth.
#
# Requires 18_electricity_gold.R. Outputs in Output/jcm/ and
# Output/figures/results_jcm/rolling_exposure.png
# =============================================================================

rm(list = ls())
suppressMessages({ library(tidyverse); library(sandwich); library(lubridate) })
src18 <- readLines("18_electricity_gold.R")
eval(parse(text = src18[1:(grep("^# ── Block regressions", src18) - 1)]))
COMMOD <- c(ELEC, EN_FUT, METALS)
adjr2 <- function(y, X) { d <- na.omit(data.frame(y = y, X)); if (nrow(d) < ncol(X) + 10) return(NA_real_)
  summary(lm(y ~ ., data = d))$adj.r.squared }

# ── (A) rolling 52-week exposure ───────────────────────────────────────────
WIN <- 52
roll <- bind_rows(lapply(c("POWR", "EWT", "SNC", "PAXG", "XAUT"), function(a) {
  bind_rows(lapply(seq(WIN, nrow(W)), function(e) {
    w <- W[(e - WIN + 1):e, ]
    tibble(token = a, week = W$wk[e],
           Crypto = adjr2(w[[a]], as.data.frame(w[, CRYPTO_BLOCK])),
           Commodities = adjr2(w[[a]], as.data.frame(w[, COMMOD])))
  }))
}))
write_csv(roll, "Output/jcm/rolling_exposure.csv")
summ <- roll %>% group_by(token) %>%
  summarise(crypto_mean = mean(Crypto, na.rm = TRUE), crypto_min = min(Crypto, na.rm = TRUE),
            comm_mean = mean(Commodities, na.rm = TRUE), comm_max = max(Commodities, na.rm = TRUE),
            share_comm_gt_crypto = mean(Commodities > Crypto, na.rm = TRUE),
            comm_2022 = mean(Commodities[year(week) == 2022 | (year(week) == 2023 & month(week) <= 3)], na.rm = TRUE),
            crypto_2022 = mean(Crypto[year(week) == 2022 | (year(week) == 2023 & month(week) <= 3)], na.rm = TRUE))
print(as.data.frame(summ))
write_csv(summ, "Output/jcm/rolling_exposure_summary.csv")

lab <- c(POWR = "Power Ledger (POWR)", EWT = "Energy Web Token (EWT)", SNC = "SunContract (SNC)",
         PAXG = "PAX Gold (PAXG)", XAUT = "Tether Gold (XAUT)")
pd <- roll %>% pivot_longer(c(Crypto, Commodities), names_to = "Block", values_to = "R2") %>%
  mutate(token = factor(lab[token], lab), Block = factor(Block, c("Crypto", "Commodities"),
                                                         c("Crypto block", "Commodity blocks: electricity, energy futures, gold, copper")))
ev <- tibble(xmin = as.Date("2022-04-01"), xmax = as.Date("2022-10-01"))
p <- ggplot(pd, aes(week, 100 * R2, colour = Block)) + theme_classic(base_size = 11) +
  geom_rect(data = ev, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), inherit.aes = FALSE, fill = "orange", alpha = 0.15) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_line(linewidth = 0.7) + facet_wrap(~ token, ncol = 3, scales = "free_x") +
  scale_colour_manual(values = c("#2C7BB6", "#D7191C"), name = NULL) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = NULL, y = "Adjusted R-squared (%), rolling 52 weeks") +
  theme(legend.position = "bottom", strip.background = element_blank(),
        strip.text = element_text(face = "bold"))
dir.create("Output/figures/results_jcm", showWarnings = FALSE, recursive = TRUE)
ggsave("Output/figures/results_jcm/rolling_exposure.png",
       p, width = 10, height = 5.6, dpi = 300, bg = "white")

# ── (B) home electricity markets ───────────────────────────────────────────
HOME <- c(POWR = "EL_AU", SNC = "EL_SI", WOZX = "EL_ITN", EWT = "EL_DE")
ll <- function(X, LL = c(-1, 0, 1, 2)) as.data.frame(do.call(cbind, lapply(LL, function(k) {
  Z <- as.data.frame(lapply(as.data.frame(X), lagv, k = k)); names(Z) <- paste0(names(as.data.frame(X)), "_L", k + 1); Z })))
home <- bind_rows(lapply(names(HOME), function(a) {
  z <- HOME[[a]]
  fitw <- function(cols, df) { d <- na.omit(df[, c(a, cols)]); names(d)[1] <- "y"; lm(reformulate(cols, "y"), data = d) }
  # weekly
  mw1 <- fitw(z, W); mw2 <- fitw(c(z, CRYPTO_BLOCK), W)
  # daily lead-lag
  Zd <- ll(D[, z, drop = FALSE]); Cd <- ll(D[, CRYPTO_BLOCK])
  dd1 <- na.omit(data.frame(y = D[[a]], Zd)); dd2 <- na.omit(data.frame(y = D[[a]], Zd, Cd))
  md1 <- lm(y ~ ., dd1); md2 <- lm(y ~ ., dd2)
  tibble(token = a, market = z,
         weekly_beta = coef(mw1)[z], weekly_t = coef(mw1)[z] / sqrt(NeweyWest(mw1, prewhite = FALSE)[z, z]),
         weekly_adjR2 = summary(mw1)$adj.r.squared,
         weekly_beta_ctrl = coef(mw2)[z], weekly_t_ctrl = coef(mw2)[z] / sqrt(NeweyWest(mw2, prewhite = FALSE)[z, z]),
         ll_adjR2 = summary(md1)$adj.r.squared, ll_p = wald_hac(md1, names(Zd)),
         ll_p_ctrl = wald_hac(md2, names(Zd)))
}))
print(home, width = 200)
write_csv(home, "Output/jcm/home_market.csv")

# ── (C) platform-activity channel: volume and electricity volatility ───────
vol <- bind_rows(lapply(c("POWR", "EWT", "SNC", "BTC"), function(a) {
  read_csv(file.path("Raw_Data/CoinGecko_volume", paste0(a, ".csv")), show_col_types = FALSE) %>%
    transmute(Date = as.Date(substr(as.character(event_date), 1, 10)), token = a, v = as.numeric(volume_usd))
})) %>% filter(Date >= START, Date <= END, v > 0)
volw <- vol %>% mutate(wk = floor_date(Date, "week", week_start = 1)) %>% group_by(token, wk) %>%
  summarise(lv = log(mean(v)), .groups = "drop") %>% arrange(wk) %>% group_by(token) %>% mutate(dlv = lv - lag(lv)) %>%
  select(-lv) %>% pivot_wider(names_from = token, values_from = dlv)
evol <- E %>% mutate(wk = floor_date(Date, "week", week_start = 1)) %>% group_by(wk) %>%
  summarise(across(all_of(ELEC), ~ log(sd(.x, na.rm = TRUE))), .groups = "drop") %>% arrange(wk) %>%
  mutate(across(all_of(ELEC), ~ .x - lag(.x), .names = "d_{.col}"))
ch <- bind_rows(lapply(names(HOME)[names(HOME) != "WOZX"], function(a) {
  z <- paste0("d_", HOME[[a]])
  d <- na.omit(volw %>% select(wk, y = all_of(a), BTC) %>% left_join(evol %>% select(wk, x = all_of(z)), by = "wk"))
  m <- lm(y ~ x + BTC, d)
  tibble(token = a, market = HOME[[a]], n = nrow(d), beta = coef(m)["x"],
         t = coef(m)["x"] / sqrt(NeweyWest(m, prewhite = FALSE)["x", "x"]), beta_btc = coef(m)["BTC"])
}))
print(ch)
write_csv(ch, "Output/jcm/volume_channel.csv")
