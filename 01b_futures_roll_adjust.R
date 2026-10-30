# =============================================================================
# 01b_futures_roll_adjust.R
# Correct returns for the continuous futures (B, CL, HH, GC, HG).
#
# The FirstRate continuous series in Raw_Data/FRD are back-adjusted by the
# difference (Panama) method, so their price levels are shifted by the
# cumulated roll gaps (Brent 6.86 on 7 Dec 2020 against an actual 48.79).
# Log changes of these levels mis-scale the returns. The correct daily return
# of the rolled front-month position is the change in the back-adjusted price
# divided by the previous unadjusted price,
#     R_t = (A_t - A_{t-1}) / U_{t-1},
# where U comes from the unadjusted continuous series in Raw_Data/FRD_unadjusted.
# We store a ratio-adjusted price index I with log(I_t / I_{t-1}) = log(1 + R_t),
# anchored to the unadjusted price on the last sample date, in place of the
# back-adjusted price, so every script that takes diff(log(price)) from
# Data/Dataset.csv now gets the correct return. Unadjusted price levels for the
# descriptive statistics are saved in Data/futures_unadjusted.csv.
# Afterwards the dataset-building part of 01_Read_data.R is re-run.
# =============================================================================

rm(list = ls())
suppressMessages({ library(tidyverse); library(data.table); library(lubridate) })

FUT <- c("B", "CL", "HH", "GC", "HG")
END <- as.Date("2026-01-31")
load("Data/Dataset.RData")                                  # df
rd <- function(f) read_csv(f, col_names = FALSE, show_col_types = FALSE) %>% transmute(Date = as.Date(X1), Close = X5)

unadj_levels <- tibble(Date = df$Date)
for (f in FUT) {
  A <- rd(file.path("Raw_Data/FRD", paste0(f, ".txt")))
  U <- rd(file.path("Raw_Data/FRD_unadjusted", paste0(f, "_full_1day_continuous_UNadjusted.txt")))
  x <- inner_join(rename(A, A = Close), rename(U, U = Close), by = "Date") %>% arrange(Date)
  if (nrow(x) < nrow(filter(A, Date <= END))) stop(f, ": dates missing in the unadjusted file")
  # the back-adjustment gap U - A may change only on roll dates
  gap <- x$U - x$A; rolls <- sum(abs(diff(gap)) > 1e-6 & x$Date[-1] >= as.Date("2020-12-07") & x$Date[-1] <= END)
  R <- c(NA, diff(x$A) / head(x$U, -1))
  lr <- c(0, log1p(R[-1]))
  k <- max(which(x$Date <= END))
  x$I <- x$U[k] * exp(cumsum(lr) - sum(lr[1:k]))
  # sanity: the old column must be the back-adjusted close
  m <- match(df$Date, x$Date); ok <- !is.na(df[[f]])
  stopifnot(all(abs(df[[f]][ok] - x$A[m[ok]]) < 1e-6))
  cat(sprintf("%-3s rolls in sample: %3d   2020-12-07  adjusted %.2f  unadjusted %.2f  index %.2f\n", f, rolls,
              x$A[x$Date == "2020-12-07"], x$U[x$Date == "2020-12-07"], x$I[x$Date == "2020-12-07"]))
  df[[f]][ok] <- x$I[m[ok]]
  unadj_levels[[f]] <- x$U[m]
}
save(df, file = "Data/Dataset.RData")
readr::write_csv(df, "Data/Dataset.csv")
readr::write_csv(unadj_levels %>% filter(if_any(all_of(FUT), ~ !is.na(.x))), "Data/futures_unadjusted.csv")

# re-build the token datasets for Julia from the corrected prices
src <- readLines("01_Read_data.R")
eval(parse(text = src[grep("^### Create Energy Tokens dataset", src):length(src)]))
message("Corrected futures returns written to Data/Dataset.{RData,csv} and julia/Data/data_*.txt")
