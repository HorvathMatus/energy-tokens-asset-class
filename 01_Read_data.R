rm(list=ls())
library(data.table)
library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)
library(tidyverse)
source("Functions/Read_data.R")


### Read CoinGecko data
data_dir <- "Raw_Data"

files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)

data_coingecko <- map(files, read_coingecko)

data_coingecko <- reduce(data_coingecko, full_join, by = "Date") %>%
  arrange(Date)

summary(data_coingecko)

data_coingecko <- data_coingecko %>% mutate(Date = as.Date(Date))

### Read FirstRate data
data_dir2 <- "Raw_Data/FRD"

files2 <- list.files(data_dir2, pattern = "\\.(csv|txt)$", full.names = TRUE)

data_firstrate <- map(files2, read_firstrate, tz = "UTC")

data_firstrate <- reduce(data_firstrate, full_join, by = "DateTime") %>%
  arrange(DateTime)

summary(data_firstrate)

data_firstrate <- data_firstrate %>%
  arrange(DateTime) %>%
  mutate(Date = as.Date(DateTime)) %>%
  group_by(Date) %>%
  summarise(across(-DateTime, ~ dplyr::last(.x)), .groups = "drop")

### Join datasets
df <- full_join(data_coingecko, data_firstrate, by = "Date") %>%
  arrange(Date)

summary(df)

### Save dataset
save(df, file="Data/Dataset.RData")
readr::write_csv(df, "Data/Dataset.csv")

################################################################################
### Create Energy Tokens dataset
rm(list=ls())
load("Data/Dataset.RData")

e_token <- df %>% filter(Date >= ymd("2020-12-07")) %>% select(Date, POWR, WPR, EWT, WOZX, SNC, SLR)
e_token <- e_token %>% fill(WPR, SLR, .direction = "down")
ret <- data.frame("Date" = e_token[-1,1], apply(e_token[,-1], 2, function(x) diff(log(as.matrix(x), ), lag = 1)))
save(ret, file="Data/return.RData")
fwrite(ret[,2:ncol(ret)],'julia/Data/data_e_token.txt',col.names = F)


### Create Energy Tokens short dataset
rm(list=ls())
load("Data/Dataset.RData")

e_token <- df %>% filter(Date >= ymd("2020-12-07")) %>% select(Date, POWR, EWT, SNC)
#e_token <- e_token %>% fill(WPR, SLR, .direction = "down")
ret <- data.frame("Date" = e_token[-1,1], apply(e_token[,-1], 2, function(x) diff(log(as.matrix(x), ), lag = 1)))
save(ret, file="Data/return.RData")
fwrite(ret[,2:ncol(ret)],'julia/Data/data_e_token_short.txt',col.names = F)

################################################################################
### Create POWR dataset
rm(list=ls())
load("Data/Dataset.RData")

e_token <- df %>% filter(Date >= ymd("2020-12-07")) %>% select(Date, POWR, 
                                                               BTC, ETH, SOL, AVAX,
                                                               DPI, IDEX, SPTOP20, 
                                                               EUA,
                                                               NEE, ENPH,
                                                               PBW, PBD, FENY, QCLN, ICLN,
                                                               B, CL, HH, GC, HG, LIT,
                                                               ESGD, ESGU, BGRN)
summary(e_token)
e_token <- e_token %>% tidyr::drop_na()

ret <- data.frame("Date" = e_token[-1,1], apply(e_token[,-1], 2, function(x) diff(log(as.matrix(x), ), lag = 1)))
save(ret, file="Data/return_PWR.RData")
fwrite(ret[,2:ncol(ret)],'julia/Data/data_PWR.txt',col.names = F)

################################################################################
### Create WPR dataset
rm(list=ls())
load("Data/Dataset.RData")

e_token <- df %>% filter(Date >= ymd("2020-12-07")) %>% select(Date, WPR, 
                                                               BTC, ETH, SOL, AVAX,
                                                               DPI, IDEX, SPTOP20, 
                                                               EUA,
                                                               NEE, ENPH,
                                                               PBW, PBD, FENY, QCLN, ICLN,
                                                               B, CL, HH, GC, HG, LIT,
                                                               ESGD, ESGU, BGRN)
summary(e_token)
e_token <- e_token %>% fill(WPR, .direction = "down")
e_token <- e_token %>% tidyr::drop_na()

ret <- data.frame("Date" = e_token[-1,1], apply(e_token[,-1], 2, function(x) diff(log(as.matrix(x), ), lag = 1)))
save(ret, file="Data/return_WPR.RData")
fwrite(ret[,2:ncol(ret)],'julia/Data/data_WPR.txt',col.names = F)

################################################################################
### Create EWT dataset
rm(list=ls())
load("Data/Dataset.RData")

e_token <- df %>% filter(Date >= ymd("2020-12-07")) %>% select(Date, EWT, 
                                                               BTC, ETH, SOL, AVAX,
                                                               DPI, IDEX, SPTOP20, 
                                                               EUA,
                                                               NEE, ENPH,
                                                               PBW, PBD, FENY, QCLN, ICLN,
                                                               B, CL, HH, GC, HG, LIT,
                                                               ESGD, ESGU, BGRN)
summary(e_token)
e_token <- e_token %>% tidyr::drop_na()

ret <- data.frame("Date" = e_token[-1,1], apply(e_token[,-1], 2, function(x) diff(log(as.matrix(x), ), lag = 1)))
save(ret, file="Data/return_EWT.RData")
fwrite(ret[,2:ncol(ret)],'julia/Data/data_EWT.txt',col.names = F)

################################################################################
### Create WOZX dataset
rm(list=ls())
load("Data/Dataset.RData")

e_token <- df %>% filter(Date >= ymd("2020-12-07")) %>% select(Date, WOZX, 
                                                               BTC, ETH, SOL, AVAX,
                                                               DPI, IDEX, SPTOP20, 
                                                               EUA,
                                                               NEE, ENPH,
                                                               PBW, PBD, FENY, QCLN, ICLN,
                                                               B, CL, HH, GC, HG, LIT,
                                                               ESGD, ESGU, BGRN)
summary(e_token)
e_token <- e_token %>% tidyr::drop_na()

ret <- data.frame("Date" = e_token[-1,1], apply(e_token[,-1], 2, function(x) diff(log(as.matrix(x), ), lag = 1)))
save(ret, file="Data/return_WOZX.RData")
fwrite(ret[,2:ncol(ret)],'julia/Data/data_WOZX.txt',col.names = F)

################################################################################
### Create SNC dataset
rm(list=ls())
load("Data/Dataset.RData")

e_token <- df %>% filter(Date >= ymd("2020-12-07")) %>% select(Date, SNC, 
                                                               BTC, ETH, SOL, AVAX,
                                                               DPI, IDEX, SPTOP20, 
                                                               EUA,
                                                               NEE, ENPH,
                                                               PBW, PBD, FENY, QCLN, ICLN,
                                                               B, CL, HH, GC, HG, LIT,
                                                               ESGD, ESGU, BGRN)
summary(e_token)
e_token <- e_token %>% tidyr::drop_na()

ret <- data.frame("Date" = e_token[-1,1], apply(e_token[,-1], 2, function(x) diff(log(as.matrix(x), ), lag = 1)))
save(ret, file="Data/return_SNC.RData")
fwrite(ret[,2:ncol(ret)],'julia/Data/data_SNC.txt',col.names = F)

################################################################################
### Create SLR dataset
rm(list=ls())
load("Data/Dataset.RData")

e_token <- df %>% filter(Date >= ymd("2020-12-07")) %>% select(Date, SLR, 
                                                               BTC, ETH, SOL, AVAX,
                                                               DPI, IDEX, SPTOP20, 
                                                               EUA,
                                                               NEE, ENPH,
                                                               PBW, PBD, FENY, QCLN, ICLN,
                                                               B, CL, HH, GC, HG, LIT,
                                                               ESGD, ESGU, BGRN)
summary(e_token)
e_token <- e_token %>% fill(SLR, .direction = "down")
e_token <- e_token %>% tidyr::drop_na()

ret <- data.frame("Date" = e_token[-1,1], apply(e_token[,-1], 2, function(x) diff(log(as.matrix(x), ), lag = 1)))
save(ret, file="Data/return_SLR.RData")
fwrite(ret[,2:ncol(ret)],'julia/Data/data_SLR.txt',col.names = F)

