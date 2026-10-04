# =============================================================================
# 13_tail_risk.R   (Referee point #30)
# Does the crypto-identity of energy tokens survive in the LEFT TAIL (crashes)?
# Three stress-state measures, token vs crypto block vs energy block:
#   (1) Lower-tail dependence  lambda_L at the 5% and 10% thresholds
#   (2) Exceedance correlation in the lower tail
#   (3) Delta-CoVaR via quantile regression (token 5% VaR conditional on the
#       factor being in distress vs at its median)
#
# Output: Output/identity/tail_risk.csv
# =============================================================================

rm(list = ls())
suppressMessages({ library(tidyverse); library(quantreg) })

load("Data/returns_full30.RData")
dir.create("Output/identity", showWarnings = FALSE, recursive = TRUE)

# Equal-weight block factors (transparent; robust to sparse tokens)
cryptoF <- rowMeans(scale(R30[, c(CRYPTO, CRYPTO_IX)]))
energyF <- rowMeans(scale(R30[, c(COMMOD, ENERGY_EQ)]))

# (1) lower-tail dependence: P(X below u-quantile | Y below u-quantile)
ltd <- function(x, y, u){
  ok <- complete.cases(x,y); x<-x[ok]; y<-y[ok]
  qx<-quantile(x,u); qy<-quantile(y,u)
  denom<-sum(y<=qy); if(denom==0) return(NA_real_)
  sum(x<=qx & y<=qy)/denom
}
# (2) exceedance correlation in lower tail (factor below its u-quantile)
exc_cor <- function(x, y, u){
  ok<-complete.cases(x,y); x<-x[ok]; y<-y[ok]
  sel<-y<=quantile(y,u); if(sum(sel)<10) return(NA_real_)
  cor(x[sel], y[sel])
}
# (3) Delta-CoVaR: token tau-quantile conditional on factor at its tau vs median
dcovar <- function(token, factor, tau=0.05){
  d<-na.omit(data.frame(y=token, f=factor))
  fit<-rq(y ~ f, tau=tau, data=d)
  fq <- as.numeric(quantile(d$f, tau)); fm <- as.numeric(median(d$f))
  covar_stress<-predict(fit, newdata=data.frame(f=fq))
  covar_med   <-predict(fit, newdata=data.frame(f=fm))
  as.numeric(covar_stress - covar_med)   # more negative = stronger tail spillover
}

rows<-list()
for(tok in TOKENS_ALL){
  x<-R30[[tok]]
  rows[[tok]]<-tibble(
    token=tok,
    ltd5_crypto = round(ltd(x,cryptoF,0.05),3), ltd5_energy = round(ltd(x,energyF,0.05),3),
    ltd10_crypto= round(ltd(x,cryptoF,0.10),3), ltd10_energy= round(ltd(x,energyF,0.10),3),
    exc_crypto  = round(exc_cor(x,cryptoF,0.10),3), exc_energy = round(exc_cor(x,energyF,0.10),3),
    dCoVaR_crypto = round(dcovar(x,cryptoF),3), dCoVaR_energy = round(dcovar(x,energyF),3)
  )
}
tail_risk<-bind_rows(rows)
write_csv(tail_risk,"Output/identity/tail_risk.csv")

cat("\n", strrep("=",78), "\n", sep="")
cat("#30 TAIL-RISK: token co-movement with crypto vs energy block in the LEFT TAIL\n")
cat(strrep("=",78), "\n", sep="")
cat("\nLower-tail dependence (5%): prob token crashes when block crashes\n")
print(tail_risk %>% select(token, ltd5_crypto, ltd5_energy))
cat("\nExceedance correlation (lower 10% of block):\n")
print(tail_risk %>% select(token, exc_crypto, exc_energy))
cat("\nDelta-CoVaR (more negative = stronger downside spillover FROM block TO token):\n")
print(tail_risk %>% select(token, dCoVaR_crypto, dCoVaR_energy))
message("\nSaved: Output/identity/tail_risk.csv")
