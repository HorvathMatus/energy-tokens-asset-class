# =============================================================================
# 12_portfolio_robustness.R   (Referee points #20, #23, #24, #25, #26)
# Robustness battery for the portfolio results, all on the same per-token return
# files used by 08_portfolio_analysis.R so numbers are directly comparable.
#
#   #23  No-crypto benchmark : does a token diversify a green/energy-only
#        portfolio (crypto assets removed)?
#   #24  Transaction costs   : net-of-cost Sharpe under two cost schemes
#        (flat 20 bps; tiered 10 bps liquid / 50 bps token) + turnover.
#   #25  Token-weight caps   : cap the token allocation at 1/2.5/5/10%.
#   #20  Covariance estimator: sample vs Ledoit-Wolf shrinkage.
#   #26  Block bootstrap     : moving-block bootstrap Sharpe-difference test.
#
# Output: Output/portfolio/robust_*.csv
# =============================================================================

rm(list = ls())
suppressMessages({
  library(tidyverse); library(quadprog); library(mFilter)
})
set.seed(2026)

WINDOW <- 252; REBAL <- 21; FREQ <- 252; NB <- 10000; BLK <- 21
# one-day implementation lag: weights use returns up to t0-2 and are held from t0, so the
# first token return held (which starts at 00:00 UTC of day t0-1) begins after the last
# exchange close in the estimation window
IMPL_LAG <- 1

CRYPTO <- c("BTC","ETH","SOL","AVAX","DPI","IDEX","SPTOP20")   # removed in #23

load_returns <- function(p){ load(p); ret$Date <- as.Date(ret$Date); ret }
FILES <- list(POWR="Data/return_PWR.RData", SNC="Data/return_SNC.RData",
              EWT="Data/return_EWT.RData")
DAT <- lapply(FILES, load_returns)

# ── Ledoit-Wolf shrinkage to constant-correlation target ─────────────────────
ledoit_wolf <- function(R){
  R <- as.matrix(R); n <- nrow(R); p <- ncol(R)
  S <- cov(R) * (n-1)/n
  s <- sqrt(diag(S)); Rcorr <- S / (s %o% s)
  rbar <- (sum(Rcorr) - p) / (p*(p-1))
  F <- rbar * (s %o% s); diag(F) <- diag(S)          # target
  Xc <- scale(R, center=TRUE, scale=FALSE)
  # pi-hat
  pihat <- sum((crossprod(Xc^2)/n - S^2))
  # rho-hat (diagonal + off-diagonal asymptotic terms)
  theta_ii <- crossprod(Xc^3, Xc)/n - diag(S)*S
  rho_diag <- sum(diag((crossprod(Xc^2)/n - S^2)))
  term <- matrix(0,p,p)
  for(i in 1:p) for(j in 1:p) if(i!=j)
    term[i,j] <- (s[j]/s[i])*( (crossprod(Xc[,i]^2,Xc[,i]*Xc[,j])/n)[1] - S[i,i]*S[i,j])
  rho_off <- rbar * sum(term[upper.tri(term)|lower.tri(term)])
  rhohat <- rho_diag + rho_off
  gammahat <- sum((F - S)^2)
  kappa <- (pihat - rhohat)/gammahat
  delta <- max(0, min(1, kappa/n))
  delta*F + (1-delta)*S
}

# ── Spectral covariance (Baxter-King), matching 08 ───────────────────────────
spectral_cov <- function(R, horizon=c("short","long"), bk=6){
  horizon <- match.arg(horizon); n <- nrow(R); k <- ncol(R)
  if(horizon=="short"){pl<-2;pu<-5}else{pl<-20;pu<-min(n%/%3,252)}
  if(n < 2*bk+pl+5) return(cov(R, use="complete.obs"))
  f <- sapply(seq_len(k), function(j)
    tryCatch(bkfilter(R[,j], pl=pl, pu=pu, drift=FALSE)$cycle,
             error=function(e) rep(NA_real_,n)))
  f <- f[complete.cases(f),,drop=FALSE]
  if(nrow(f)<10) return(cov(R, use="complete.obs")); cov(f)
}
make_pd <- function(S){ ev<-min(eigen(S,symmetric=TRUE,only.values=TRUE)$values)
  if(ev<=0) S<-S+(-ev+1e-7)*diag(nrow(S)); S }

# ── Weights with optional cap on ONE asset (token) ───────────────────────────
ew_w  <- function(n) rep(1/n,n)
gmv_w <- function(S, cap=NULL, ti=NULL){
  n<-nrow(S); Dmat<-2*S+diag(1e-8,n); dvec<-rep(0,n)
  Amat<-cbind(rep(1,n),diag(n)); bvec<-c(1,rep(0,n))
  if(!is.null(cap)&&!is.null(ti)){ e<--diag(n)[,ti,drop=FALSE]
    Amat<-cbind(Amat,e); bvec<-c(bvec,-cap) }              # w_token <= cap
  sol<-tryCatch(solve.QP(Dmat,dvec,Amat,bvec,meq=1),error=function(e)NULL)
  if(is.null(sol)) return(ew_w(n)); w<-pmax(sol$solution,0); w/sum(w)
}
mve_w <- function(mu,S,cap=NULL,ti=NULL){
  n<-nrow(S); ex<-mu; if(all(ex<=0)) return(gmv_w(S,cap,ti))
  Dmat<-2*S+diag(1e-8,n); dvec<-ex
  Amat<-cbind(rep(1,n),diag(n)); bvec<-c(1,rep(0,n))
  if(!is.null(cap)&&!is.null(ti)){ e<--diag(n)[,ti,drop=FALSE]
    Amat<-cbind(Amat,e); bvec<-c(bvec,-cap) }
  sol<-tryCatch(solve.QP(Dmat,dvec,Amat,bvec,meq=1),error=function(e)NULL)
  if(is.null(sol)) return(gmv_w(S,cap,ti)); w<-pmax(sol$solution,0)
  if(sum(w)<1e-10) return(gmv_w(S,cap,ti)); w/sum(w)
}

# ── Rolling engine: returns gross daily returns + weight path ────────────────
roll <- function(ret_df, token_col=NULL, include_tok=TRUE,
                 strategy="GMV", horizon="total", covest="sample",
                 cap=NULL, drop_assets=NULL, window=WINDOW, rebal=REBAL){
  dates<-ret_df$Date; R_all<-as.matrix(ret_df[,-1])
  if(!is.null(drop_assets)) R_all<-R_all[,!colnames(R_all)%in%drop_assets,drop=FALSE]
  keep <- if(!is.null(token_col)&&!include_tok) which(colnames(R_all)!=token_col) else seq_len(ncol(R_all))
  R<-R_all[,keep,drop=FALSE]; n<-nrow(R); asset_names<-colnames(R)
  ti_full <- if(!is.null(token_col)) match(token_col,asset_names) else NA
  port<-rep(NA_real_,n)
  rebal_idx<-seq(window+1+IMPL_LAG,n-1,by=rebal)
  W_tar<-matrix(0,length(rebal_idx),ncol(R)); W_dri<-matrix(0,length(rebal_idx),ncol(R))
  w_prev<-rep(0,ncol(R)); t_prev<-NA; r=1
  for(t0 in rebal_idx){
    te<-t0-1; ts<-te-IMPL_LAG-window+1; tn<-min(t0+rebal-1,n)
    Rw<-R[ts:(te-IMPL_LAG),,drop=FALSE]; naf<-colMeans(is.na(Rw)); ok<-which(naf<0.10)
    if(length(ok)<2){r<-r+1;next}; Rw<-na.omit(Rw[,ok,drop=FALSE])
    if(nrow(Rw)<50){r<-r+1;next}
    mu<-colMeans(Rw)
    S <- if(horizon=="total"){ if(covest=="lw") ledoit_wolf(Rw) else cov(Rw)
         } else spectral_cov(Rw,horizon); S<-make_pd(S)
    ti_loc <- if(!is.na(ti_full)) match(ti_full, ok) else NA
    capi <- if(!is.null(cap)&&!is.na(ti_loc)) cap else NULL
    w_trim<-switch(strategy, EW=ew_w(ncol(Rw)),
                   GMV=gmv_w(S,capi,ti_loc), MVE=mve_w(mu,S,capi,ti_loc))
    w_full<-rep(0,ncol(R)); w_full[ok]<-w_trim
    # drift of previous weights over the holding period just ended (t_prev..t0-1)
    w_drift<-if(sum(w_prev)>0){
      gr<-apply(exp(R[t_prev:te,,drop=FALSE]),2,function(x){x[is.na(x)]<-1;prod(x)})
      wd<-w_prev*gr; wd/sum(wd)} else rep(0,ncol(R))
    W_tar[r,]<-w_full; W_dri[r,]<-w_drift; w_prev<-w_full; t_prev<-t0
    Rn<-R[t0:tn,,drop=FALSE]; port[t0:tn]<-as.numeric(ifelse(is.na(Rn),0,Rn)%*%w_full)
    r<-r+1
  }
  list(dates=dates, gross=port, W_tar=W_tar, W_dri=W_dri,
       rebal_idx=rebal_idx, assets=asset_names, token_idx=ti_full)
}

# ── Net-of-cost returns + turnover ───────────────────────────────────────────
net_of_cost <- function(res, cost_vec){
  g<-res$gross; cv<-cost_vec[res$assets]; cv[is.na(cv)]<-0
  for(i in seq_along(res$rebal_idx)){
    t0<-res$rebal_idx[i]
    cost<-sum(abs(res$W_tar[i,]-res$W_dri[i,])*cv)
    if(t0<=length(g)&&!is.na(g[t0])) g[t0]<-g[t0]-cost
  }
  g
}
turnover_ann <- function(res){
  tn<-rowSums(abs(res$W_tar-res$W_dri))*0.5
  tn<-tn[rowSums(res$W_tar)>0]
  mean(tn)*(FREQ/REBAL)
}
SR <- function(x){x<-na.omit(x); if(length(x)<20)return(NA); mean(x)/sd(x)*sqrt(FREQ)}

# ── Sharpe-difference tests (#26) ────────────────────────────────────────────
# (i) Ledoit & Wolf (2008) HAC delta-method test on the paired daily returns.
lw_sr_test <- function(r_aug,r_base){
  ok<-complete.cases(r_aug,r_base); x<-r_aug[ok]; y<-r_base[ok]; n<-length(x)
  m1<-mean(x); m2<-mean(y); g1<-mean(x^2); g2<-mean(y^2)
  s1<-sqrt(g1-m1^2); s2<-sqrt(g2-m2^2); d<-m1/s1-m2/s2
  grad<-c(g1/s1^3,-g2/s2^3,-m1/(2*s1^3),m2/(2*s2^3))
  Y<-cbind(x-m1,y-m2,x^2-g1,y^2-g2); L<-floor(4*(n/100)^(2/9))
  Psi<-crossprod(Y)/n
  for(l in seq_len(L)){ G<-crossprod(Y[(l+1):n,,drop=FALSE],Y[1:(n-l),,drop=FALSE])/n
    Psi<-Psi+(1-l/(L+1))*(G+t(G)) }
  se<-sqrt(as.numeric(t(grad)%*%Psi%*%grad)/n)
  2*pnorm(-abs(d/se))
}
# (ii) Moving-block bootstrap, with the bootstrap distribution centred on the
#      observed difference so that it approximates the null distribution.
block_boot_sr <- function(r_aug,r_base,B=NB,L=BLK){
  ok<-complete.cases(r_aug,r_base); a<-r_aug[ok]; b<-r_base[ok]; n<-length(a)
  obs<-SR(a)-SR(b); nb_blocks<-ceiling(n/L)
  d<-replicate(B,{
    starts<-sample(1:(n-L+1),nb_blocks,replace=TRUE)
    idx<-as.vector(sapply(starts,function(s)s:(s+L-1)))[1:n]
    SR(a[idx])-SR(b[idx])
  })
  list(diff=round(obs,3), p=round(mean(abs(d-obs)>=abs(obs)),3),
       p_lw=round(lw_sr_test(r_aug,r_base),3),
       ci=round(quantile(d,c(.025,.975)),3))
}

# ── Cost schemes ─────────────────────────────────────────────────────────────
all_assets <- colnames(DAT$POWR)[-1]
cost_flat  <- setNames(rep(0.0020,length(all_assets)), all_assets)         # 20 bps
cost_tier  <- setNames(rep(0.0010,length(all_assets)), all_assets)         # 10 bps liquid
for(tk in names(FILES)) cost_tier[tk]<-0.0050                              # 50 bps token

message("Running robustness battery...")

# =============================================================================
# #24  Transaction costs + turnover (GMV, total)
# =============================================================================
rows24<-list()
for(tk in names(FILES)){
  base<-roll(DAT[[tk]],token_col=tk,include_tok=FALSE,strategy="GMV")
  aug <-roll(DAT[[tk]],token_col=tk,include_tok=TRUE ,strategy="GMV")
  for(scheme in c("flat20","tier1050")){
    cv<-if(scheme=="flat20")cost_flat else cost_tier
    rows24[[paste(tk,scheme)]]<-tibble(
      token=tk, cost_scheme=scheme,
      SR_base_gross=SR(base$gross), SR_aug_gross=SR(aug$gross),
      SR_base_net=SR(net_of_cost(base,cv)), SR_aug_net=SR(net_of_cost(aug,cv)),
      dSR_net=SR(net_of_cost(aug,cv))-SR(net_of_cost(base,cv)),
      p_lw_net=lw_sr_test(net_of_cost(aug,cv),net_of_cost(base,cv)),
      turnover_base=turnover_ann(base), turnover_aug=turnover_ann(aug))
  }
}
tab24<-bind_rows(rows24)%>%mutate(across(where(is.numeric),~round(.,3)))
write_csv(tab24,"Output/portfolio/robust_txcost.csv")

# =============================================================================
# #25  Token-weight caps (GMV, total)
# =============================================================================
rows25<-list()
for(tk in names(FILES)){
  base<-roll(DAT[[tk]],token_col=tk,include_tok=FALSE,strategy="GMV")
  for(cap in c(0.01,0.025,0.05,0.10,NA)){
    capv<-if(is.na(cap))NULL else cap
    aug<-roll(DAT[[tk]],token_col=tk,include_tok=TRUE,strategy="GMV",cap=capv)
    tw<-aug$W_tar[,match(tk,aug$assets)]; tw<-tw[rowSums(aug$W_tar)>0]
    bb<-block_boot_sr(aug$gross,base$gross)
    rows25[[paste(tk,cap)]]<-tibble(
      token=tk, cap=ifelse(is.na(cap),"none",paste0(cap*100,"%")),
      SR_aug=SR(aug$gross), dSR=bb$diff, p_block=bb$p, p_lw=bb$p_lw,
      mean_tok_w=round(mean(tw),4), max_tok_w=round(max(tw),4))
  }
}
tab25<-bind_rows(rows25)%>%mutate(across(where(is.numeric),~round(.,3)))
write_csv(tab25,"Output/portfolio/robust_caps.csv")

# =============================================================================
# #20  Covariance estimator: sample vs Ledoit-Wolf (GMV, total)
# =============================================================================
rows20<-list()
for(tk in names(FILES)){
  for(ce in c("sample","lw")){
    base<-roll(DAT[[tk]],token_col=tk,include_tok=FALSE,strategy="GMV",covest=ce)
    aug <-roll(DAT[[tk]],token_col=tk,include_tok=TRUE ,strategy="GMV",covest=ce)
    bb<-block_boot_sr(aug$gross,base$gross)
    rows20[[paste(tk,ce)]]<-tibble(
      token=tk, covest=ce, SR_base=SR(base$gross), SR_aug=SR(aug$gross),
      dSR=bb$diff, p_block=bb$p, p_lw=bb$p_lw)
  }
}
tab20<-bind_rows(rows20)%>%mutate(across(where(is.numeric),~round(.,3)))
write_csv(tab20,"Output/portfolio/robust_covest.csv")

# =============================================================================
# #23  No-crypto green/energy-only benchmark (GMV + EW, total)
# =============================================================================
rows23<-list()
for(tk in names(FILES)){
  for(strat in c("GMV","EW")){
    base<-roll(DAT[[tk]],token_col=tk,include_tok=FALSE,strategy=strat,drop_assets=CRYPTO)
    aug <-roll(DAT[[tk]],token_col=tk,include_tok=TRUE ,strategy=strat,drop_assets=CRYPTO)
    bb<-block_boot_sr(aug$gross,base$gross)
    rows23[[paste(tk,strat)]]<-tibble(
      token=tk, strategy=strat,
      SR_base=SR(base$gross), SR_aug=SR(aug$gross),
      vol_base=round(sd(na.omit(base$gross))*sqrt(FREQ)*100,2),
      vol_aug =round(sd(na.omit(aug$gross ))*sqrt(FREQ)*100,2),
      dSR=bb$diff, p_block=bb$p, p_lw=bb$p_lw)
  }
}
tab23<-bind_rows(rows23)%>%mutate(across(where(is.numeric),~round(.,3)))
write_csv(tab23,"Output/portfolio/robust_nocrypto.csv")

# ── Console ──────────────────────────────────────────────────────────────────
cat("\n#24 TRANSACTION COSTS + TURNOVER\n"); print(tab24,n=30)
cat("\n#25 TOKEN-WEIGHT CAPS\n");            print(tab25,n=30)
cat("\n#20 COVARIANCE ESTIMATOR\n");         print(tab20,n=30)
cat("\n#23 NO-CRYPTO BENCHMARK\n");          print(tab23,n=30)
message("\nSaved robust_{txcost,caps,covest,nocrypto}.csv to Output/portfolio/")
