rm(list=ls())

# Set-up / Loading packages --------------------------------------------
pacman::p_load(
  tidyverse,
  urca,
  readxl,
  ggplot2,
  lubridate,
  vars,
  dplyr,
  MCMCpack,
  magic,
  coda
)

if(suppressWarnings(!require(bvarsv))){
  install.packages("bvarsv")
}

if(suppressWarnings(!require(MCMCpack))){
  install.packages("MCMCpack")
}

if(suppressWarnings(!require(magic))){
  install.packages("magic")
}

require(bvarsv)



# Loading data --------------------------------------------

data <- read_excel("./data/data_com_fin.xlsx") 

colnames(data)[2] <- "p_wheat" # Renaming wheat price and s&p
colnames(data)[7] <- "sp"

data$month <- gsub("M", "", data$month) # Transforming col "month" into date format
data$month <- ym(data$month, quiet = FALSE, tz = NULL, locale = Sys.getlocale("LC_TIME"))

# Setting up data --------------------------------------------

p_wheat_log_diff<-diff(log(data$p_wheat))
p_oil_log_diff<-diff(log(data$p_oil))
p_cotton_log_diff<-diff(log(data$p_cotton))               
reer_log<-log(data$reer)
ind_prod_log_diff<-diff(log(data$ind_prod))
sp_log_diff<-diff(log(data$sp))
ir_log_diff<-diff(log(data$ir))
cpi_log_diff<-diff(log(data$cpi))
exports_total_log_diff<-diff(log(data$exports_total))
netlong_mm <- data$netlong_mm[-1]/1000000 # Dividing by 1 million to make numbers comparable
netlong_swap<-data$netlong_swap[-1]/1000000 # otherwise there was an error 
reer_log<-reer_log[-1]
merged_data<-data.frame(p_wheat_log_diff,p_oil_log_diff,reer_log,ind_prod_log_diff
                        ,sp_log_diff,ir_log_diff,cpi_log_diff,exports_total_log_diff,
                        netlong_mm,netlong_swap)

d <- data$month[-1]
merged_data<-data.frame(d,merged_data)

## Data for wheat and money managers --------------------------
# Needed time series: ind_prod, exports, netlong_mm, p_wheat
# Selecting the needed time series in the correct order:
df_mm_w <- data.frame(merged_data[c(5,9,10,2)]) 
df_mm_w <- na.omit(df_mm_w)

Traw <- nrow(df_mm_w)
Yraw <- df_mm_w

plot.ts(Yraw)

# Useful function for lagging data matrices -------------------------------------
mlag <- function(X,lag){
  p <- lag
  X <- as.matrix(X)
  Traw <- nrow(X)
  N <- ncol(X)
  Xlag <- matrix(0,Traw,p*N)
  for (ii in 1:p){
    Xlag[(p+1):Traw,(N*(ii-1)+1):(N*ii)]=X[(p+1-ii):(Traw-ii),(1:N)]
  }
  return(Xlag)
}


# Independent normal-Wishart prior for the VAR ----------------------------------
# specifications
plag <- 2     # number of lags
cons <- TRUE  # include constant?

# Create data matrices
Yraw <- as.matrix(Yraw)
Xraw <- mlag(Yraw, plag) # X's are the lagged values of Y

if(cons) Xraw <- cbind(Xraw, 1) # Note that constant is located after lags of variables

# look at size of data
dim(Yraw)
dim(Xraw)

# first plag rows are zero | conditioning on the first p observations
Y <- Yraw[(plag + 1):nrow(Yraw), ]
X <- Xraw[(plag + 1):nrow(Xraw), ]
y <- as.vector(Y)

dim(Y)
dim(X)
# View(X)

# get useful dimensions
M    <- ncol(Y)           # number of endogenous variables in the VAR
bigT <- nrow(Y)           # sample size, do not use T only as name!
K    <- M * plag          # number of autoregressive coefficients
k    <- ncol(X)           # number of parameters per equation
v    <- M * (M - 1) / 2

# Initial Values, OLS preliminaries, can also just take a draw from the prior distribution ----

A_OLS <- solve(crossprod(X)) %*% crossprod(X, Y)
# A_OLS <- chol2inv(chol(crossprod(X)))%*%crossprod(X,Y) 
# Cholesky to inverse saves computation time
E_OLS <- Y - X %*% A_OLS
S_OLS <- crossprod(E_OLS) / (bigT - K)


# Priors --------------------------------------------

A_prior <- matrix(0, k, M) 
diag(A_prior) <- 1 # prior mean: 1 for first own lags, 0 otherwise. Note that prior mean for
# deterministics is in last row due to ordering of constant in Xraw
A_prior
a_prior <- as.vector(A_prior) # vectorize prior mean matrix

# get AR variances to scale cross-variable lags of Minnesota prior
sigs <- numeric(length = M)
for(mm in 1:M){
  yuse <- Y[, mm, drop=FALSE]
  xuse <- cbind(X[,seq(mm, M * plag, by=M), drop=FALSE], 1)
  b    <- solve(crossprod(xuse)) %*% crossprod(xuse,yuse)
  sigs[mm] <- crossprod(yuse-xuse %*% b) / (bigT - plag - 1)
}

# Minnesota prior 
# own lags:       (lambda1/k)^2   # k == lag
# cross lags:     (sig_i^2/sig_j^2)(lambda1 * lambda2/k)^2
# deterministics: lambda3*sig_i^2
# we have monthly data, so lambda1 and lambda2 are multiplied by three to induce less shrinkage
lambda1 <- 0.3; lambda2 <- 1.5; lambda3 <- 100 
V_prior <- array(0, c(k, k, M))
for(mm in 1:M){ # over all equations
  for(pp in 1:plag){ # over all lags
    for(kk in 1:M){ # over all coefficients
      if(mm == kk){ # own lag
        V_prior[(pp - 1) * M + kk, (pp - 1) * M + kk, mm] <- (lambda1 / pp)^2
      }else{ # cross-lags
        V_prior[(pp - 1) * M + kk, (pp - 1) * M + kk, mm] <- (sigs[mm] / sigs[kk]) * (lambda1 * lambda2 / pp)^2
      }
    }
  }
  V_prior[k, k, mm] <- lambda3 * sigs[mm] # for deterministics (i.e. constant)
}
V_prior    <- lapply(seq(dim(V_prior)[3]), function(x) V_prior[ , , x]) # array to list
V_prior    <- Reduce(magic::adiag, V_prior) # bring in form
V_prior[1:(M*plag+1), 1:(M*plag+1)]
V_priorinv <- diag(1 / diag(V_prior))

# hyperparameters for inverse Wishart
s0 <- M + 2
S0 <- (s0 - M - 1) * diag(sigs)

# initialize draws with OLS estimators
A_draw <- A_OLS
S_draw <- S_OLS

# outside loop calculations
s_post    <- bigT + s0
XX <- crossprod(X)

# MCMC setup ------------------------------------------------------------------------

nsave <- 1000               # number of saved draws
nburn <- 1000               # number of burned draws
ntot  <- nsave + nburn      # number of total draws
nhor  <- 13                 # horizon for IRFs
fhorz <- 8                  # forecasting horizon

# Container for MCMC draws, stored after burn-in
A_store <- array(NA, c(nsave, k, M))
S_store <- array(NA, c(nsave, M, M))

# container for sign restriction attempts
cou_store <- numeric(length = nsave)

# Predictions -- dimensions: number of draws x number of variables x forecasting horizon
yf_store <- array(NA, c(nsave, M, fhorz))

# IRFs -- dimensions: Number of draws x Number of responses x Number of structural shocks x horizon
IRFchol_store <- array(NA,c(nsave, M, M, nhor))
IRFsign_store <- array(NA,c(nsave, M, M, nhor))

# MCMC ------------------------------------------------------------------------
set.seed(1)
for(irep in 1:ntot){
 
  ## Step 1: Draw S_draw | Y, A_draw from IW ------------------------------------------------------------------------
  # s_overbar = T + s_underbar (s0)
  # S_overbar = (Y-XA)'(Y-XA) + S_underbar (S0)
  # SIGMA | Y ~ iW(s_overbar,S_overbar)
  
  S_post    <- crossprod(Y - X %*% A_draw)
  S_drawinv <- matrix(rWishart(1,s_post,solve(S_post)),M,M) 
  # note that we can draw from the Wishart and inverting leads to the inverse-Wishart
  S_draw    <- solve(S_drawinv) 
  
  # Step 2: Draw A_draw | Y, S_draw from multivariate normal ------------------------------------------------------------------------
  # V_overbar = (SIGMAinv otimes X'X + Vinv_underbar)^{-1}
  # a_overbar = V_overbar (Vinv_underbar a_underbar + (SIGMAinv otimes X')y)
  V_post    <- solve(kronecker(S_drawinv, XX) + V_priorinv)
  # Here, using chol2inv(chol(kronecker(S_drawinv, XX) + V_priorinv)) saves time
  A_post    <- V_post %*% (kronecker(S_drawinv, t(X))%*%y + V_priorinv%*%a_prior)
  
  eig_check <- TRUE
  while(eig_check) {
    # Here, using the lower Cholesky factor multiplied by k * M Normal draws creates
    # draws from the multivariate Normal and saves time, compared to drawing from 
    # multivariate Normal directly
    A_draw     <- matrix(A_post + t(chol(V_post)) %*% rnorm(k * M), k, M)
    # stability check using companion form
    Cm <- matrix(0, K, K)
    Cm[1:M,] <- t(A_draw[1:K, ]) # companion matrix excludes deterministic terms
    diag(Cm[(M + 1):K, 1:(M * (plag - 1))]) <- 1
    
    eig_check <- max(abs(Re(eigen(Cm)$values))) > 1
  }
  
  # Step 3: Storage/Predictions/IRFs ------------------------------------------------------------------------
  if(irep > nburn){
    ## Step 3a: Save parameter draws ------------------------------------------------------------------------
    A_store[irep - nburn, , ] <- A_draw
    S_store[irep - nburn, , ] <- S_draw
    
    #Step 3b: Build companion matrix for forecasts and IRFs again (illustration) --------------------------------------------------------
    Cm <- matrix(0, K, K)
    Cm[1:M,] <- t(A_draw[1:K, ]) # companion matrix excludes deterministic terms
    diag(Cm[(M + 1):K, 1:(M * (plag - 1))]) <- 1
    Jm <- matrix(0, K, M)
    diag(Jm) <- 1
    
    # Step 3c: Do predictions and calculate fhorz-step ahead prediction density ------------------------------------------------------------------------
    Mean00  <- c(Y[bigT, ], X[bigT, 1:(M * (plag - 1))]) 
    # take latest values for Y plus p-1 lags from X for forecasts
    Sigma00 <- matrix(0, K, K)
    for(ihorz in 1:fhorz){
      # first and second moments
      Mean00  <- Cm %*% Mean00 # Create mean forecast
      Sigma00 <- Cm %*% Sigma00 %*% t(Cm) + Jm %*% S_draw %*% t(Jm) # update variance of forecasts
      
      # draw forecasts from predictive density
      yf    <- Mean00[1:M] + t(chol(Sigma00[1:M, 1:M])) %*% rnorm(M) 
      yf_store[irep-nburn, , ihorz] <- yf
    }
    
    # Step 3d: Impulse response functions; both Cholesky and sign restrictions ---------
    
    ## Step 3d1: Identification via Cholesky ------------------------------------------------------------------------
    shock.chol <- t(chol(S_draw))
    # Normalise shocks to 1 units
    shock.chol <- diag(1 / diag(shock.chol)) %*% shock.chol
    
    # Step 3d2: Identification via sign-restrictions ------------------------------------------------------------------------
    cond.overall <- TRUE
    counter      <- 0
    MaxTries     <- 1000
    # draw rotation matrices Q till you find a fitting one (while-loop)
    while(cond.overall && counter < MaxTries){
      counter <- counter + 1
      
      # Define a rotation matrix with positive values on the main diagonal
      Rtilda <- matrix(rnorm(M^2, 0, 1), M, M)
      qr.object <- qr(Rtilda)
      Q <- qr.Q(qr.object)
      Q <- Q %*% diag((diag(Q) > 0) - (diag(Q) < 0))
      
      #shock is a full matrix
      shock.chol <- t(chol(S_draw))
      shock.sign <- shock.chol %*% Q
      
      # Monetary Policy shock
      cond.MP <- (shock.sign[1, 3] < 0) * (shock.sign[2, 3] > 0) * (shock.sign[3, 3] > 0)
      # Oil price shock // aggregate supply shock
      cond.AS <- (shock.sign[1, 1] > 0) * (shock.sign[2, 1] > 0) * (shock.sign[3, 1] < 0)
      # Demand shock
      cond.AD <- (shock.sign[1, 2] < 0) * (shock.sign[2, 2] > 0) * (shock.sign[3, 2] < 0)
      
      #Shocks have to be mutually exclusive (orthogonal)
      cond.overall <- (cond.MP * cond.AS * cond.AD)==0
    }
    cou_store[irep - nburn] <- counter
    
    # Step 3e: Compute IRFs ------------------------------------------------------------------------
    # Temporary objects for state space representation
    irf.mat.chol<- irf.mat.sign <- array(NA, c(M, M, nhor))
    
    # Impulse --> shock at t = 0:
    irf.mat.chol[ , , 1] <- shock.chol
    irf.mat.sign[ , , 1] <- shock.sign
    
    #start at t = 1, as t = 0 is the impulse shock
    Cmi <- Cm
    for(ihorz in 2:nhor){
      irf.mat.chol[ , , ihorz] <- t(Jm) %*% Cmi %*% Jm %*% shock.chol
      irf.mat.sign[ , , ihorz] <- t(Jm) %*% Cmi %*% Jm %*% shock.sign
      Cmi <- Cmi %*% Cm
    }
    IRFchol_store[irep - nburn, , , ] <- irf.mat.chol
    IRFsign_store[irep - nburn, , , ] <- irf.mat.sign
  }
  if(irep %% 50 == 0) print(paste0("Round: ", irep, "/", ntot))
}

# Check convergence ------------------------------------------
crit_val <- 1.96
Z_scores <- c()

par(mfrow=c(k,3),mar=c(2,2,1,1))
for(ii in 1:k){
  for(jj in 1:M){
    plot.ts(A_store[,ii,jj])
    abline(h = mean(A_store[,ii,jj]), col = "red", lty = 2)
    abline(h = quantile(A_store[,ii,jj], p = c(0.05, 0.95)), col = "blue")
    Z_scores[(jj-1)*k+ii] <- geweke.diag(A_store[,ii,jj])$z
  }
}

# Autocorrelation of parameter draws for AR coefficients ------------------------------------------
par(mfrow=c(k,3),mar=c(1,1,1,1))
for(ii in 1:k){
  for(jj in 1:M){
    acf(A_store[,ii,jj])
  }
}

par(mfrow=c(3,3),mar=c(2,2,1,1))
for(jj in 1:M){
  for(ii in 1:M){
    plot.ts(S_store[,ii,jj])
    # convergence diagnostics, if not rejected samples have converged
    Z_scores[k*M+(jj-1)*M+ii] <- geweke.diag(S_store[,ii,jj])$z
  }
}

# Autocorrelation of parameter draws for variance coefficients ------------------------------------------
par(mfrow=c(M,M),mar=c(1,1,1,1))
for(jj in 1:M){
  for(ii in 1:M){
    acf(S_store[,ii,jj])
  }
}

idx <- which(abs(Z_scores) > crit_val)
paste(length(idx), " out of ",k*M+M^2, " variables' z-values exceed the 1.96 threshold", " (", round(length(idx)/(k*M+M^2)*100,2),"%)",sep="")

# Quantiles over the first dimension (number of saved draws) ------------------------------------------
IRFchol_low    <- apply(IRFchol_store, c(2,3,4), quantile, 0.16,na.rm=TRUE)
IRFchol_high   <- apply(IRFchol_store, c(2,3,4), quantile, 0.84,na.rm=TRUE)
IRFchol_median <- apply(IRFchol_store, c(2,3,4), median, na.rm=TRUE)

IRFsign_low    <- apply(IRFsign_store, c(2,3,4), quantile, 0.16,na.rm=TRUE)
IRFsign_high   <- apply(IRFsign_store, c(2,3,4), quantile, 0.84,na.rm=TRUE)
IRFsign_median <- apply(IRFsign_store, c(2,3,4), median, na.rm=TRUE)

# Start plotting the IRFs w.r.t. shocks ------------------------------------------
## Cholesky for all demand, supply and financial shock on wheat price ------------------------------------------
par(mfrow=c(3,1),mar=c(4,4,2,2))
for(ii in 4:4){ # Column 4 is price of wheat
  for(jj in 1:(M-1)){ # in the first 3 columns (M-1) are the demand, supply and financial shock
    min1 <- min(IRFchol_low[ii,jj,])
    max1 <- max(IRFchol_high[ii,jj,])
    plot.ts(IRFchol_median[ii,jj,], ylab="", main=paste0("Shock ",colnames(Y)[jj], " on ",colnames(Y)[ii]), ylim = c(min1,max1),xaxt="n",lwd=2,xlab="")
    lines(IRFchol_low[ii,jj,], lty = 2, lwd=2)
    lines(IRFchol_high[ii,jj,], lty = 2, lwd=2)
    abline(h=0,col="red",lwd=2)
    abline(v=seq(1,nhor,by=2), col="lightgrey", lty=2)
    axis(1, at=seq(1,nhor,by=2), labels=seq(0,nhor-1,by=2))
  }
}

## Cholesky for all shocks on all variables ------------------------------------------
par(mfrow=c(4,4),mar=c(4,4,2,2))
for(ii in 1:M){
  for(jj in 1:M){
    min1 <- min(IRFchol_low[ii,jj,])
    max1 <- max(IRFchol_high[ii,jj,])
    plot.ts(IRFchol_median[ii,jj,], ylab="", main=paste0("Shock ",colnames(Y)[jj], " on ",colnames(Y)[ii]), ylim = c(min1,max1),xaxt="n",lwd=2,xlab="")
    lines(IRFchol_low[ii,jj,], lty = 2, lwd=2)
    lines(IRFchol_high[ii,jj,], lty = 2, lwd=2)
    abline(h=0,col="red",lwd=2)
    abline(v=seq(1,nhor,by=2), col="lightgrey", lty=2)
    axis(1, at=seq(1,nhor,by=2), labels=seq(0,nhor-1,by=2))
  }
}

 ## Sign restrictions for all shocks on all variables ------------------------------------------
par(mfrow=c(4,4))
for(ii in 1:M){
  for(jj in 1:M){
    min1 <- min(IRFsign_low[ii,jj,])
    max1 <- max(IRFsign_high[ii,jj,])
    plot.ts(IRFsign_median[ii,jj,], ylab="", xlab="", main=paste0("Shock ",colnames(Y)[jj], " on ",colnames(Y)[ii]), ylim = c(min1,max1), xaxt="n",lwd=2)
    lines(IRFsign_low[ii,jj,], lty = 2, lwd=2)
    lines(IRFsign_high[ii,jj,], lty = 2, lwd=2)
    abline(h=0,col="red",lwd=2)
    abline(v=seq(1,nhor,by=2), col="lightgrey", lty=2)
    axis(1, at=seq(1,nhor,by=2), labels=seq(0,nhor-1,by=2))
  }
}

# Check signs ------------------------------------------
par(mfrow=c(1,1))
hist(cou_store)

# Plotting predictions ------------------------------------------
# yf_low    <- apply(yf_store, c(2,3), quantile, 0.16, na.rm=TRUE)
# yf_median <- apply(yf_store, c(2,3), quantile, 0.50, na.rm=TRUE)
# yf_high   <- apply(yf_store, c(2,3), quantile, 0.84, na.rm=TRUE)
# 
# yf_low    <- cbind(t(Yraw[(bigT-20):bigT,]),yf_low)
# yf_median <- cbind(t(Yraw[(bigT-20):bigT,]),yf_median)
# yf_high   <- cbind(t(Yraw[(bigT-20):bigT,]),yf_high)
# 
# xax <- c(as.character(time[(bigT-20):bigT]),paste0("t+",seq(1,fhorz)))
# 
# par(mfrow=c(3,1),mar=c(5,3,2,3))
# for(ii in 1:M){
#   min1 <- min(yf_low[ii,])
#   max1 <- max(yf_high[ii,])
#   plot.ts(yf_median[ii,], ylim=c(min1,max1), main=colnames(Y)[[ii]], ylab="", xlab="", xaxt="n",lwd=2)
#   lines(yf_low[ii,], lty=2,lwd=2)
#   lines(yf_high[ii,], lty=2,lwd=2)
#   lines(yf_median[ii,], lty=1,lwd=2)
#   axis(1, at=seq(1,29), labels=xax, las=2)
#   abline(v=seq(1,29), col="lightgrey", lty=2)
# }

