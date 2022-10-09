## Question 3 

## Read data
library(readr)
library(dplyr)
library(ggplot2)
FVX <- read_csv("Downloads/FVX.csv", col_types = cols(Open = col_number(), 
                                                          High = col_number(), Low = col_number(), 
                                                           Close = col_number(), `Adj Close` = col_number(), 
                                                           Volume = col_number()))

## data cleaning
FVX = as.matrix(FVX[-which(is.na(FVX[,2])),2:7]) ## delete the row when market is not open

R_t = (FVX[,5]-FVX[,1])/(FVX[,1])

n = length((FVX[,2]))

log_R_t = log(FVX[,5]/FVX[,1])

plot(log_R_t)
plot(R_t)

## estimation
Est = log_R_t[seq(n-19,n)]
Pre = log_R_t[-seq(n-19,n)]

X1 = Pre[2:5527]
X2 = Pre[1:5526]
y  =  Pre[3:5528]
n_X = length(X1)
iota = rep(1,n_X)
X = cbind(iota,X1,X2)
epsilon = rnorm(length(X))
y = y
sigma2 = var(Pre)


## OLS
b_ols = solve(t(X)%*%X)%*%t(X)%*%y
print(b_ols)

## MLE
negloglik = function(par){
  neg = (n_X/2)*log(2*pi*sigma2) + ( 1/(2*sigma2) )*sum( (y-(par[1]+par[2]*X1+par[3]*X2))^2 )
  return(neg)
}

result = optim(c(1,1,1),negloglik)
print(result$par)

## AR
library(tseries)
result_ts = tseries::arma(y,c(2,0))
result_ts$coef

## 
print(b_ols)
print(result$par)
print(result_ts$coef)

## estimation
iota_e = rep(1,18)
X_e = cbind(iota_e,Est[1:18],Est[2:19])
## ols
est_ols = X_e%*%b_ols
e_ols = Est[3:20] - est_ols
ssr_ols = sum((Est[3:20] - est_ols)^2)
## MLE
est_MLE = X_e%*%result$par
e_MLE = Est[3:20] - est_MLE
ssr_MLE = sum((est_MLE - Est[3:20])^2)
## AR(2)
coef_AR = c(result_ts$coef[3],result_ts$coef[1:2])
est_AR = X_e%*%coef_AR
e_AR = Est[3:20] - est_AR
ssr_AR = sum((est_MLE - Est[3:20])^2)

## 
plot(e_ols)
plot(e_MLE)
plot(e_AR)
##
print(shapiro.test(e_ols))
print(shapiro.test(e_MLE))
print(shapiro.test(e_AR))

## 20 days forward
forecast = function(par,step,start){
  mat = matrix(0, nrow = step+1, ncol = 3)
  mat[,1] = 1
  mat[1,2:3] = start
  est = rep(0,step)
  for (i in 1:step) {
    est[i] = mat[i,]%*%par
    mat[i+1,2:3] = c(est[i],mat[i,2])
  }
  return(est)
}

est_20_ols = forecast(b_ols, 20, Pre[5527:5528])
est_20_MLE = forecast(result$par, 20, Pre[5527:5528])
est_20_AR = forecast(coef_AR, 20, Pre[5527:5528])

plot(Est,type = 'l', col = 'red',xlab = 'Times', ylab = 'Estimated log-return')
lines(est_20_MLE, col = 'green')
lines(est_20_ols, col = 'blue')
lines(est_20_AR)

## Question 4
## import
X_AEX <- read_csv("Downloads/^AEX.csv", col_types = cols(Date = col_character(), 
                                                              Open = col_number(), High = col_number(), 
                                                              Low = col_number(), Close = col_number(), 
                                                              `Adj Close` = col_number(), Volume = col_number()))
## cleaning
AEX = as.matrix(X_AEX[-which(is.na(X_AEX[,2])),2:7])
n_AEX = length(AEX[,2])
##log return
log_R_t_AEX = log(AEX[,5] / AEX[,1])
log_R_t2_AEX = ( log(AEX[,5] / AEX[,1]) )^2
## plot
plot(log_R_t_AEX)
plot(log_R_t2_AEX)
hist(log_R_t_AEX,breaks = 100)
hist(log_R_t2_AEX,breaks = 100)

## Pre and Est
Pre_AEX = log_R_t_AEX[5625:5644]
Est_AEX = log_R_t_AEX[1:5624]

## MLE
var = var(Pre_AEX)

negloglik_AEX = function(par){
  
}