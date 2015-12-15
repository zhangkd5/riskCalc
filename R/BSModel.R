callputflag <- function(callput) {
  if(is.numeric(callput)) return(callput)
  callput <- pmatch(tolower(callput), c("call", "put"))
  ifelse(callput==1L, 1, -1)
}

BSPrice <- function(price, strike, rate, vol, TTM, callput=1, divRate=0)
{
  callput <- callputflag(callput)
  d1 = (log(price/strike) + (rate-divRate+vol^2/2)*TTM)/(vol*sqrt(TTM))
  d2 = d1 - vol*sqrt(TTM)
  return(callput*(price*exp(-divRate*TTM)*pnorm(callput*d1)-strike*exp(-rate*TTM)*pnorm(callput*d2)))
}

BSDelta <- function(price, strike, rate, vol, TTM, callput=1, divRate=0)
{
  callput <- callputflag(callput)
  d1 = (log(price/strike) + (rate-divRate+vol^2/2)*TTM)/(vol*sqrt(TTM))
  return(callput*exp(-divRate*TTM)*pnorm(callput*d1))
}

BSGamma <- function(price, strike, rate, vol, TTM, callput=1, divRate=0)
{
  callput <- callputflag(callput)
  d1 = (log(price/strike) + (rate-divRate+vol^2/2)*TTM)/(vol*sqrt(TTM))
  return(exp(-divRate*TTM)*dnorm(callput*d1)/(price*vol*sqrt(TTM)))
}

BSTheta <- function(price, strike, rate, vol, TTM, callput=1, divRate=0)
{
  callput <- callputflag(callput)
  d1 = (log(price/strike) + (rate-divRate+vol^2/2)*TTM)/(vol*sqrt(TTM))
  d2 = d1 - vol*sqrt(TTM)
  return(-exp(-divRate*TTM)*price*dnorm(d1)*vol/(2*sqrt(TTM)) - 
           callput*rate*strike*exp(-rate*TTM)*pnorm(callput*d2) +
           callput*divRate*price*exp(-divRate*TTM)*pnorm(callput*d1))
}



# price=200
# strike=190
# rate=0.01
# vol=0.25
# TTM=1
# callput=1
# divRate=0.02
# 
# TT = 5/252
# mu =0.09
# sigma = 0.2
# 
# Z = rnorm(10000)
# St = price * exp((mu-0.5*sigma^2)*TT + sigma*sqrt(TT)*Z)
# O0 = BSPrice(price, strike, rate, vol, TTM, callput, divRate)
# Ot = BSPrice(St, strike, rate, vol, TTM-TT, callput, divRate)
# PNL = Ot-O0
# dO = (BSTheta(price, strike, rate, vol, TTM, callput, divRate)) * TT +
#   (BSDelta(price, strike, rate, vol, TTM, callput, divRate)-
#      BSGamma(price, strike, rate, vol, TTM, callput, divRate)*price) * (St-price) +
#   0.5*BSGamma(price, strike, rate, vol, TTM, callput, divRate) * (St^2-price^2)
# dO2 = (BSTheta(price, strike, rate, vol, TTM, callput, divRate)) * TT +
#   (BSDelta(price, strike, rate, vol, TTM, callput, divRate)) * (St-price)
# dO3 = (BSDelta(price, strike, rate, vol, TTM, callput, divRate)-
#      BSGamma(price, strike, rate, vol, TTM, callput, divRate)*price) * (St-price) +
#   0.5*BSGamma(price, strike, rate, vol, TTM, callput, divRate) * (St^2-price^2)
# 
# data = data.frame(Z, St, PNL, dO, dO2, dO3)
# data$iter = seq(nrow(data))
# ggplot(data, aes(x=iter, y=PNL)) + geom_line(aes(color="TRUE")) + geom_line(aes(y=dO3, color="FACTOR")) +
#   geom_line(aes(y=PNL-dO3, color="diff"))
# quantile(PNL, prob=0.01, type=4)
# quantile(dO, prob=0.01, type=4)
# quantile(dO2, prob=0.01, type=4)
# quantile(dO3, prob=0.01, type=4)
