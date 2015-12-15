#' Calculate VaR with Various Methods
#' 
#' This generic function computes VaR and creates an S3 class object that contains data and VaR results.
#' 
#' @param data an object that contains structured data of stocks, options, or a portfolio.
#' @param TimeLen the time horizon in days that the VaR is defined over.
#' @param conf.level the confidence level of VaR
#' @param method the method used to calculate VaR. Must be one of \code{"parametric"} (default), 
#' \code{"historical"}, or \code{"Monte-Carlo"}, can be abbreviated.
#' @param lookback.years the time horizon in years that historical simulation is based on.
#' @param iteration the number of iteration Monte-Carlo simulation to be performed.
#' @param ... arguments to be passed to or from methods.
#' @param V0 the total value which VaR is measured upon.
#'  
#' @return 
#' An object of class \code{"VaR"}.
#' 
#' @seealso
#' \code{\link{backtest}} to perform VaR backtesting.
#' 
#' @details
#' This function is able to compute portfolio VaR for portfolio data, and is also able to 
#' compute VaR for each individual stocks and/or options given stocks or options data. 
#' 
#' In parametric method and Monte-Carlo method, the portfolio components should be firstly calibrated
#' to a GBM model. In parametric method, the portfolio is assumed normally distributed and the options are
#' linearly approximated by underlying stocks. In historical and Monte-Carlo method, 
#' the option values are fully evaluated.
#' 
#' See model documentation for details about VaR calculation methods.
#' 
#' @export
VaR <- function(data, ...)
  UseMethod("VaR")

#' @rdname VaR
#' @export
VaR.portfolioData <- function(data, TimeLen=5, conf.level=0.99, 
                              method=c("parametric", "historical", "Monte-Carlo"), 
                              lookback.years=5, iteration=10000, ...)
{
  method <- match.arg(method)
  if(method!="historical" && !inherits(data, "GBM"))
    stop(gettextf("method %s requires to firstly fit GBM model to data", deparse(method)))
  
  w.raw <- matrix(data$compWeight, nrow=data$N, ncol=data$DimPf, byrow=TRUE)
  if(attr(data$compWeight, "unit")=="share") {
    for(i in seq_along(data$compNames)) {
      if(data$compType[i]=="stock")
        w.raw[,i] <- w.raw[,i] * data$price[,data$compNames[i]]
      else
        w.raw[,i] <- w.raw[,i] * data$premium[,data$compNames[i]]
    }
  }
  w.raw <- w.raw / rowSums(abs(w.raw)) * data$V0
  
  if(method=="parametric") 
  {
    opNames <- data$compNames[data$compType=="option"]
    rawIdx <- match(data$stockNames, data$compNames)    
    w <- w.raw[,rawIdx,drop=FALSE]
    w[,is.na(rawIdx)] <- 0
    for(Name in opNames) {
      idx <- match(Name, data$optionNames)
      und <- data$underlying[idx]
      delta <- BSDelta(data$price[,und], data$strike[,Name], data$rate[,2], 
                       data$impVol[,Name], data$maturity[idx], data$callput[idx], data$divRate[,und])
      i.w <- match(und, data$stockNames)
      i.raw <- match(Name, data$compNames)
      w[,i.w] <- w[,i.w] + delta * data$price[,und] * w.raw[,i.raw] / data$premium[,Name]
    }
    VaR <- as.numeric(ComputeVaR.PF(TimeLen/252, conf.level, data.matrix(data$mu[,-1,drop=FALSE]), 
                                    data.matrix(data$vol[,-1,drop=FALSE]), corr=data$corr, w))
  }
  else if(method=="historical") {
    lookback <- lookback.years * 252
    if(lookback > data$N)
      stop("cannot compute historical VaR because of insufficient data")
    
    stkRet <- matrix(nrow=data$N, ncol=data$Dim, dimnames=list(NULL, data$stockNames))
    stkRet[seq(data$N-TimeLen),] <- 
      -diff(log(data.matrix(data$price[,-1,drop=FALSE])), lag=TimeLen)
    
    VaR <- c(as.numeric(sapply(seq(data$N-lookback+1), function(nr) {
      Vhist <- numeric(lookback)
      for(i in seq_along(data$compNames)) {
        if(data$compType[i]=="stock") {
          Vhist <- Vhist + w.raw[nr,i] * (exp(stkRet[nr:(nr+lookback-1),data$compNames[i]]))
        } else { ## option
          idx <- match(data$compNames[i], data$optionNames)
          und <- data$underlying[idx]
          undPrice <- data$price[nr,und] * exp(stkRet[nr:(nr+lookback-1),und])
          newPremium <- BSPrice(undPrice, data$strike[nr,idx+1], data$rate[nr,2], data$impVol[nr,idx+1],
                                data$maturity[idx]-TimeLen/252, data$callput[idx], data$divRate[nr,und])
          Vhist <- Vhist + w.raw[nr,i] * (newPremium / data$premium[nr,idx+1])
        }
      }
      sum(w.raw[nr,]) - quantile(Vhist, prob=1-conf.level, type=4, na.rm=TRUE)
    })), rep(NA, lookback-1))
  }
  else { # Monte-Carlo
    VaR <- numeric(data$N)
    for(nr in seq(data$N)) {
      stkRet <- structure(SimulateRet(TimeLen/252, data$mu[nr,-1], data$vol[nr,-1], 
                                      data$corr[[nr]], iteration), dimnames=list(NULL, data$stockNames))
      VSim <- numeric(iteration)
      for(i in seq_along(data$compNames)) {
        if(data$compType[i]=="stock") {
          VSim <- VSim + w.raw[nr,i] * exp(stkRet[,data$compNames[i]])
        } else {
          idx <- match(data$compNames[i], data$optionNames)
          und <- data$underlying[idx]
          undPrice <- data$price[nr,und] * exp(stkRet[,und])
          newPremium <- BSPrice(undPrice, data$strike[nr,idx+1], data$rate[nr,2], data$impVol[nr,idx+1],
                                data$maturity[idx]-TimeLen/252, data$callput[idx], data$divRate[nr,und])
          VSim <- VSim + w.raw[nr,i] * (newPremium / data$premium[nr,idx+1])
        }
      }
      VaR[nr] <- sum(w.raw[nr,]) - quantile(VSim, prob=1-conf.level, type=4, na.rm=TRUE)
    }
  }
  VaR <- data.frame(Date=data$price[,1], VaR=VaR)
  r <- structure(list(data=data, VaR=VaR, TimeLen=TimeLen, conf.level=conf.level, method=method), class="VaR")
  if(method=="historical") r$lookback.years <- lookback.years
  if(method=="Monte-Carlo") r$iteration <- iteration
  return(r)
}

#' @rdname VaR
#' @export
VaR.optionsData <- function(data, TimeLen=5, conf.level=0.99, 
                            method=c("parametric", "historical", "Monte-Carlo"), V0=10000, 
                            lookback.years=5, iteration=10000, ...)
{
  r <- NextMethod()
  for(Name in data$optionNames) {
    pf <- portfolio(data, Name, 1, V0)
    r$VaR[,Name] <- VaR(pf, TimeLen, conf.level, method, lookback.years, iteration)$VaR[,2]
  }
  return(r)
}

#' @rdname VaR
#' @export
VaR.stocksData <- function(data, TimeLen=5, conf.level=0.99, 
                           method=c("parametric", "historical", "Monte-Carlo"), V0=10000, 
                           lookback.years=5, iteration=10000, ...)
{
  method <- match.arg(method)
  VaR <- data$price[,1,drop=FALSE]
  for(Name in data$stockNames) {
    pf <- portfolio(data, Name, 1, V0)
    VaR[,Name] <- VaR(pf, TimeLen, conf.level, method, lookback.years, iteration)$VaR[,2]
  }
  data$V0 <- V0
  r <- structure(list(data=data, VaR=VaR, TimeLen=TimeLen, conf.level=conf.level, method=method), class="VaR")
  if(method=="historical") r$lookback.years <- lookback.years
  if(method=="Monte-Carlo") r$iteration <- iteration
  return(r)
}


# 
# ComputeVaR.GBM <- function(TimeLen, p, mu, vol, V0) {
#   stopifnot(p>0, p<1, TimeLen>0, na.omit(vol>=0))
#   n = max(length(TimeLen), length(p), length(mu), length(vol), length(V0))
#   TimeLen = rep(TimeLen, length.out = n)
#   p = rep(p, length.out = n)
#   mu = rep(mu, length.out = n)
#   vol = rep(vol, length.out = n)
#   V0 = rep(V0, length.out = n)
#   
#   return(V0 - V0 * exp(vol*sqrt(TimeLen)*qnorm(1-p) + (mu-0.5*vol^2)*TimeLen))
# }

# SimulateVaR.GBM.Option <- function(TimeLen, p, mu, vol, 
#                                    price, strike, rate, maturity, ImpVol, callput=1, divRate=0,
#                                    V0=1, w.stock=0, w.option=1, iter=10000)
# {
#   stopifnot(TimeLen>0, p>0, p<1, all(vol>0|is.na(vol)), all(ImpVol>0|is.na(ImpVol)))
#   n = length(mu)
#   nS = V0 * w.stock / price
#   P0 = BSPrice(price, strike, rate, ImpVol, maturity, callput, divRate)
#   nP = V0 * w.option / P0
#   q = numeric(n)
#   for(i in seq(n)) {
#     St = price[i] * exp((mu[i]-vol[i]^2/2)*TimeLen + vol[i]*sqrt(TimeLen)*rnorm(iter))
#     Pt = BSPrice(St, strike[i], rate[i], ImpVol[i], maturity-TimeLen, callput, divRate[i])
#     Vt.P = nS[i] * St + nP[i] * Pt
#     q[i] = V0 - quantile(Vt.P, prob=1-p, type=4, na.rm=TRUE)
#   }
#   return(q)
# }

ComputeVaR.PF <- function(TimeLen, p, mu, vol, corr, w)
{
  n <- nrow(mu)
  if(is.vector(w, "numeric") && length(w)==ncol(mu))
    w <- t(w)
  w <- as.matrix(w)
  w <- w[rep(seq(nrow(w)),length.out=n),,drop=FALSE]
  #  w <- w / rowSums(abs(w)) * V0
  if(!is.list(corr)) corr <- rep(list(corr),length.out=n)
  V0 <- rowSums(w)
  ESt <- exp(mu*TimeLen)
  EVt <- rowSums(ESt * w)
  VarVt <- sapply(seq(n), function(nr) {
    w[nr,,drop=FALSE] %*% ((t(ESt[nr,,drop=FALSE]) %*% ESt[nr,,drop=FALSE]) * 
                             (exp((t(vol[nr,,drop=FALSE]) %*% vol[nr,,drop=FALSE])*corr[[nr]]*TimeLen)-1)) %*%
      t(w[nr,,drop=FALSE])
  })
  V0 - (EVt - sqrt(VarVt) * qnorm(p))
}

#' @import mvtnorm
SimulateRet <- function(TimeLen, mu, vol, corr, iter)
{
  mu <- as.numeric(mu)
  vol <- as.numeric(vol)
  if(any(is.na(corr)))
    return(matrix(NA_real_, nrow=iter, ncol=length(mu)))
  Z <- rmvnorm(iter, sigma = corr)
  t((mu-vol^2)*TimeLen + vol*sqrt(TimeLen)*t(Z))
}
