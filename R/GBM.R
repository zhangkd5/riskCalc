#' GBM Model Fitting
#' 
#' A generic function which is the creator for an S3 class of multivariate GBM model.
#' 
#' @param ... arguments to be passed to methods.
#' @param drift a vector or matrix, which manually sets the drift of the model.
#' @param volatility a vector or matrix, which manually sets the volatility.
#' @param S0 numerical vecotr, the spot price.
#' @param corr a list of correlation matrices.
#' @param Names a character vector that specifies the symbols of the stocks.
#' @param data an object of class or inherited from \code{stocksData}.
#' @param len the window size in years.
#' @param lambda the decay factor used by exponential weighting method. 
#' If \code{NULL}, the function will use the equivalence of \code{len}.
#' @param method a character string indicating calibration method to be used.
#' Must be \code{"unweighted"} (default) or \code{"exp-weighted"}, can be abbreviated.
#' 
#' @return An object of class \code{"GBM"} which contains the data and the parameters.
#' 
#' @details
#' This generic function is the mid-layer of the risk calculation system, which performs 
#' model calibration based on historical data and parameter specification. 
#' See model documentation for details. 
#' 
#' @seealso \code{\link{stocks}}, \code{\link{addOptions}}, \code{\link{portfolio}}. 
GBM <- function(...) UseMethod("GBM")

#' @rdname GBM
#' @export
GBM.default <- function(drift, volatility, S0, corr=NULL, Names=NULL) {
  if(is.vector(drift, "numeric"))
    drift <- t(drift)
  else
    drift <- as.matrix(drift)
  if(is.vector(volatility, "numeric"))
    volatility <- t(volatility)
  else
    volatility <- as.matrix(volatility)
  if(is.vector(S0, "numeric"))
    S0 <- t(S0)
  else
    S0 <- as.matrix(S0)
  D <- ncol(drift)
  n <- nrow(drift)
  if(is.null(corr)) corr <- rep(0, n)
  if(is.vector(corr, "numeric")) 
    corr <- lapply(corr, function(rho, D) diag(rep(1-rho, D))+rho, D=D)
  corr <- rep(corr, length.out=n)
  r <- stocks(S0, Names=Names, date.col=NULL, price.col=seq(D))
  colnames(drift) <- colnames(volatility) <- r$stockNames
  corr <- lapply(corr, function(m){dimnames(m)<-rep(list(r$stockNames),2);m})
  r$mu <- data.frame(Date=n:1, drift)
  r$vol <- data.frame(Date=n:1, volatility)
  r$corr <- corr
  class(r) <- unique(c("GBM", class(r)))
  return(r)
}

#' @rdname GBM
#' @import plyr
#' @export
GBM.stocksData <- function(data, len = 5, lambda = NULL, 
                           method = c("unweighted", "exp-weighted"))
{
  method <- match.arg(method)
  D <- data$Dim
  if(method == "exp-weighted") {
    if(is.null(lambda)) 
      lambda <- 0.25^(1/(len*252))
    else len <- log(0.25, lambda) / 252
  }
  if(D == 0) {
    r <- data
    r[c("mu", "vol", "corr")] <- list(mu=data.frame(), vol=data.frame(), corr=list())
    class(r) <- unique(c("GBM", class(r)))
    r$method <- method
    r$len <- len
    r$lambda <- lambda
    return(r)
  }
  Date <- data$logReturn[,1]
  logR <- data.matrix(data$logReturn[,-1,drop=FALSE])
  if(method == "exp-weighted") {
    l <- alply(logR, 2, computeDriftAndVolEWMA, lambda=lambda, .dims=TRUE)
    corr <- computeCorrEWMA(logR, lambda)
  }
  else {
    l <- alply(logR, 2, computeDriftAndVol, len=len, .dims=TRUE)
    corr <- computeCorr(logR, len)
  }
  
  mu <- do.call(cbind, c(list(data.frame(Date)), lapply(l, function(x)x[,1])))
  vol <- do.call(cbind, c(list(data.frame(Date)), lapply(l, function(x)x[,2])))
  
  names(corr) <- Date
  
  r <- data
  r[c("mu", "vol", "corr")] <- list(mu, vol, corr)
  class(r) <- unique(c("GBM", class(r)))
  r$method <- method
  r$len <- len
  r$lambda <- lambda
  return(r)
}

#' @rdname GBM
#' @export
GBM.portfolioData <- function(data, len = 5, lambda = NULL, 
                              method = c("unweighted", "exp-weighted"))
{
  r <- NextMethod()
  if(r$method == "exp-weighted") {
    l <- computeDriftAndVolEWMA(r$pfReturn[,2], lambda=r$lambda)
  }
  else {
    l <- computeDriftAndVol(r$pfReturn[,2], len=r$len)
  }
  r$pfMu <- data.frame(Date=r$price[,1], mu=l[,1])
  r$pfVol <- data.frame(Date=r$price[,1], vol=l[,2])
  class(r) <- unique(c("GBM", class(r)))
  return(r)
}

#' @rdname subset.stocksData
#' @export
subset.GBM <- function(x, select, ...)
{
  r <- NextMethod()
  select <- r$stockNames
  Date <- r$price[,1,drop=FALSE]
  r$mu <- cbind(Date, subset(x$mu[,-1,drop=FALSE], select=select))
  r$vol <- cbind(Date, subset(x$vol[,-1,drop=FALSE],select=select))
  r$corr <- lapply(x$corr, function(m) m[select,select,drop=FALSE])
  class(r) <- unique(c("GBM", class(r)))
  return(r)
}

computeDriftAndVol <- function(logR, len)
{
  n <- length(logR)
  l <- 252 * len
  if(n < l) {
    warning("No calibration performed because of insufficient data.")
    return(matrix(NA_real_, nrow=n, ncol=2, dimnames=list(NULL,c("mu","vol"))))
  }
  #  system.time(ElogR <- c(rollapply(logR, l, mean, na.rm=TRUE), rep(NA, l-1)))
  ElogR <- c(sapply(seq(n-l+1), function(nr) mean(logR[nr:(nr+l-1)], na.rm=TRUE)), 
             rep(NA, l-1))
  #  system.time(sdlogR <- c(rollapply(logR, l, sd, na.rm=TRUE), rep(NA, l-1)))
  sdlogR <- c(sapply(seq(n-l+1), function(nr) sd(logR[nr:(nr+l-1)], na.rm=TRUE)), 
              rep(NA, l-1))
  mu <- 252 * (ElogR + sdlogR^2 / 2)
  vol <- sdlogR * sqrt(252)
  data.matrix(data.frame(mu, vol))
}

computeDriftAndVolEWMA <- function(logR, lambda)
{
  n <- length(logR)
  l <- round(log(0.25)/log(lambda))
  if(n < l) {
    warning("No calibration performed because of insufficient data.")
    return(matrix(NA_real_, nrow=n, ncol=2, dimnames=list(NULL,c("mu","vol"))))
  }
  ElogR <- ESqlogR <- rep(NA_real_, n)
  ElogR[n-l+1] <- sum(logR[(n-l+1):n] * lambda^seq(0,l-1) * (1-lambda)/(1-lambda^l), na.rm=TRUE)
  ESqlogR[n-l+1] <- sum(logR[(n-l+1):n]^2 * lambda^seq(0,l-1) * (1-lambda)/(1-lambda^l), na.rm=TRUE)
  
  if(n > l) {
    for(nr in (n-l):1) 
      ElogR[nr] <- ifelse(!is.na(logR[nr]), (1-lambda)*logR[nr] + lambda*ElogR[nr+1], ElogR[nr+1])
    for(nr in (n-l):1) 
      ESqlogR[nr] <- ifelse(!is.na(logR[nr]), (1-lambda)*logR[nr]^2+lambda*ESqlogR[nr+1], ESqlogR[nr+1])
  }
  
  sdlogR <- sqrt(ESqlogR - ElogR^2)
  mu <- 252 * (ElogR + sdlogR^2 / 2)
  vol <- sdlogR * sqrt(252)
  data.matrix(data.frame(mu, vol))
}

computeCorr <- function(logR, len)
{
  D <- ncol(logR)
  n <- nrow(logR)
  l <- 252 * len
  if(D == 1) {
    return(rep(list(matrix(1, dimnames=rep(list(colnames(logR)),2))), n))
  }
  if(n < l) {
    warning("No calibration performed because of insufficient data.")
    return(rep(list(matrix(NA_real_, nrow=D, ncol=D, dimnames=rep(list(colnames(logR)),2))), n))
  }
  rho <- c(lapply(seq(n-l+1), function(nr) unname(cor(logR[nr:(nr+l-1),], use="complete.obs"))),
    rep(list(matrix(NA_real_, nrow=D, ncol=D)), l-1))
  rho <- lapply(rho, function(m) {dimnames(m) <- rep(list(colnames(logR)),2); m})
  return(rho)
}

computeCorrEWMA <- function(logR, lambda) 
{
  n <- nrow(logR)
  D <- ncol(logR)
  l <- round(log(0.25)/log(lambda))
  if(D == 1) {
    return(rep(list(matrix(1, dimnames=rep(list(colnames(logR)),2))), n))
  }
  if(n < l) {
    warning("No calibration performed because of insufficient data.")
    return(rep(list(matrix(NA_real_, nrow=D, ncol=D, dimnames=rep(list(colnames(logR)),2))), n))
  }
  ElogR <- t(aaply(logR, 2, function(logR) {
    ElogR <- rep(NA_real_, n)
    ElogR[n-l+1] <- sum(logR[(n-l+1):n] * lambda^seq(0,l-1) * (1-lambda)/(1-lambda^l), na.rm=TRUE)
    if(n > l) {
      for(nr in (n-l):1) 
        ElogR[nr] <- ifelse(!is.na(logR[nr]), (1-lambda)*logR[nr], 0) + lambda*ElogR[nr+1]
    }
    ElogR
  }))
  logR[is.na(logR)] <- 0
  ERi.Rj <- rep(list(matrix(NA_real_, nrow=D, ncol=D)), n)
  ERi.Rj[[n-l+1]] <- 
    t(logR[(n-l+1):n,,drop=FALSE]) %*% (logR[(n-l+1):n,,drop=FALSE] * lambda^seq(0,l-1)) * 
    (1-lambda)/(1-lambda^l)
  if(n > l) {
    for(nr in (n-l):1) 
      ERi.Rj[[nr]] <- (1-lambda)*(t(logR[nr,,drop=FALSE]) %*% logR[nr,,drop=FALSE]) + lambda*ERi.Rj[[nr+1]]
  }
  covR <- lapply(seq(n), function(nr) ERi.Rj[[nr]] - (t(ElogR[nr,,drop=FALSE]) %*% ElogR[nr,,drop=FALSE]))
  rho <- lapply(seq(n), function(nr) if(all(!is.na(covR[[nr]]))) cov2cor(covR[[nr]]) else covR[[nr]])
  rho <- lapply(rho, function(m) {dimnames(m) <- rep(list(colnames(logR)),2); m})
  return(rho)
}
