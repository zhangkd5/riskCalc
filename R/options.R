#' Build Options Dataset
#' 
#' This function is the creator for an S3 class of structured dataset on options.
#' 
#' @param stocksData a \code{stocksData} object, the stocks dataset that contains underlying information.
#' @param optionNames a character vector, the symbols of the options.
#' @param underlyingNames a character vector of the same length as \code{optionNames}, which specifies the underlying of each option.
#' @param Date a Date vector, which specifies the dates of options data.
#' @param impVol the implied volatility of each option for each date. 
#' @param strike,moneyness a numerical vector that specifies the strike or moneyness of the options.
#' @param callput a character vector which can either take \code{"call"} or \code{"put"}. 
#' @param maturity time to maturity of the options.
#' 
#' @return An object of class \code{optionsData}, which stores the structured data of options and stocks.
#' 
#' @details 
#' This function builds an options dataset by organizing raw options data and underlying stocks data 
#' into a structured form.
#' If \code{optionsNames==NULL}, the options will be automatically named as \code{"OP1"}, \code{"OP2"}, and so on. 
#' If \code{Date==NULL}, the options data is regarded as in reverse chronological order with the same dates as in \code{stocksData}.
#' 
#' @seealso \code{\link{stocks}} for building \code{stocksData} objects.
#' 
#' @export
addOptions <- function(stocksData, optionNames=NULL, underlyingNames,
                    Date=NULL, impVol, strike, moneyness, callput, maturity)
{
  n <- stocksData$N
  m <- length(underlyingNames)
  underlyingNames <- match.arg(underlyingNames, stocksData$stockNames, several.ok=TRUE)
  if(length(underlyingNames) < m)
    stop("underlyingNames has ambiguous or unknown stock names")
  if(length(callput) == 1)
    callput <- rep(callput, length.out=m)
  callput <- match.arg(tolower(callput), c("call","put"), several.ok=TRUE)

  if(missing(strike) && missing(moneyness))
    stop("strike and moneyness cannot be missing at the same time")
  if(missing(strike)) {
    strike <- t(t(data.matrix(stocksData$price[,underlyingNames,drop=FALSE])) * moneyness)
  } else
    strike <- matrix(rep(strike, length.out=m), nrow=n, ncol=m, byrow=TRUE)
  strike <- data.frame(Date=stocksData$price[,1], strike)
  
  if(is.vector(impVol,"numeric")) {
    if(m == 1) 
      impVol <- as.matrix(impVol)
    else impVol <- t(impVol)
  }
  if(!is.null(Date)) {
    impVol <- merge(stocksData$price[,1,drop=FALSE], data.frame(Date = as.Date(Date), impVol),
                    by="Date", all.x=TRUE, all.y=FALSE, sort=FALSE)
  } else
    impVol <- data.frame(Date=stocksData$price[,1], impVol)
  
#   if(length(rate) == 1)
#     rate <- data.frame(Date=stocksData$price[,1], rate=rep(rate, length.out=n))
#   else if(!is.null(Date))
#   {
#     rate <- merge(stocksData$price[,1,drop=FALSE], data.frame(Date = as.Date(Date), rate=rate),
#                   by="Date", all.x=TRUE, all.y=FALSE, sort=FALSE)
#   } else
#     rate <- data.frame(Date=stocksData$price[,1], rate=rate)
  
#   if(is.vector(divRate,"numeric")) {
#     divRate <- matrix(rep(divRate, length.out=m), nrow=n, ncol=m, byrow=TRUE)
#     divRate <- data.frame(Date=stocksData$price[,1], divRate)
#   } else if(!is.null(Date)) {
#     divRate <- merge(stocksData$price[,1,drop=FALSE], data.frame(Date = as.Date(Date), divRate),
#                      by="Date", all.x=TRUE, all.y=FALSE, sort=FALSE)
#   } else
#     divRate <- data.frame(Date=stocksData$price[,1], divRate)
  
  maturity <- rep(maturity, length.out=m)
  
#   if(nrow(impVol) != n) {
#     warning("number of rows of impVol inconsistent with stockData: may be truncated or reused")
#     impVol <- impVol[rep(seq(nrow(impVol)), length.out=n),]
#   }
#   if(nrow(rate) != n) {
#     warning("length of rate inconsistent with stockData: may be truncated or reused")
#     rate <- rate[rep(seq(nrow(rate)), length.out=n),]
#   }
#   if(nrow(divRate) != n) {
#     warning("number of rows of divRate inconsistent with stockData: may be truncated or reused")
#     divRate <- divRate[rep(seq(nrow(divRate)), length.out=n),]
#   }
  if(is.null(optionNames))
    optionNames <- paste0("OP", seq(m))
#   colnames(impVol)[-1] <- colnames(divRate)[-1] <- colnames(strike)[-1] <- optionNames
  colnames(impVol)[-1]  <- colnames(strike)[-1] <- optionNames
  premium <- data.frame(Date=stocksData$price[,1])
#   opReturn <- data.frame(Date=stocksData$price[,1])
  for(i in seq_along(optionNames)) {
    premium[,optionNames[i]] <- BSPrice(stocksData$price[,underlyingNames[i]], 
                                        strike[,optionNames[i]], 
                                        stocksData$rate[,2], impVol[,optionNames[i]], 
                                        maturity[i], callput[i], 
                                        stocksData$divRate[,underlyingNames[i]])
#     opReturn[,optionNames[i]] <- c(log(BSPrice(stocksData$price[-n,underlyingNames[i]], 
#                                                strike[-1,optionNames[i]], 
#                                                rate[-n,2], impVol[-n,optionNames[i]], 
#                                                maturity[i], callput[i], 
#                                                divRate[-n,optionNames[i]]) / 
#                                          premium[-1,optionNames[i]]), NA)
  }
  r <- stocksData
  r$impVol <- impVol
  r$strike <- strike
  r$callput <- callput
  r$maturity <- maturity
#   r$rate <- rate
#   r$divRate <- divRate
  r$premium <- premium
#   r$opReturn <- opReturn
  r$underlying <- underlyingNames
  r$optionNames <- optionNames
  r$DimOp <- m
  class(r) <- unique(c("optionsData", class(r)))
  return(r)
}

#' @rdname subset.stocksData
#' @export
subset.optionsData <- function(x, select, drop=TRUE, ...)
{
  if(any(select %in% x$stockNames) && !drop) {
    warning("ignoring drop=FALSE since selecting stocks as well")
    drop=TRUE    
  }
  keep.op <- intersect(select, x$optionNames)
  idx.op <- match(keep.op, x$optionNames)
  if(drop)
    keep.stock <- unique(c(setdiff(select, x$optionNames), x$underlying[idx.op]))
  else
    keep.stock <- x$stockNames
  
  r <- NextMethod(select=keep.stock)
  r$impVol <- x$impVol[,c("Date",keep.op),drop=FALSE]
  r$strike <- x$strike[,c("Date",keep.op),drop=FALSE]
  r$callput <- x$callput[idx.op]
  r$maturity <- x$maturity[idx.op]
#   r$rate <- x$rate
#   r$divRate <- x$divRate[,c("Date",keep.op),drop=FALSE]
  r$premium <- x$premium[,c("Date",keep.op),drop=FALSE]
#   r$opReturn <- x$opReturn[,c("Date",keep.op),drop=FALSE]
  r$underlying <- x$underlying[idx.op]
  r$optionNames <- keep.op
  r$DimOp <- length(keep.op)
  class(r) <- unique(c("optionsData", class(r)))
  return(r)
}

#' @rdname merge.stocksData
#' @export
merge.optionsData <- function(x, y, ...)
{
  y <- as.optionsData(y)
  if(x$Dim==0) return(y)
  if(is.null(y) || y$Dim==0) return(x)
  r <- NextMethod()
  keep <- c("Date", setdiff(y$optionNames, x$optionNames))
  idx <- match(setdiff(y$optionNames, x$optionNames), y$optionNames)
  r$impVol <- merge(x$impVol, y$impVol[,keep,drop=FALSE], by="Date", sort=FALSE)
  r$strike <- merge(x$strike, y$strike[,keep,drop=FALSE], by="Date", sort=FALSE)
  r$callput <- c(x$callput, y$callput[idx])
  r$maturity <- c(x$maturity, y$maturity[idx])
#   r$rate <- merge(x$rate, y$rate, sort=FALSE)
#   r$divRate <- merge(x$divRate, y$divRate[,keep,drop=FALSE], by="Date", sort=FALSE)
  r$premium <- merge(x$premium, y$premium[,keep,drop=FALSE], by="Date", sort=FALSE)
#   r$opReturn <- merge(x$opReturn, y$opReturn[,keep,drop=FALSE], by="Date", sort=FALSE)
  r$underlying <- c(x$underlying, y$underlying[idx])
  r$optionNames <- c(x$optionNames, keep[-1])
  r$DimOp <- length(r$optionNames)
  class(r) <- unique(c("optionsData", class(r)))
  return(r)
}

#' Coercion stocksData to optionsData
#' 
#' Methods for coercing \code{stocksData} objects to class \code{optionsData}.
#' 
#' @param x an object.
#' @param ... further arguments to be passed to or from methods.
#' 
#' @return An object of class \code{optionsData}
#' 
#' @seealso \code{\link{stocks}}, \code{\link{addOptions}}
#' 
#' @details
#' If \code{x} is \code{NULL}, the method will return an empty \code{optionsData} object. 
#' 
#' @export as.optionsData
as.optionsData <- function(x, ...) UseMethod("as.optionsData")

#' @rdname as.optionsData
#' @export
as.optionsData.default <- function(x, ...)
{
  if(is.null(x)) return(as.optionsData(stocks()))
  else {
    stop(gettextf("cannot coerce class \"%s\" to optionsData", deparse(class(x))), 
         domain = NA)
  }
}

#' @rdname as.optionsData
#' @export
as.optionsData.stocksData <- function(x, ...)
{
  if(inherits(x, "optionsData")) return(x)
  x$impVol <- x$price[,1,drop=FALSE]
  x$strike <- x$price[,1,drop=FALSE]
  x$callput <- character(0)
  x$maturity <- numeric(0)
#   x$rate <- x$price[,1,drop=FALSE]
#   x$divRate <- x$price[,1,drop=FALSE]
  x$premium <- x$price[,1,drop=FALSE]
#   x$opReturn <- x$price[,1,drop=FALSE]
  x$underlying <- character(0)
  x$optionNames <- character(0)
  x$DimOp <- 0
  class(x) <- unique(c("optionsData", class(x)))
  return(x)
}