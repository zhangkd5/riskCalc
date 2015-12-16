#' VaR Backtesting
#' 
#' This function performs VaR backtesting and creates an S3 class object of backtesting results
#' 
#' @param object an object of class \code{"VaR"}.
#' @param testLen.years the window size of backtesting in years. 
#' 
#' @return
#' An object of class \code{"VaR.backtesting"}. 
#' 
#' @seealso
#' \code{\link{VaR}}
#' 
#' @details
#' Backtesting is performed by firstly compute the realized loss, and then calculate the rate that
#' VaR is exceeded across the backtesting window. 
#' 
#' @export
backtest <- function(object, testLen.years=1)
{
  testLen <- testLen.years * 252
  if(testLen > object$data$N)
    stop("backtest failed: insufficient historical data")
  r <- object
  loss <- realizedLoss(object$data, object$TimeLen)
  exceed <- loss > data.matrix(object$VaR[,-1,drop=FALSE])
  exceedRate <- sapply(seq(object$data$N - testLen+1), function(nr) {
    colMeans(exceed[nr:(nr+testLen-1),,drop=FALSE], na.rm=TRUE)
  })
  if(is.matrix(exceedRate)) 
    exceedRate <- t(exceedRate)
  else
    exceedRate <- as.matrix(exceedRate)
  exceedRate <- rbind(exceedRate, matrix(NA_real_, nrow=testLen-1, ncol=ncol(exceedRate)))
  dimnames(exceedRate) <- dimnames(loss)
  r$actualLoss <- data.frame(Date=object$data$price[,1], loss)
  r$exceedRate <- data.frame(Date=object$data$price[,1], exceedRate)
  r$testLen.years <- testLen.years
  class(r) <- unique(c("VaR.backtest", class(r)))
  return(r)
}

#' Calcuate Realized Loss
#' 
#' This generic function is an internal function used by \code{\link{backtest}} to calculate realized loss
#' given various types of dataset. 
#' 
#' @param data an object of structured dataset.
#' @param TimeLen the time horizon the losses are defined over, in days.
#' @param V0 the total value the losses are defined upon.
#' @param ... arguments to be passed to or from methods.
#' 
#' @return
#' A matrix of realized losses.
#' 
#' @seealso \code{\link{backtest}}
#' 
#' @export
realizedLoss <- function(data, TimeLen, ...)
  UseMethod("realizedLoss")

#' @rdname realizedLoss
#' @export
realizedLoss.portfolioData <- function(data, TimeLen, ...)
{
  loss <- NextMethod(V0=1)
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
  as.matrix(rowSums(loss[,data$compNames,drop=FALSE] * w.raw))
}

#' @rdname realizedLoss
#' @export
realizedLoss.optionsData <- function(data, TimeLen, V0=NULL, ...)
{
  if(is.null(V0)) 
    V0 <- ifelse(is.null(data$V0), 10000, data$V0)
  loss <- as.data.frame(NextMethod())
  
  for(i in seq_along(data$optionNames))
  {
    FV <- c(rep(NA, TimeLen), 
            BSPrice(data$price[seq(data$N-TimeLen),data$underlying[i]], data$strike[-seq(TimeLen),i+1], 
                    data$rate[seq(data$N-TimeLen),2], data$impVol[seq(data$N-TimeLen),i+1], 
                    data$maturity[i], data$callput[i], data$divRate[seq(data$N-TimeLen),data$underlying[i]]))
    loss[,data$optionName[i]] <- V0 * (1 - FV / data$premium[,i+1])
  }
  return(data.matrix(loss))
}

#' @rdname realizedLoss
#' @export
realizedLoss.stocksData <- function(data, TimeLen, V0=NULL, ...)
{
  if(is.null(V0)) 
    V0 <- ifelse(is.null(data$V0), 10000, data$V0)
  loss <- rbind(matrix(NA_real_, nrow=TimeLen, ncol=data$Dim), 
    data.matrix(V0*(1-data$price[seq(data$N-TimeLen),-1,drop=FALSE]/data$price[-seq(TimeLen),-1,drop=FALSE])))
  dimnames(loss) <- list(NULL, data$stockNames)
  return(loss)
}