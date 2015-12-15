#' Build Stocks Dataset
#' 
#' This function is the creator for an S3 class of structured dataset on stocks. 
#' 
#' @param data a data frame, the raw data that contains stock prices.
#' @param Names a character vector, the symbols of the stocks.
#' @param date.col an integer or a character string that specifies the date column in data.
#' @param price.col a vector of integers or character strings that specifies the price columns in data.
#' @param rate.col an integer or a string that specifies the column for risk free rate.
#' @param divRate.col a vector of integers or strings that specifies the columns for dividend rates.
#' 
#' @return An object of class \code{stocksData}, which stores the structured data of stocks.
#' 
#' @details 
#' This function builds a stocks dataset by organizing raw data into a structured form.
#' If \code{Names==NULL}, the stocks will be automatically named as \code{"S1"}, \code{"S2"}, and so on. 
#' If the raw data does not have a date column, \code{date.col} should be set to \code{NULL}, 
#' and the data is regarded as in reverse chronological order.
#' 
#' @export
stocks <- function(data=NULL, Names=NULL, date.col=1, price.col=-1, rate.col=NULL, divRate.col=NULL)
{
  data <- as.data.frame(data)
  n <- nrow(data)
  if(n == 0) {
    return(structure(list(price = data.frame(Date=numeric(0)), logReturn = data.frame(Date=numeric(0)), 
                          rate = data.frame(Date=numeric(0), rate=numeric(0)), 
                          divRate = data.frame(Date=numeric(0)), Dim = 0L, N = 0L, 
                          stockNames = character(0)), class = "stocksData"))
  }
  if(is.numeric(price.col) && any(price.col<0))
    price.col <- setdiff(seq_along(data), -price.col)
  if(is.numeric(date.col) && any(date.col<0))
    date.col <- setdiff(seq_along(data), -date.col)
  if(length(date.col)>1) {
    warning("There should be at most one date column. Taking the first specified column.")
    date.col <- date.col[1]
  }
  if(length(rate.col)>1) {
    warning("There should be at most one rate column. Taking the first specified column.")
    rate.col <- rate.col[1]
  }
  D <- length(price.col)
  price <- data.matrix(data[,price.col,drop=FALSE])
  if(!is.null(rate.col)) {
    rate <- data[,rate.col]
  }
  else
    rate <- rep(0, n)
  if(!is.null(divRate.col)) {
    divRate <- data.matrix(data)[,divRate.col,drop=FALSE]
  }
  else
    divRate <- matrix(0, nrow=n, ncol=D)
  
  if(!is.null(date.col)) {
    Date <- as.Date(data[,date.col])
    ord <- order(Date)[n:1]
    Date <- Date[ord]
    price <- price[ord,,drop=FALSE]
    rate <- rate[ord]
    divRate <- divRate[ord,,drop=FALSE]
  } else
    Date <- n:1
  
  logReturn <- rbind(-diff(log(price)), matrix(NA, nrow=1, ncol=D))
  
  if(is.null(Names)) {
    Names <- paste0("S", seq(D))
  }
  colnames(price) <- colnames(logReturn) <- colnames(divRate) <- Names
  
  structure(list(price = data.frame(Date, price), logReturn = data.frame(Date, logReturn), 
                 rate = data.frame(Date, rate), divRate = data.frame(Date, divRate),
                 Dim = D, N = n, stockNames = Names), class = "stocksData")
}

#' Merge Stocks or Options Datasets
#' 
#' Merge two stocks or options datasets by common dates.
#' 
#' @param x,y objects of class \code{stocksData} or \code{optionsData}.
#' @param ... arguments to be passed to or from methods.
#' 
#' @return An object of the same class as \code{x}. 
#' 
#' @details
#' If \code{x} is a \code{stocksData} object but \code{y} is an \code{optionsData} object, 
#' the options data in \code{y} will be lost in the merged result.
#' Also if \code{x} or \code{y} have been calibrated to a GBM model, that information will also be lost 
#' in the merged dataset.
#' 
#' @seealso \code{\link{stocks}}, \code{\link{addOptions}}
#' 
#' @export
merge.stocksData <- function(x, y, ...)
{
  if(x$Dim==0) return(y)
  if(is.null(y) || y$Dim==0 || length(setdiff(y$stockNames, x$stockNames))==0) return(x)
  keep <- c("Date", setdiff(y$stockNames, x$stockNames))
  price <- merge(x$price, y$price[,keep,drop=FALSE], by="Date", sort=FALSE)
  logReturn <- merge(x$logReturn, y$logReturn[,keep,drop=FALSE], by="Date", sort=FALSE)
  rate <- merge(x$rate, y$rate[,1,drop=FALSE], sort=FALSE)
  divRate <- merge(x$divRate, y$divRate[,keep,drop=FALSE], by="Date", sort=FALSE)
  Dim <- ncol(price)-1
  N <- nrow(price)
  stockNames <- colnames(price)[-1]
  structure(list(price = price, logReturn = logReturn, rate = rate, divRate = divRate, 
                 Dim = Dim, N = N, stockNames = stockNames), class = "stocksData")
}

#' Subsetting Options or Stocks Dataset
#' 
#' Return subset of the dataset with selected symbols.
#' 
#' @param x object to be subsetted, of class \code{stocksData}, \code{optionsData} or \code{GBM}.
#' @param select a character vector specifying selected symbols of stocks and/or options.
#' @param drop logical. If \code{TRUE}, the unused underlying stocks in the options dataset will be dropped.
#' @param ... arguments to be passed to or from methods.
#' 
#' @return An object of the same class as \code{x}.
#' 
#' @seealso \code{\link{stocks}}, \code{\link{addOptions}}, \code{\link{GBM}}
#' @details
#' If you subset an \code{optionsData} object and do not select any options, 
#' the result will still be an \code{optionsData} object with no options. \code{GBM} objects also can be
#' subsetted and parameters for selected instruments will be kept intact. 
#' 
#' @export
subset.stocksData <- function(x, select, ...) 
{
  Date <- x$price[,1,drop=FALSE]
  price <- cbind(Date, subset(x$price[,-1,drop=FALSE], select=select))
  logReturn <- cbind(Date, subset(x$logReturn[,-1,drop=FALSE], select=select))
  divRate <- cbind(Date, subset(x$divRate[,-1,drop=FALSE], select=select))
  structure(list(price = price, logReturn = logReturn, rate = x$rate, divRate = divRate,
                 Dim = ncol(price)-1, N = x$N, stockNames = colnames(price)[-1]), class = "stocksData")
}

