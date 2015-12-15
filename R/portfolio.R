#' Build Portfolio Dataset
#' 
#' This function is the creator of an S3 class of structured dataset for a portfolio.
#' 
#' @param data an object that contains the components of the portfolio.
#' @param Names a character vector indicating the components of the portfolio.
#' @param weights an object of class \code{pfWeight}, or a numerical vector (see Details). 
#' @param V0 the total value of the portfolio. 
#' 
#' @return
#' An object of class \code{portfolioData}.
#' 
#' @seealso \code{\link{stocks}}, \code{\link{addOptions}}, \code{\link{GBM}}, \code{\link{portfolioWeight}}
#' 
#' @details
#' This function, together with \code{\link{stocks}} and \code{\link{addOptions}}, 
#' forms the bottom layer of the risk calculation system, which performs data structuring. 
#' 
#' This function subsets the data and only keeps the components of the portfolio 
#' (and the underlyings if components are options). 
#' 
#' The function support portfolio weights defined both in value and number of shares, which is supported by
#' \code{\link{portfolioWeight}} function. 
#' If \code{weights} is a numerical vector, 
#' it is regarded as portfolio weights in value, not in number of shares. 
#' 
#' @export
portfolio <- function(data, Names, weights, V0 = 1)
{
  if(anyDuplicated(Names))
    stop("componentNames needs to be not duplicated")
  D <- length(Names)
  if(length(weights) != D)
    stop("componentNames and weights should have the same length")
  if(V0 <= 0)
    stop("portfolio value V0 should be positive")
  weights <- portfolioWeight(weights)
  r <- subset(data, Names)

  pfReturn <- sumWeights <- numeric(r$N)
  for(i in seq_along(Names)) {
    if(Names[i] %in% r$stockNames) {
      pfReturn <- pfReturn + weights[i] *
        (if(attr(weights, "unit")=="share") c(r$price[-1,Names[i]],NA) else 1) *
        exp(r$logReturn[,Names[i]])
      sumWeights <- sumWeights + weights[i] *
        (if(attr(weights, "unit")=="share") c(r$price[-1,Names[i]],NA) else 1)
    }
    else {
      pfReturn <- pfReturn + weights[i] * 
        (if(attr(weights, "unit")=="share") c(r$premium[-1,Names[i]],NA) else 1) *
        exp(r$opReturn[,Names[i]])
      sumWeights <- sumWeights + weights[i] *
        (if(attr(weights, "unit")=="share") c(r$premium[-1,Names[i]],NA) else 1)
    }
  }
  pfReturn <- log(pfReturn / sumWeights)
  
  r$pfReturn <- data.frame(Date=r$price[,1], pfReturn=pfReturn)
  r$V0 <- V0
  r$compWeights <- weights
  r$compNames <- Names
  r$compType <- ifelse(r$compNames %in% r$stockNames, "stock", "option")
  r$DimPf <- D
  class(r) <- unique(c("portfolioData", class(r)))
  return(r)
}

#' Define Portfolio Weights
#' 
#' This function is the creator of an S3 class that defines portfolio weights 
#' in terms of a proportion in dollar value or number of shares for a portfolio.
#' 
#' @param weight a numerical vector that specifies the weights
#' @param unit a character string specifying how the weights are defined. 
#' One of \code{"value"} (default), or \code{"share"}. Also see \code{"Details"}.
#' 
#' @return
#' An object of class \code{pfWeight}.
#' 
#' @seealso \code{\link{portfolio}}
#' 
#' @details
#' \code{portfolioWeight} is able to produce portfolio weights defined using different approaches: 
#' the most basic type of weight is determined by dividing the dollar value of a security 
#' by the total dollar value of the portfolio (\code{unit = "value"}). 
#' Another approach would be to divide the number of units of a given security by the total number of shares 
#' held in the portfolio (\code{unit = "share"}).
#' 
#' @export
portfolioWeight <- function(weight, unit=c("value", "share"))
{
  if(is.null(weight)) return(NULL)
  if(inherits(weight, "pfWeight")) return(weight)
  unit = match.arg(unit)
  
  if(unit=="value") {
    weight = weight / sum(abs(weight))
  }
  attr(weight, "unit") <- unit
  attr(weight, "class") <- "pfWeight"
  return(weight)
}