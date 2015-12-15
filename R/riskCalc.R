#' Risk Calculation System -- MATH G4082 Project
#'
#' The riskCalc package provides a risk calculation system that is able to 
#' \enumerate{
#' \item compute Monte Carlo, historical, and parametric VaR for a portfolio of stock and option positions;
#' \item both calibrate to historical data and take parameters as input;
#' \item backtest the computed VaR against history.
#' }
#' This package is a part of the course project for MATH G4082. 
#'
#' @details
#' \tabular{ll}{
#' Package: \tab riskCalc\cr
#' Type: \tab Package\cr
#' Version: \tab 0.1-1\cr
#' Date: \tab 2015-11-22\cr
#' License: \tab GPL (>=2)\cr
#' }
#' This package is a risk calculation system that consists of three layers of operations. 
#' The first (bottom) layer includes functions \code{\link{stocks}}, \code{\link{addOptions}}, 
#' \code{\link{portfolio}} which creates structured data objects. The second (mid) layer includes a 
#' generic function \code{\link{GBM}} which performs model calibration / specification. 
#' The third (top) layer includes functions \code{\link{VaR}} and \code{\link{backtest}} which performs
#' VaR calculation and backtesting based on calibrated or uncalibrated data. 
#' 
#' @author
#' Kaidong Zhang \email{kz2236@@columbia.edu},
#' Weicong Wang \email{tyrael0825@@gmail.com},
#' Chenxing Ouyang \email{co2341@@columbia.edu}
#'
#' Maintainer: Kaidong Zhang \email{kz2236@@columbia.edu}
#' 
#' @name riskCalc
#' 
#' @docType package
NULL