# riskCalc

Risk Calculation System -- MATH G4082 Project

### Package Installation

The package is packed as an R source package, which can be installed in two ways:

* In the terminal of your operating system, execute
  ```
  R CMD INSTALL riskCalc_0.1-1.tar.gz
  ```

* In R console, execute
  ```r
  install.packages("riskCalc_0.1-1.tar.gz", repo=NULL, type="source")
  ```

This package depends on `plyr` and `mvtnorm`, if your installation is failed, please check if you have these dependency installed first. 