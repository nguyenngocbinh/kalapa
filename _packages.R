# Initial
options(repos = "http://cran.rstudio.org")
have.packages <- installed.packages()[, 1]
cran.packages <- c("tidyverse", "inspectdf", "data.table", "mlr3verse", "here", "paradox", "drake", "magrittr", "R.utils" , "scorecard")
to.install <- setdiff(cran.packages, have.packages)
if (length(to.install) > 0) install.packages(to.install)

#-----------------------------------------------------------------------------
# require packages
pkgs <- c("inspectdf", "readr", "dplyr", "tidyr", "magrittr", "data.table", "mlr3verse", "mlr3viz", "here", "R.utils", "drake")
lapply(pkgs, function(pk) require(pk, character.only = TRUE))
