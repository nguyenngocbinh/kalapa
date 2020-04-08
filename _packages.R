# Initial
options(repos = "http://cran.rstudio.org")
# options(repos = "https://cloud.r-project.org")
have.packages <- installed.packages()[, 1]
cran.packages <- c("tidyverse", "inspectdf", "data.table", "mlr3verse", "here",
                   "R.utils", "drake", "paradox", "bestNormalize", "xgboost", "ranger")
to.install <- setdiff(cran.packages, have.packages)
if (length(to.install) > 0) install.packages(to.install)

#-----------------------------------------------------------------------------
# require packages
pkgs <- c("inspectdf", "readr", "dplyr", "tidyr", "magrittr", "data.table",
          "mlr3verse", "mlr3viz", "here", "R.utils", "drake", "scorecard",
          "ranger", "bestNormalize", "purrr", "stringr")
lapply(pkgs, function(pk) require(pk, character.only = TRUE))
