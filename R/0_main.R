# First: 15-jan-2020
# Update:
# Author: NNB
# Purpose: Main
#=============================================================================

# Instruction
# R/0_main.R : File chính chạy các kết quả
# R/1_functions.R: Chứa các user-defined functions
# R/2_plots.R: Chứa các hàm tiện ích cho plot
# vignettes/report.rmd: Tổng hợp kết quả và giải thích

# Contents
# 1. Data understand
# 2. Data preprocessing
# 3. Feature selection
# 4. Model
# 5. Predict

# Load packages
pkgs = c("tidyverse", "inspectdf", "data.table", "mlr3")
lapply(pkgs, function(pk) require(pk, character.only = TRUE))

#=============================================================================
# Import data
train = data.table::fread("train.csv")
test = data.table::fread("test.csv")
columns_description = data.table::fread("columns-description.csv")
sample_submission = data.table::fread("sample-submission.csv")


#=============================================================================
# 1. Data understand
## 1.1. Check target variable
### 1.1.1. Missing
train$label %>% is.na() %>% table() # No missing

### 1.1.2. Imbalance
train$label %>% table() %>% prop.table() # imbalanced


## 1.2. Check missing predictors 
check_missing = inspect_na(train) # some fields can be removed
check_missing
check_missing %>% show_plot()

## 1.3. check inbalance predictors
check_imb = inspect_imb(train)

#=============================================================================
# 2. Data preprocessing
# In this section, I will compose a function to apply with train and test


#=============================================================================
# 3. Feature selection

#=============================================================================
# 4. Model

#=============================================================================
# 5. Predict

