# First: 15-jan-2020
# Update: 28-jan-2020
# Author: NNB
# Purpose: Prepare data for regression model
# =============================================================================

# Instruction
# R/0_main.R : File chính chạy các kết quả
# R/1_functions.R: Chứa các user-defined functions
# R/2_plots.R: Chứa các hàm tiện ích cho plot
# vignettes/report.rmd: Tổng hợp kết quả và giải thích

# Contents
# 1. Data cleaning
# 2. Data preprocessing
## 2.1. Prepare cleaned data
## 2.2. create classif task
## 2.3. Imputation
## 2.4. Imbalanced adjustment
# 3. Feature selection
# 4. Model
## 4.1. Choose learner
## 4.2. Make graph
## 4.3. Tuning hypeparameters
# 5. Predict

# Initial
options(repos = "http://cran.rstudio.org")
have.packages <- installed.packages()
cran.packages <- c("tidyverse", "inspectdf", "data.table", "mlr3verse")
to.install <- setdiff(cran.packages, have.packages[, 1])
if (length(to.install) > 0) install.packages(to.install)

# Load packages
pkgs <- c("readr", "dplyr", "tidyr", "inspectdf", "data.table", "mlr3verse", "mlr3viz")
lapply(pkgs, function(pk) require(pk, character.only = TRUE))

# ============================================================================= Import data
rm(list = ls())
col_types <- cols(id = col_double(), province = col_character(), district = col_character(), age_source1 = col_double(),
                  age_source2 = col_double(), maCv = col_character(), FIELD_1 = col_double(), FIELD_2 = col_double(), FIELD_3 = col_double(),
                  FIELD_4 = col_double(), FIELD_5 = col_double(), FIELD_6 = col_double(), FIELD_7 = col_character(), FIELD_8 = col_character(),
                  FIELD_9 = col_character(), FIELD_10 = col_character(), FIELD_11 = col_character(), FIELD_12 = col_character(),
                  FIELD_13 = col_character(), FIELD_14 = col_double(), FIELD_15 = col_double(), FIELD_16 = col_double(), FIELD_17 = col_character(),
                  FIELD_18 = col_logical(), FIELD_19 = col_logical(), FIELD_20 = col_logical(), FIELD_21 = col_double(), FIELD_22 = col_double(),
                  FIELD_23 = col_logical(), FIELD_24 = col_character(), FIELD_25 = col_logical(), FIELD_26 = col_logical(), FIELD_27 = col_logical(),
                  FIELD_28 = col_logical(), FIELD_29 = col_character(), FIELD_30 = col_character(), FIELD_31 = col_character(),
                  FIELD_32 = col_double(), FIELD_33 = col_double(), FIELD_34 = col_double(), FIELD_35 = col_character(), FIELD_36 = col_character(),
                  FIELD_37 = col_character(), FIELD_38 = col_logical(), FIELD_39 = col_character(), FIELD_40 = col_character(),
                  FIELD_41 = col_character(), FIELD_42 = col_character(), FIELD_43 = col_character(), FIELD_44 = col_character(),
                  FIELD_45 = col_character(), FIELD_46 = col_double(), FIELD_47 = col_logical(), FIELD_48 = col_logical(), FIELD_49 = col_logical(),
                  FIELD_50 = col_double(), FIELD_51 = col_double(), FIELD_52 = col_double(), FIELD_53 = col_double(), FIELD_54 = col_double(),
                  FIELD_55 = col_double(), FIELD_56 = col_double(), FIELD_57 = col_double())

train <- readr::read_csv("data/train.csv", col_types = col_types)
test <- readr::read_csv("data/test.csv", col_types = col_types)
columns_description <- readr::read_csv("data/columns_description.csv")
sample_submission <- readr::read_csv("data/sample_submission.csv")

# add label to test set
test$label <- NA

# combine two data
dset <- bind_rows(train, test)

# check combination
dset$label %>% is.na() %>% table()

# 1. Data cleaning ===========================================================
## 1.1. Check target variable
### 1.1.1. Missing
train$label %>% is.na() %>% table()  # No missing

### 1.1.2. Imbalance
train$label %>% table() %>% prop.table()  # imbalanced


## 1.2. Check missing predictors
check_missing = inspect_na(train, test)  # some fields can be removed
check_missing # same distribution in 2 dataset
# check_missing %>% show_plot()

## 1.3. check inbalance predictors
check_imb = inspect_imb(train, test, include_na = TRUE)
check_imb

## 1.4 check type
check_types <- inspect_types(train, test)
check_types

## 1.5 check range
f_check_range(train, "check_range_train")

# 2. Data preprocessing
# In this section, I will compose a function to apply with train and test
## 2.1. Clean data
# Export to excel to check
f_clean_df <- function(df) {
  df <-  df %>%
    rename_all(tolower) %>%
    mutate(field_11 = as.numeric(na_if(field_11, "None")),
           field_9 = field_9 %>% na_if("na") %>% replace_na("MISSING"),
           age = ifelse(is.na(age_source1), age_source2, age_source1)) %>%
    mutate_if(is.factor, as.character) %>%
    mutate_if(is.logical, as.character)

  df_non_na <- df %>%
    filter(field_9 != "MISSING")

  df_na <- df %>%
    filter(field_9 == "MISSING") %>%
    mutate_if(is.character, replace_na, "MISSING")

  bind_df <- df_non_na %>%
    bind_rows(df_na) %>%
    # mutate(label_fct = if_else(label == 1, "bad", "good"))%>%
    mutate_if(is.character, as.factor) %>%
    arrange(id) %>% # Note
    as.data.table()

  bind_df <- scorecard::replace_na(bind_df, repl = "median")

  bins <- scorecard::woebin(bind_df, y = "label", var_skip = c("id"), bin_num_limit = 10, check_cate_num = FALSE)

  cleaned_df <- scorecard::woebin_ply(bind_df, bins = bins, to = "woe")

  cleaned_df <- cleaned_df %>% mutate_if(is.character, as.factor)

  return(cleaned_df)
}

# clean data using function
# cleaned_train <- f_clean_df(train)
# cleaned_test <- f_clean_df(test)
cleaned_dset <- f_clean_df(dset)

# See detail of data
cleaned_dset %>% glimpse()

# typeof variables
numeric_vars <- train %>% select_if(is.numeric) %>% names()
factor_vars <- train %>% select_if(is.factor) %>% names()
character_vars <- train %>% select_if(is.character) %>% names()
logical_vars <- train %>% select_if(is.logical) %>% names()


## 2.2 create reg task
task = TaskRegr$new(id = "kalapa", backend = cleaned_dset, target = "label")
train_idx = 1:30000
test_idx = setdiff(seq_len(task$nrow), train_idx)

save(dset, cleaned_dset, task, train_idx, test_idx, file = "data/task_reg.Rdata")

## 2.2 create classif task

cleaned_dset <- cleaned_dset %>%
  mutate(label = as.factor(label))

task = TaskClassif$new(id = "kalapa", backend = cleaned_dset, target = "label")

save(dset, cleaned_dset, task, train_idx, test_idx, file = "data/task_classif.Rdata")
