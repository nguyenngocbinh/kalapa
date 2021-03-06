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
numeric_vars <- c("age_source1", "age_source2",
                  "FIELD_1", "FIELD_2", "FIELD_3", "FIELD_4", "FIELD_5", "FIELD_6",
                  "FIELD_14", "FIELD_15", "FIELD_16",
                  "FIELD_21", "FIELD_22",
                  "FIELD_32", "FIELD_33", "FIELD_34",
                  "FIELD_46",
                  "FIELD_50", "FIELD_51", "FIELD_52", "FIELD_53", "FIELD_54", "FIELD_55", "FIELD_56", "FIELD_57")

logical_vars <- c("FIELD_18", "FIELD_19", "FIELD_20",
                  "FIELD_23",
                  "FIELD_25", "FIELD_26", "FIELD_27", "FIELD_28", "FIELD_29", "FIELD_30", "FIELD_31",
                  "FIELD_36", "FIELD_37", "FIELD_38",
                  "FIELD_47", "FIELD_48", "FIELD_49")


character_vars <- c("province", "district", "maCv",
                    "FIELD_7", "FIELD_8", "FIELD_9", "FIELD_10", "FIELD_11", "FIELD_12", "FIELD_13",
                    "FIELD_17",
                    "FIELD_24",
                    "FIELD_35",
                    "FIELD_39", "FIELD_40", "FIELD_41", "FIELD_42", "FIELD_43", "FIELD_44", "FIELD_45")
# Kiem tra xem du so luong bien chua
names(dset) %>%  setdiff(c(numeric_vars, character_vars, logical_vars))

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
inspect_cat(dset[, logical_vars]) %>% show_plot()
inspect_num(dset[, numeric_vars]) %>% show_plot()
inspect_cat(dset[, setdiff(character_vars, "maCv")]) %>% show_plot()

# f_check_range(train, "check_range_train")

# 2. Data preprocessing
# In this section, I will compose a function to apply with train and test
## 2.1. Clean data

# typeof variables
new_character_vars <- character_vars %>% setdiff(c("FIELD_11", "FIELD_12", "FIELD_40", "FIELD_45"))
new_numeric_vars <- c(numeric_vars, "FIELD_11", "FIELD_40", "FIELD_45") %>% setdiff(c("age_source1", "age_source2"))
new_logical_vars <- c(logical_vars, "FIELD_12")

imputer_numeric_vars <- f_return_impute_vars()

# Export to excel to check
eval(parse("D:/R/kalapa/R/1_functions.R"))

df <-  dset %>%
  mutate_at(c(logical_vars, character_vars), toupper) %>%
  mutate_at(c(logical_vars, character_vars), na_if, "NONE") %>%
  mutate(FIELD_11 = as.numeric(FIELD_11)) %>%
  mutate(FIELD_12 = case_when(FIELD_12 == "1" ~ TRUE, FIELD_12 == "0" ~ FALSE, TRUE ~ NA )) %>%
  mutate(FIELD_13 = case_when(FIELD_13 %in% c("0", "4", "8", "12") ~ NA_character_,
                              TRUE ~ as.character(FIELD_13))) %>%
  mutate(FIELD_39 = na_if(FIELD_39, "1")) %>%
  mutate(FIELD_40 = case_when(FIELD_40  %in% c("02 05 08 11", "05 08 11 02", "08 02") ~ NA_character_,
                              TRUE ~ as.character(FIELD_40)),
         FIELD_40 = as.numeric(FIELD_40)) %>%
  mutate(FIELD_43 = FIELD_43 %>% na_if("0") %>% na_if("5")) %>%
  mutate(FIELD_45 = as.numeric(FIELD_45)) %>%
  mutate(FIELD_9 = case_when(FIELD_9 == "NA" ~ "MISSING",
                             FIELD_9  %in% c("75", "79", "80", "86") ~ "RANDOM MISSING",
                             TRUE ~ as.character(FIELD_9))) %>%
  mutate(maCv =  f_recode_macv(maCv)) %>%
  # working with numeric variables
  mutate(AGE = if_else(age_source1 == age_source2, age_source1, NA_real_)) %>%
  mutate(AGE = if_else(AGE >= 18, AGE, NA_real_)) %>%
  select(-age_source1, - age_source2, - FIELD_7, - district) %>%
  # convert ti character
  mutate_if(is.factor, as.character) %>%
  mutate_if(is.logical, as.character)

df_non_na <- df %>%
  filter(FIELD_9 != "MISSING" | is.na(FIELD_9)) %>%
  mutate_if(is.character, replace_na, "RANDOM MISSING")

df_na <- df %>%
  filter(FIELD_9 == "MISSING") %>%
  mutate_if(is.character, replace_na, "MISSING")

cleaned_dt <- df_non_na %>%
  bind_rows(df_na) %>%
  mutate_if(is.character, as.factor) %>%
  # mutate_at(new_numeric_vars, fimpute_numeric) %>% # neu dung mutate if thi bien id va label bi loi
  mutate_at(imputer_numeric_vars, fimpute_numeric) %>%
  arrange(id) %>% # Note
  as.data.table()

bins <- scorecard::woebin(cleaned_dt, y = "label", var_skip = c("id"), bin_num_limit = 8, check_cate_num = FALSE)
bins_numeric <- scorecard::woebin(cleaned_dt, y = "label", x = new_numeric_vars, var_skip = c("id"), bin_num_limit = 8, check_cate_num = FALSE)

dt_woe <- scorecard::woebin_ply(cleaned_dt, bins = bins, to = "woe")
dt_bin <- scorecard::woebin_ply(cleaned_dt, bins = bins, to = "bin")

dt_woe_cat <- scorecard::woebin_ply(cleaned_dt, bins = bins_numeric, to = "woe")
dt_bin_cat <- scorecard::woebin_ply(cleaned_dt, bins = bins_numeric, to = "bin")


# See detail of data
cleaned_dt %>% glimpse()


cleaned_dt %>% select_if(is.factor) %>% inspect_cat() %>% show_plot()
cleaned_dt %>% select_if(is.numeric) %>% inspect_num() %>% show_plot()

## 2.2 create reg task
train_idx = 1:30000
test_idx = 30001:50000
task = TaskRegr$new(id = "kalapa", backend = dt_woe, target = "label")
save(dset, dt_woe, task, train_idx, test_idx, file = "data/task_reg_woe.Rdata")

task = TaskRegr$new(id = "kalapa", backend = dt_bin, target = "label")
save(dset, dt_bin, task, train_idx, test_idx, file = "data/task_reg_bin.Rdata")

task = TaskRegr$new(id = "kalapa", backend = dt_woe_cat, target = "label")
save(dset, dt_woe_cat, task, train_idx, test_idx, file = "data/task_reg_woe_cat.Rdata")

task = TaskRegr$new(id = "kalapa", backend = dt_bin_cat, target = "label")
save(dset, dt_bin_cat, task, train_idx, test_idx, file = "data/task_reg_bincat.Rdata")

## 2.2 create classif task

dt_woe[, label := as.factor(fifelse(label == 1, "bad", "good"))]
task = TaskClassif$new(id = "kalapa", backend = dt_woe, target = "label")
save(dset, dt_woe, task, train_idx, test_idx, file = "data/task_classif_woe.Rdata")

dt_bin[, label := as.factor(fifelse(label == 1, "bad", "good"))]
task = TaskClassif$new(id = "kalapa", backend = dt_bin, target = "label")
save(dset, dt_bin, task, train_idx, test_idx, file = "data/task_classif_woe.Rdata")

dt_woe_cat[, label := as.factor(fifelse(label == 1, "bad", "good"))]
task = TaskClassif$new(id = "kalapa", backend = dt_woe_cat, target = "label")
save(dset, dt_woe_cat, task, train_idx, test_idx, file = "data/task_classif_woe.Rdata")

dt_bin_cat[, label := as.factor(fifelse(label == 1, "bad", "good"))]
task = TaskClassif$new(id = "kalapa", backend = dt_bin_cat, target = "label")
save(dset, dt_bin_cat, task, train_idx, test_idx, file = "data/task_classif_woe.Rdata")
