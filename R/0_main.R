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
pkgs <- c("tidyverse", "inspectdf", "data.table", "mlr3verse")
lapply(pkgs, function(pk) require(pk, character.only = TRUE))

#=============================================================================
# Import data
train <-  readr::read_csv("data/train.csv")
test <-  readr::read_csv("data/test.csv")
columns_description <- readr::read_csv("data/columns_description.csv")
sample_submission <- readr::read_csv("data/sample_submission.csv")

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
check_imb

## 1.4 check type
inspect_types(train)

## 1.5 check range
train_fct <- train %>% mutate_all(as.factor)
train_na <- train %>% filter(FIELD_9 == "na")
train_none <- train %>% filter(FIELD_11 == "None")
check_range = inspect_cat(train_fct)
check_range %>% show_plot()
writexl::write_xlsx(check_range$levels, "check_range.xlsx")


test_fct <- test %>% mutate_all(as.factor)
check_range_test = inspect_cat(test_fct)
writexl::write_xlsx(check_range_test$levels, "check_range_test.xlsx")


#=============================================================================
# 2. Data preprocessing
# In this section, I will compose a function to apply with train and test
numeric_vars <- train %>% select_if(is.numeric) %>% names()
factor_vars <- train %>% select_if(is.factor) %>% names()
character_vars <- train %>% select_if(is.character) %>% names()
logical_vars <- train %>% select_if(is.logical) %>% names()

numeric_vars <- c(  "id",         "label" ,      "age_source1", "age_source2", "FIELD_1",     "FIELD_2",     "FIELD_3",
                    "FIELD_4",     "FIELD_5",     "FIELD_6",     "FIELD_14",    "FIELD_15",    "FIELD_16",    "FIELD_21",
                    "FIELD_22",    "FIELD_32",    "FIELD_33",    "FIELD_34",    "FIELD_45",    "FIELD_46",    "FIELD_50",
                    "FIELD_51",    "FIELD_52",    "FIELD_53",    "FIELD_54",    "FIELD_55",    "FIELD_56",    "FIELD_57",
                    "FIELD_11") # Add FIELD_11


cleaned_train <- train %>%
  # mutate_all(na_if, "None") %>%
  mutate(FIELD_11 = as.numeric(na_if(FIELD_11, "None")),
         FIELD_9 = na_if(FIELD_9, "na")) %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.logical, as.factor) %>%
  mutate(label_fct = as.factor(label))


cleaned_train %>%
  inspect_cat() %>%
  pull(levels) %>%
  writexl::write_xlsx("check_range.xlsx")

f_preprocessing <- function(dt){

}


#=============================================================================
# 3. Feature selection
## create task
task = TaskClassif$new(id = "kalapa", backend = cleaned_train, target = "label_fct")
task = TaskRegr$new(id = "kalapa", backend = train, target = "label")
task$feature_names = setdiff(task$feature_names, c("label", "id"))

autoplot(task)
mlr3viz::autoplot(task, type = "pairs")

#=============================================================================
# 4. Model
## create learner
learner = mlr_learners$get("classif.ranger")
set.seed(1911)
train_set = sample(task$nrow * .8)
test_set = setdiff(seq_len(task$nrow), train_set)

#=============================================================================
# 5. Predict
learner$train(task, row_ids = train_set)

