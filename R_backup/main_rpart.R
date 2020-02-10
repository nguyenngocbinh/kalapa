# First: 15-jan-2020
# Update: 20-jan-2020
# Author: NNB
# Purpose: Using rpart to train model
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
pkgs <- c("readr", "dplyr", "inspectdf", "data.table", "mlr3verse", "mlr3viz")
lapply(pkgs, function(pk) require(pk, character.only = TRUE))

# ============================================================================= Import data

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
  cleaned_df <- df %>%
    rename_all(tolower) %>%
    mutate(field_11 = as.numeric(na_if(field_11, "None")),
           field_9 = na_if(field_9, "na"),
           label_fct = if_else(label == 1, "bad", "good")) %>%
    mutate_if(is.character, as.factor) %>%
    mutate_if(is.logical, as.factor) %>%
    as.data.table()

  return(cleaned_df)
}

# clean data using function
cleaned_train <- f_clean_df(train)
cleaned_test <- f_clean_df(test)
cleaned_dset <- f_clean_df(dset)

# See detail of data
cleaned_train %>% glimpse()

# typeof variables
numeric_vars <- train %>% select_if(is.numeric) %>% names()
factor_vars <- train %>% select_if(is.factor) %>% names()
character_vars <- train %>% select_if(is.character) %>% names()
logical_vars <- train %>% select_if(is.logical) %>% names()


## 2.2 create classif task
task = TaskClassif$new(id = "kalapa", backend = cleaned_dset, target = "label_fct", positive = "bad")
task$col_roles$feature = setdiff(task$col_roles$feature, c("label", "id"))
task$col_roles

# split row_roles
train_idx = 1:30000
test_idx = setdiff(seq_len(task$nrow), train_idx)

task$row_roles$use <- train_idx
task$row_roles$validation <- test_idx


mlr3viz::autoplot(task)

## 2.3 Imputation
pom = PipeOpMissInd$new()
pon = PipeOpImputeHist$new(id = "imputer_num", param_vals = list(affect_columns = is.numeric))
pof = PipeOpImputeNewlvl$new(id = "imputer_fct", param_vals = list(affect_columns = is.factor))
imputer = pom %>>% pon %>>% pof
imputer = pon %>>% pof

## 2.4 Imbalanced adjustment
pop = po("smote")

opb = po("classbalancing")
opb$param_set$values = list(ratio = 20, reference = "minor",
                            adjust = "minor", shuffle = FALSE)
result_opb = opb$train(list(task))[[1L]]
table(result_opb$truth())

# 3. Feature selection ------------------------------------------------------------
filter = mlr_pipeops$get("filter",
                         # filter = mlr3filters::FilterVariance$new(),
                         filter = mlr3filters::FilterImportance$new(),
                         param_vals = list(filter.frac = 0.5))


# =============================================================================
# 4. Model create learner
## 4.1. Choose learner

#learner = lrn("classif.ranger", importance = "impurity", predict_type = "prob")
learner = lrn("classif.rpart", predict_type = "prob")

print(learner)
polrn = PipeOpLearner$new(learner)

## 4.2. Make graph

graph = imputer %>>% pop %>>% filter %>>% polrn
graph = opb %>>% imputer %>>% filter %>>% polrn
graph$plot(html = TRUE) %>% visNetwork::visInteraction()

glrn = GraphLearner$new(graph)
print(glrn)
glrn$predict_type <- "prob"

## 4.3. Tuning hypeparameters

### 4.3.1. Tuning strategy

library("paradox")
library("mlr3tuning")
### 4.3.1. Tuning strategy

resampling_inner = rsmp("cv", folds = 6)
measures = msr("classif.auc")
ps = ParamSet$new(list(
  ParamInt$new("classif.rpart.minsplit", lower = 19, upper = 25),
  ParamDbl$new("classif.rpart.cp", lower = 0, upper = .2),
  ParamDbl$new("importance.filter.frac", lower = .35, upper = .7))
  )


terminator = term("evals", n_evals = 30)
### 4.3.2 Tuning

instance = TuningInstance$new(
  task = task,
  learner = glrn,
  resampling = resampling_inner,
  measures = measures,
  param_set = ps,
  terminator = terminator

)

tuner = TunerRandomSearch$new()
# tuner = TunerGridSearch$new()
tuner$tune(instance)

## result
instance$result
instance$result$perf
instance$archive(unnest = "params")[, c("classif.rpart.cp",
                                        "classif.rpart.minsplit",
                                        "importance.filter.frac",
                                        "classif.auc")]

### 4.3.3. Re-estimate using tuned hyperparameters
glrn$param_set$values = instance$result$params
glrn$train(task)

## 4.4. Auto tuner to evaluate performance
library(mlr3tuning)
tuner = tnr("grid_search", resolution = 10)
tuner = tnr("random_search")
at = AutoTuner$new(learner = glrn,
                   resampling = resampling_inner,
                   measures = measures,
                   tune_ps = ps,
                   terminator = terminator,
                   tuner = tuner)

resampling_outer = rsmp("cv", folds = 3)
rr = resample(task = task,
              learner = at,
              resampling = resampling_outer,
              store_models = TRUE)

# learner hyperparameters are stored in at
rr$aggregate()
rr$errors
rr$prediction()
rr$prediction()$confusion

rr$prediction() %>% as.data.table() %>% filter(truth == "bad") %>% pull(prob.bad) %>% mean()
# =============================================================================
# 5. Predict
glrn$predict(task, row_ids = test_idx)

# check good_bad
glrn$predict(task, row_ids = test_idx) %>% as.data.table() %>% pull(response) %>% table()

# store data
save(glrn, task, test_idx, train_idx, file = "results/folder1/rpart_01.Rdata")

# Export predict
glrn$predict(task, row_ids = test_idx) %>%
  as.data.table() %>%
  select(id = row_id, label = prob.bad) %>%
  mutate(id = id - 1) %>%
  rio::export("results/folder1/01.csv")

# Tai lap ket qua
load("results/folder1/rpart_01.Rdata")
