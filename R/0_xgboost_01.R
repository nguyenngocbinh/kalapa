pkgs <- c("readr", "dplyr", "inspectdf", "data.table", "mlr3verse", "mlr3viz")
lapply(pkgs, function(pk) require(pk, character.only = TRUE))

## 2.2 create classif task
load("data/task_classif.Rdata")
task$col_roles$feature = setdiff(task$col_roles$feature, c("label", "id"))
task$col_roles

# split row_roles
train_idx = 1:30000
test_idx = setdiff(seq_len(task$nrow), train_idx)

task$row_roles$use <- train_idx
task$row_roles$validation <- test_idx
print(task)

mlr3viz::autoplot(task)
plot_cols <- setdiff(task$feature_names,
                     c("district", "field_13", "field_39", "field_7", "field_9", "macv",
                       "province"))

## 4.1. Choose learner
# learner = lrn("classif.ranger", predict_type = "prob", importance = "impurity" )
learner = lrn("classif.xgboost")
print(learner)
learner$param_set %>% as.data.table()
polrn = PipeOpLearner$new(learner)

## 2.3 Imputation
pom = PipeOpMissInd$new()
pon = po("imputehist")
# pon = PipeOpImputeHist$new(id = "imputer_num", param_vals = list(affect_columns = is.numeric))
pof = po("imputenewlvl")
# pof = PipeOpImputeNewlvl$new(id = "imputer_fct", param_vals = list(affect_columns = is.factor))
posample = po("imputesample")

poe = po("encode")

# check imputation
new_task = posample$train(list(task = task))[[1]]
new_task$backend$data(rows = train_idx, cols = new_task$feature_names) %>%
  as.data.table() %>%
  inspectdf::inspect_na()
task$backend$data(rows = train_idx, cols = new_task$feature_names) %>%
  as.data.table() %>%
  inspectdf::inspect_na()

# ?mlr3pipelines::PipeOpEncode
# ?mlr3pipelines::PipeOpCollapseFactors
# ?mlr3pipelines::PipeOpImputeSample

# imputer = pom %>>% pon %>>% pof
# imputer = posample
imputer = pom %>>% pon %>>% pof
# imputer = list(
#   po("imputesample"),
#   po("missind")
# ) %>>% po("featureunion")


# imputer check
imputer_task = imputer$train(task)[[1]]
df_imputer <- imputer_task$backend$data(rows = train_idx, cols = imputer_task$feature_names) %>%
  as.data.table()

df_imputer %>%
  inspectdf::inspect_na()

## 2.4 Imbalanced adjustment
# pop = po("smote")
opb = po("classbalancing")
opb$param_set$values = list(ratio = 21, reference = "minor",
                            adjust = "minor", shuffle = FALSE)
# check result
result_opb = opb$train(list(task))[[1L]]
table(result_opb$truth())

# 3. Feature selection ------------------------------------------------------------
filter = mlr_pipeops$get("filter",
                         filter = mlr3filters::FilterImportance$new(),
                         param_vals = list(filter.frac = 0.2, xval = 5))
# filter = flt("auc")
# filter <- flt("importance", learner = lrn("classif.ranger", predict_type = "prob", importance = "impurity"))
#print(filter)
#new_filter <- filter$calculate(imputer_task)
#head(as.data.table(new_filter), 20)
#sel_importance <- head(as.data.table(new_filter), 10)$feature
# =============================================================================
## 4.2. Make graph

# graph = imputer %>>% pop %>>% filter %>>% polrn
# graph = opb %>>% imputer %>>% filter %>>% polrn
graph = opb %>>% imputer %>>% filter %>>%  poe %>>%  polrn
graph$plot(html = TRUE) %>% visNetwork::visInteraction()

glrn = GraphLearner$new(graph)
print(glrn)
glrn$predict_type <- "prob"
# glrn$param_set$params$classif.ranger.importance <- "impurity" # Khong co tac dung
glrn$param_set$values$classbalancing.ratio <- 22


## 4.3. Tuning hypeparameters

### 4.3.1. Tuning strategy

library("paradox")
library("mlr3tuning")
### 4.3.1. Tuning strategy

resampling_inner = rsmp("cv", folds = 4)
measures = msr("classif.auc")

glrn$param_set %>% as.data.table()
ps = ParamSet$new(list(
  ParamInt$new("classbalancing.ratio", lower = 15, upper = 20)
  # ParamInt$new("classif.ranger.num.trees", lower = 70, upper = 90),
  # ParamInt$new("classif.ranger.mtry", lower = 10, upper = 40)
))


terminator = term("evals", n_evals = 2)
### 4.3.2 Tuning

sel_task = task$clone()$select(c("district", "field_10", "field_12", "field_13", "field_17", "field_18", "field_19", "field_20", "field_23", "field_24"))

instance = TuningInstance$new(
  task = sel_task,
  learner = glrn,
  resampling = resampling_inner,
  measures = measures,
  param_set = ps,
  terminator = terminator
)

tuner = TunerRandomSearch$new()
# tuner = TunerGridSearch$new()
# tuner = TunerGenSA$new()
tuner$tune(instance)

## result
instance$result
instance$result$perf
instance$archive(unnest = "params")[, c("classbalancing.ratio",
                                          "classif.ranger.num.trees",
                                        "classif.ranger.mtry",
                                        "classif.auc")] %>%
  arrange(-classif.auc)

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
glrn$predict(task, row_ids = train_idx)$confusion
glrn$predict(task, row_ids = test_idx) %>% as.data.table() %>% pull(response) %>% table()

# store data
save(glrn, instance, task, test_idx, train_idx, file = "results/folder_ranger/ranger_03.Rdata")

# Export predict
glrn$predict(task, row_ids = test_idx) %>%
  as.data.table() %>%
  select(id = row_id, label = prob.bad) %>%
  mutate(id = id - 1) %>%
  rio::export("results/folder_ranger/ranger_03.csv")

# Tai lap ket qua
load("results/folder_ranger/ranger_03.Rdata")
