pkgs <- c( "dplyr", "data.table", "mlr3verse", "mlr3viz", "paradox", "mlr3tuning")
lapply(pkgs, function(pk) require(pk, character.only = TRUE))

## 2.2 create classif task
load("data/task_classif.Rdata")
task$col_roles$feature = setdiff(task$col_roles$feature, c("id", "age_source1_woe", "age_source2_woe", "field_23_woe"))
task$col_roles

# split row_roles
train_idx = 1:30000
test_idx = setdiff(seq_len(task$nrow), train_idx)

task$row_roles$use <- train_idx
task$row_roles$validation <- test_idx
print(task)

#mlr3viz::autoplot(task, type = "pairs")
plot_cols <- setdiff(task$feature_names,
                     c("district", "field_13", "field_39", "field_7", "field_9", "macv",
                       "province"))

## 4.1. Choose learner
learner = lrn("classif.ranger", predict_type = "prob", importance = "impurity", num.trees = 100)
print(learner)
polrn = PipeOpLearner$new(learner)

## 2.3 Imputation
pom = PipeOpMissInd$new()
pon = po("imputehist")
# pon = PipeOpImputeHist$new(id = "imputer_num", param_vals = list(affect_columns = is.numeric))
pof = po("imputenewlvl")
# pof = PipeOpImputeNewlvl$new(id = "imputer_fct", param_vals = list(affect_columns = is.factor))
posample = po("imputesample")

# check imputation
# new_task = posample$train(list(task = task$clone()))[[1]]
# new_task$backend$data(rows = train_idx, cols = new_task$feature_names) %>%
#   as.data.table() %>%
#   inspectdf::inspect_na()
# task$backend$data(rows = train_idx, cols = new_task$feature_names) %>%
#   as.data.table() %>%
#   inspectdf::inspect_na()

# ?mlr3pipelines::PipeOpEncode
# ?mlr3pipelines::PipeOpCollapseFactors
# ?mlr3pipelines::PipeOpImputeSample

# imputer = pom %>>% pon %>>% pof
# imputer = posample
# imputer = pon %>>% pof
# imputer = list(
#   po("imputesample"),
#   po("missind")
# ) %>>% po("featureunion")

imputer = list(
  pof %>>% pon,
  pom
) %>>% po("featureunion")

# imputer check
# imputer_task = imputer$train(task$clone())[[1]]
# df_imputer <- imputer_task$backend$data(rows = train_idx, cols = imputer_task$feature_names) %>%
#   as.data.table()
#
# df_imputer %>%
#   inspectdf::inspect_na()

## 2.4 Imbalanced adjustment
pop = po("smote")
opb = po("classbalancing")
opb$param_set$values = list(ratio = 20, reference = "minor",
                            adjust = "minor", shuffle = FALSE)
# check result
# result_opb = opb$train(list(task))[[1L]]
# table(result_opb$truth())

# 3. Feature selection ------------------------------------------------------------
# filter = mlr_pipeops$get("filter",
#                          filter = mlr3filters::FilterImportance$new(),
#                          param_vals = list(filter.frac = 0.5))

filter = mlr_pipeops$get("filter",
                         filter = mlr3filters::FilterInformationGain$new(),
                         param_vals = list(filter.nfeat = 30 # filter.frac = 0.5
                                           )
                         )

# filter <- flt("importance", learner = learner)

# print(filter)
# new_filter <- filter$calculate(imputer_task)
# head(as.data.table(new_filter), 20)
# =============================================================================
## 4.2. Make graph

# graph = imputer %>>% pop %>>% filter %>>% polrn
# graph = opb %>>% imputer %>>% filter %>>% polrn
# graph = opb %>>% pof %>>% posample %>>%  polrn
# graph = opb %>>% pof %>>% posample  %>>% filter %>>%  polrn
graph = pop %>>%  polrn
graph$plot(html = TRUE) %>% visNetwork::visInteraction()

glrn = GraphLearner$new(graph)
print(glrn)
glrn$predict_type <- "prob"
glrn$param_set$values$classif.ranger.importance <- "impurity"
# glrn$param_set$values$classbalancing.ratio <- 27
# glrn$param_set$values$classif.ranger.num.trees <- 190
# glrn$param_set$values$classif.ranger.mtry <- 30

## 4.3. Tuning hypeparameters

### 4.3.1. Tuning strategy
### 4.3.1. Tuning strategy

resampling_inner = rsmp("cv", folds = 3)
measures = msr("classif.auc")

glrn$param_set %>% as.data.table()
ps = ParamSet$new(list(
  # ParamInt$new("classbalancing.ratio", lower = 1, upper = 15),
  ParamInt$new("smote.K", lower = 1, upper = 6),
  # ParamInt$new("information_gain.filter.nfeat", lower = 30, upper = 40)
  ParamInt$new("classif.ranger.num.trees", lower = 300, upper = 600)
  # ParamInt$new("classif.ranger.mtry", lower = 30, upper = 40)
))


terminator = term("evals", n_evals = 20)
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
#tuner = TunerGridSearch$new()
# tuner = TunerGenSA$new()
set.seed(1911)
tuner$tune(instance)

## result
instance$result
instance$result$perf
instance$archive(unnest = "params")[, c("smote.K",
                                        "classif.ranger.num.trees",
                                        "classif.auc")] %>%
  arrange(-classif.auc)

instance$archive(unnest = "params")[, c("smote.K",
                                        "classif.ranger.num.trees",
                                        "classif.auc")] %>%
  cor()
### 4.3.3. Re-estimate using tuned hyperparameters
glrn$param_set$values = instance$result$params
glrn$train(task)



# =============================================================================
# 5. Predict
glrn$predict(task, row_ids = test_idx)

# check good_bad
glrn$predict(task, row_ids = train_idx)$confusion

# store data
save(glrn, instance, task, test_idx, train_idx, file = "results/ranger_classif_woe.Rdata")

# Export predict
glrn$predict(task, row_ids = test_idx) %>%
  as.data.table() %>%
  select(id = row_id, label = prob.1) %>%
  mutate(id = id - 1) %>%
  rio::export("results/ranger_classif_woe.csv")

## 4.4. Auto tuner to evaluate performance
library(mlr3tuning)
tuner = tnr("grid_search", resolution = 10)
# tuner = tnr("random_search")
at = AutoTuner$new(learner = glrn,
                   resampling = rsmp("holdout"), # resampling_inner,
                   measures = measures,
                   tune_ps = ps,
                   terminator = terminator,
                   tuner = tuner)

at$train(task)
at$model
at$learner
at$predict(task, row_ids = test_idx)

at$predict(task, row_ids = test_idx) %>%
  as.data.table() %>%
  select(id = row_id, label = response) %>%
  mutate(id = id - 1) %>%
  rio::export("results/folder_ranger/ranger_reg.csv")

resampling_outer = rsmp("cv", folds = 3)
# rr = resample(task = task,
#               learner = list(at, glrn),
#               resampling = resampling_outer,
#               store_models = TRUE)
#
# # learner hyperparameters are stored in at
# rr$aggregate()
# rr$errors
# rr$prediction()
# rr$prediction()$confusion
#
# rr$prediction() %>% as.data.table() %>% filter(truth == "bad") %>% pull(prob.bad) %>% mean()

# benchmark

grid = benchmark_grid(
  task = task,
  learner = list(at, glrn),
  resampling = resampling_outer
)
bmr = benchmark(grid)
bmr$aggregate(measures)

