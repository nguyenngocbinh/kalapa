pkgs <- c("dplyr", "data.table", "mlr3verse", "mlr3viz")
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

# mlr3viz::autoplot(task)
# plot_cols <- setdiff(task$feature_names,
#                      c("district", "field_13", "field_39", "field_7", "field_9", "macv",
#                        "province"))

## 4.1. Choose learner
learner = lrn("classif.kknn", predict_type = "prob")
print(learner)
learner$param_set %>% as.data.table()
polrn = PipeOpLearner$new(learner, id = "main_kknn")

# imputation------------------------------------------------------------------
pof_0 = PipeOpImputeNewlvl$new(id = "imputenewlvl_0")
pof_1 = PipeOpImputeNewlvl$new(id = "imputenewlvl_1")

posample = po("imputesample")

# select ---------------------------------------------------------------------
filter = flt("information_gain")

posn = PipeOpSelect$new(id = "select_numeric")
posn$param_set$values$selector = selector_type("numeric")
# =============================================================================
## 4.2. Make graph

graph = filter %>>% pof_1 %>>% posample %>>% polrn
graph$plot(html = TRUE) %>% visNetwork::visInteraction()

glrn = GraphLearner$new(graph)
print(glrn)
glrn$predict_type <- "prob"


## 4.3. Tuning hypeparameters

### 4.3.1. Tuning strategy

# glrn$param_set %>% as.data.table()
ps = ParamSet$new(list(
  ParamDbl$new("information_gain.filter.frac", lower = 0.1, upper = 0.2),
  ParamInt$new("main_kknn.k", lower = 6, upper = 8),
  ParamDbl$new("main_kknn.distance", lower = 1, upper = 2)
))

resampling_inner = rsmp("cv", folds = 3)
measures = msr("classif.auc")
terminator = term("evals", n_evals = 10)

### 4.3.2 Tuning
instance = TuningInstance$new(
  task = task,
  learner = glrn,
  resampling = resampling_inner,
  measures = measures,
  param_set = ps,
  terminator = terminator
)

#tuner = TunerRandomSearch$new()
tuner = TunerGridSearch$new()
# tuner = TunerGenSA$new()
tuner$tune(instance)

## result
instance$result
instance$archive(unnest = "params")[, c("main_xgboost.scale_pos_weight",
                                        "main_xgboost.max_depth",
                                        "main_xgboost.eta",
                                        "main_xgboost.gamma",
                                        "classif.auc")] %>%
  arrange(-classif.auc)

instance$archive(unnest = "params")[, c("main_xgboost.scale_pos_weight",
                                        "main_xgboost.max_depth",
                                        "main_xgboost.eta",
                                        "main_xgboost.gamma",
                                        "classif.auc")] %>% cor()
### 4.3.3. Re-estimate using tuned hyperparameters
glrn$param_set$values = instance$result$params
glrn$train(task)


# =============================================================================
# 5. Predict
glrn$predict(task, row_ids = test_idx)

# check good_bad
glrn$predict(task, row_ids = train_idx)$confusion
glrn$predict(task, row_ids = test_idx) %>% as.data.table() %>% pull(response) %>% table()

# store data
save(glrn, instance, task, test_idx, train_idx, file = "results/folder_ranger/xgboost_numeric.Rdata")

# Export predict
glrn$predict(task, row_ids = test_idx) %>%
  as.data.table() %>%
  select(id = row_id, label = prob.bad) %>%
  mutate(id = id - 1) %>%
  rio::export("results/folder_ranger/xgboost_numeric.csv")

