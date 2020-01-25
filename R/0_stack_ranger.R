pkgs <- c("readr", "dplyr", "inspectdf", "data.table", "mlr3verse", "mlr3viz")
lapply(pkgs, function(pk) require(pk, character.only = TRUE))
# task -----------------------------------------------------------------------

load("data/task_classif.Rdata")
task$col_roles$feature = setdiff(task$col_roles$feature, c("label", "id"))
task$col_roles

# split row_roles
train_idx = 1:30000
test_idx = setdiff(seq_len(task$nrow), train_idx)

task$row_roles$use <- train_idx
task$row_roles$validation <- test_idx
print(task)
# select type ----------------------------------------------------------------
posf = PipeOpSelect$new(id = "select_factor")
posf$param_set$values$selector = selector_type("factor")

posn_0 = PipeOpSelect$new(id = "select_numeric_0")
posn_0$param_set$values$selector = selector_type("numeric")

posn_1 = PipeOpSelect$new(id = "select_numeric_1")
posn_1$param_set$values$selector = selector_type("numeric")

# imputation------------------------------------------------------------------
pof_0 = PipeOpImputeNewlvl$new(id = "imputenewlvl_0")
pof_1 = PipeOpImputeNewlvl$new(id = "imputenewlvl_1")

posample = po("imputesample")
# imbalance-------------------------------------------------------------------
opb = po("classbalancing")
opb$param_set$values = list(ratio = 40, reference = "minor",
                            adjust = "minor", shuffle = FALSE)

# Learner --------------------------------------------------------------------
lnr_ranger = lrn("classif.ranger", predict_type = "prob", num.trees = 122)
lnr_xgboost = lrn("classif.xgboost", predict_type = "prob", scale_pos_weight = 30)
ponop = PipeOpNOP$new()

#  Create Learner CV Operators
glnr_ranger_0 = PipeOpLearnerCV$new(lnr_ranger, id = "glnr_ranger_0")
glnr_xgboost_0 = PipeOpLearnerCV$new(lnr_xgboost, id = "glnr_xgboost_0")
glnr_xgboost_1 = PipeOpLearnerCV$new(lnr_xgboost, id = "glnr_xgboost_1")

# main learner
glnr_main = PipeOpLearner$new(lnr_ranger, id = "main_ranger")


# Graph ----------------------------------------------------------------------
level_0 = gunion(list(posn_1 %>>% glnr_xgboost_1,
                      posn_0 %>>% glnr_xgboost_0))

level_1 = gunion(list(level_0,
                      ponop))

graph = level_1 %>>%
  PipeOpFeatureUnion$new(3) %>>%
  # PipeOpCopy$new(1) %>>%
  opb %>>%
  pof_0 %>>%
  posample %>>%
  glnr_main

graph$plot(html = TRUE) %>% visNetwork::visInteraction(zoomView = TRUE)

#=============================================================================

glrn = GraphLearner$new(graph)
glrn$param_set %>% as.data.table()
print(glrn)
glrn$predict_type <- "prob"

# Tuning
ps = ParamSet$new(list(
  ParamInt$new("classbalancing.ratio", lower = 25, upper = 30),
  ParamInt$new("main_ranger.num.trees", lower = 100, upper = 300),
  ParamInt$new("main_ranger.mtry", lower = 30, upper = 40)
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

tuner = TunerRandomSearch$new()
# tuner = TunerGridSearch$new()
# tuner = TunerGenSA$new()
set.seed(1911)
tuner$tune(instance)

instance$result

instance$archive(unnest = "params")  %>%
  arrange(-classif.auc)

glrn$param_set$values = instance$result$params
glrn$train(task)


# check good_bad
glrn$predict(task, row_ids = train_idx)$confusion
glrn$predict(task, row_ids = test_idx) %>% as.data.table() %>% pull(response) %>% table()

# store data
save(glrn, instance, task, test_idx, train_idx, file = "results/folder_ranger/stack_ranger.Rdata")

# Export predict
glrn$predict(task, row_ids = test_idx) %>%
  as.data.table() %>%
  select(id = row_id, label = prob.bad) %>%
  mutate(id = id - 1) %>%
  rio::export("results/folder_ranger/stack_ranger.csv")
