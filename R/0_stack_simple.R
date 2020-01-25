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

posn = PipeOpSelect$new(id = "select_numeric")
posn$param_set$values$selector = selector_type("numeric")

# imputation------------------------------------------------------------------
pof = po("imputenewlvl")
imputer = pof

# imbalance-------------------------------------------------------------------
opb = po("classbalancing")
opb$param_set$values = list(ratio = 20, reference = "minor",
                            adjust = "minor", shuffle = FALSE)

# Learner --------------------------------------------------------------------
lnr_ranger = lrn("classif.ranger", predict_type = "prob", num.trees = 122)
lnr_xgboost = lrn("classif.xgboost", predict_type = "prob")
ponop = PipeOpNOP$new()

#  Create Learner CV Operators
glnr_ranger_0 = PipeOpLearnerCV$new(lnr_ranger, id = "glnr_ranger_0")
glnr_xgboost_0 = PipeOpLearnerCV$new(lnr_xgboost, id = "glnr_xgboost_0")


# Graph ----------------------------------------------------------------------
level_0 = gunion(list(posf %>>% imputer %>>% glnr_ranger_0,
                      posn %>>% glnr_xgboost_0))

graph = opb %>>%
  level_0 %>>%
  PipeOpFeatureUnion$new(2) %>>%
  # PipeOpCopy$new(1) %>>%
  PipeOpLearner$new(lnr_xgboost, id = "glnr_xgboost_1")

graph$plot(html = TRUE) %>% visNetwork::visInteraction(zoomView = TRUE)

#=============================================================================

glrn = GraphLearner$new(graph)
glrn$param_set %>% as.data.table()
print(glrn)
glrn$predict_type <- "prob"

# Tuning
ps = ParamSet$new(list(
  ParamInt$new("classbalancing.ratio", lower = 25, upper = 30)
  # ParamInt$new("information_gain.filter.nfeat", lower = 30, upper = 40)
  # ParamInt$new("glnr_ranger_0.num.trees", lower = 100, upper = 300)
  # ParamInt$new("classif.ranger.mtry", lower = 30, upper = 40)
))

resampling_inner = rsmp("cv", folds = 10)
measures = msr("classif.auc")
terminator = term("evals", n_evals = 2)
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
set.seed(158)
tuner$tune(instance)

instance$result

instance$archive(unnest = "params")  %>%
  arrange(-classif.auc)
