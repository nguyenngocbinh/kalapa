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

pom = PipeOpMissInd$new()
pon = po("imputehist")
pof = po("imputenewlvl")

imputer = list(
  pof %>>% pon,
  pom
) %>>% po("featureunion")

imputer$plot(html = TRUE) %>% visNetwork::visInteraction()

imputer_task = imputer$train(task)[[1]]

print(imputer_task)

df_imputer <- imputer_task$backend$data(rows = train_idx, cols = imputer_task$feature_names) %>%
  as.data.table()
df_imputer %>%
  inspectdf::inspect_na()


filter <- flt("importance", learner = lrn("classif.ranger", predict_type = "prob", importance = "impurity"))
filter = mlr_pipeops$get("filter",
                         filter = mlr3filters::FilterImportance$new(),
                         param_vals = list(filter.frac = 0.2, xval = 5))

new_filter <- filter$calculate(imputer_task)
head(as.data.table(new_filter), 20)
fs_importance <- head(as.data.table(new_filter), 10)$feature

fs_task <- imputer_task$clone()$select(fs_importance)
print(fs_task)

learner = lrn("classif.xgboost")
print(learner)
polrn = PipeOpLearner$new(learner)
opb = po("classbalancing")
poe = po("encode")
graph = opb %>>%  poe %>>%  polrn
graph$plot(html = TRUE) %>% visNetwork::visInteraction()

glrn = GraphLearner$new(graph)
glrn$predict_type <- "prob"
sel_task <- task$clone()$select(setdiff(fs_importance, c("district", "field_7", "province")) )

resampling_inner = rsmp("cv", folds = 4)
measures = msr("classif.auc")
ps = ParamSet$new(list(
  ParamInt$new("classbalancing.ratio", lower = 15, upper = 20)
  # ParamInt$new("classif.ranger.num.trees", lower = 70, upper = 90),
  # ParamInt$new("classif.ranger.mtry", lower = 10, upper = 40)
))

terminator = term("evals", n_evals = 2)

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
