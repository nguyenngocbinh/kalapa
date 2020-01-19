pom = PipeOpMissInd$new()
pon = PipeOpImputeHist$new(id = "imputer_num", param_vals = list(affect_columns = is.numeric))
pof = PipeOpImputeNewlvl$new(id = "imputer_fct", param_vals = list(affect_columns = is.factor))
imputer = pom %>>% pon %>>% pof

# learner = mlr_learners$get("classif.rpart")
learner = mlr_learners$get("classif.ranger") # , importance = "impurity"
print(learner)

polrn = PipeOpLearner$new(learner)

graph = imputer %>>% polrn
graph$plot(html = TRUE) %>% visNetwork::visInteraction()
lrn = GraphLearner$new(graph = graph)


task = TaskClassif$new(id = "kalapa", backend = cleaned_train, target = "label_fct")
task$col_roles$feature = setdiff(task$col_roles$feature, c("label", "id"))

glrn = GraphLearner$new(graph)
glrn$predict_type <- "prob"

cv3 = rsmp("cv", folds = 3)
resample(task, glrn, cv3)

# glrn$param_set$values$variance.filter.frac = 0.25
# cv3 = rsmp("cv", folds = 3)
# resample(task, glrn, cv3)

library("paradox")
ps = ParamSet$new(list(
  ParamInt$new("classif.ranger.num.trees", lower = 1, upper = 5),
  ParamInt$new("classif.ranger.mtry", lower = 1, upper = 5)
  #ParamDbl$new("variance.filter.frac", lower = 0.25, upper = 1)
))



library("mlr3tuning")
instance = TuningInstance$new(
  task = task,
  learner = glrn,
  resampling = rsmp("holdout"),
  measures = msr("classif.auc"),
  param_set = ps,
  terminator = term("evals", n_evals = 200)
)

tuner = TunerRandomSearch$new()
tuner$tune(instance)

instance$result

instance$result$perf

