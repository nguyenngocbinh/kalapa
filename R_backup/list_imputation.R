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
