learner_plan = drake_plan(

  ## 1. Choose learner
  # learner = lrn("classif.ranger", predict_type = "prob", importance = "impurity", num.trees = 100),
  learner = lrn("classif.xgboost", predict_type = "prob"),
  # print(learner)
  polrn = PipeOpLearner$new(learner),

  ## 2. Imputation
  pom = PipeOpMissInd$new(),
  # pon = po("imputehist"),
  pon = PipeOpImputeHist$new(id = "imputehist"),
  pof = PipeOpImputeNewlvl$new(),
  posample = PipeOpImputeSample$new(),

  imputer = list(
    pof %>>% pon,
    pom
  ) %>>% po("featureunion"),

  ## 3. Imbalanced adjustment
  pop = po("smote"),
  opb = target({
    opb = po("classbalancing")
    opb$param_set$values = list(ratio = 20, reference = "minor", adjust = "minor", shuffle = FALSE)
    return(opb)
  }),

  # 4. Feature selection -------------------------------------------------------
  filter_impotance = mlr_pipeops$get("filter",
                           filter = mlr3filters::FilterImportance$new(),
                           param_vals = list(filter.frac = 0.7)),

  filter_info = mlr_pipeops$get("filter",
                           filter = mlr3filters::FilterInformationGain$new(),
                           param_vals = list(filter.frac = 0.7, type = "infogain")),

  filter_cor = flt("correlation", method = "kendall"),

  # 5. Feature transformation --------------------------------------------------
  pca        = po("pca"),
  po_yeo = po("yeojohnson"),

  # =============================================================================
  ## 4.2. Make graph
  # graph = posample %>>% pca %>>% polrn,
  graph = posample %>>% po_yeo %>>% polrn,
  # graph = posample %>>% polrn,
  glrn = target({
    glrn = GraphLearner$new(graph)
    glrn$predict_type = "prob"
    return(glrn)
  })
)

# readd(graph)$plot(html = TRUE) %>% visNetwork::visInteraction()
# print(readd(glrn))

# ## 4.1. Choose learner
# learner = lrn("classif.ranger", predict_type = "prob", importance = "impurity", num.trees = 100)
# print(learner)
# polrn = PipeOpLearner$new(learner)
#
# ## 2.3 Imputation
# pom = PipeOpMissInd$new()
# pon = po("imputehist")
# # pon = PipeOpImputeHist$new(id = "imputer_num", param_vals = list(affect_columns = is.numeric))
# pof = po("imputenewlvl")
# # pof = PipeOpImputeNewlvl$new(id = "imputer_fct", param_vals = list(affect_columns = is.factor))
# posample = po("imputesample")
#
# # check imputation
# # new_task = posample$train(list(task = task$clone()))[[1]]
# # new_task$backend$data(rows = train_idx, cols = new_task$feature_names) %>%
# #   as.data.table() %>%
# #   inspectdf::inspect_na()
# # task$backend$data(rows = train_idx, cols = new_task$feature_names) %>%
# #   as.data.table() %>%
# #   inspectdf::inspect_na()
#
# # ?mlr3pipelines::PipeOpEncode
# # ?mlr3pipelines::PipeOpCollapseFactors
# # ?mlr3pipelines::PipeOpImputeSample
#
# # imputer = pom %>>% pon %>>% pof
# # imputer = posample
# # imputer = pon %>>% pof
# # imputer = list(
# #   po("imputesample"),
# #   po("missind")
# # ) %>>% po("featureunion")
#
# imputer = list(
#   pof %>>% pon,
#   pom
# ) %>>% po("featureunion")
#
# # imputer check
# # imputer_task = imputer$train(task$clone())[[1]]
# # df_imputer <- imputer_task$backend$data(rows = train_idx, cols = imputer_task$feature_names) %>%
# #   as.data.table()
# #
# # df_imputer %>%
# #   inspectdf::inspect_na()
#
# ## 2.4 Imbalanced adjustment
# pop = po("smote")
# opb = po("classbalancing")
# opb$param_set$values = list(ratio = 20, reference = "minor",
#                             adjust = "minor", shuffle = FALSE)
# # check result
# result_opb = opb$train(list(task))[[1L]]
# table(result_opb$truth())
#
# # 3. Feature selection ------------------------------------------------------------
# # filter = mlr_pipeops$get("filter",
# #                          filter = mlr3filters::FilterImportance$new(),
# #                          param_vals = list(filter.frac = 0.5))
#
# filter = mlr_pipeops$get("filter",
#                          filter = mlr3filters::FilterInformationGain$new(),
#                          param_vals = list(filter.nfeat = 30 # filter.frac = 0.5
#                          )
# )
#
# # filter <- flt("importance", learner = learner)
#
# # print(filter)
# # new_filter <- filter$calculate(imputer_task)
# # head(as.data.table(new_filter), 20)
# # =============================================================================
# ## 4.2. Make graph
#
# # graph = imputer %>>% pop %>>% filter %>>% polrn
# # graph = opb %>>% imputer %>>% filter %>>% polrn
# # graph = opb %>>% pof %>>% posample %>>%  polrn
# # graph = opb %>>% pof %>>% posample  %>>% filter %>>%  polrn
# graph = opb %>>%  polrn
# graph$plot(html = TRUE) %>% visNetwork::visInteraction()
#
# glrn = GraphLearner$new(graph)
# print(glrn)
# glrn$predict_type <- "prob"
# glrn$param_set$values$classif.ranger.importance <- "impurity"
# # glrn$param_set$values$classbalancing.ratio <- 27
# # glrn$param_set$values$classif.ranger.num.trees <- 190
# # glrn$param_set$values$classif.ranger.mtry <- 30
