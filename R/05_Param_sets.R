# readd(glrn)$param_set %>% as.data.table()

# SET PARAMETER FOR XGBOOST
param_sets_plan = drake_plan(
  ps = ParamSet$new(list(
    # ParamDbl$new("information_gain.filter.frac", lower = .3, upper = .9),
    #ParamFct$new("classif.xgboost.booster", levels = c("gbtree", "gblinear", "dart")),
    # ParamLgl$new("pca.center", default = TRUE),
    ParamDbl$new("yeojohnson.eps", lower = .01, upper = .3),
    ParamFct$new("classif.xgboost.booster", levels = "gblinear"),
    ParamInt$new("classif.xgboost.scale_pos_weight", lower = 2, upper = 80),
    ParamInt$new("classif.xgboost.max_depth", lower = 8, upper = 50),
    ParamDbl$new("classif.xgboost.eta", lower = .01, upper = .7),
    ParamInt$new("classif.xgboost.gamma", lower = 3, upper = 7),
    ParamDbl$new("classif.xgboost.min_child_weight", lower = .01, upper = 20),
    ParamInt$new("classif.xgboost.max_delta_step", lower = 1, upper = 10), # control imbalances
    ParamDbl$new("classif.xgboost.subsample", lower = 0.5, upper = 0.8),
    #ParamFct$new("classif.xgboost.feature_selector", levels = c("cyclic", "shuffle", "random", "greedy", "thrifty"))
    ParamFct$new("classif.xgboost.feature_selector", levels = "shuffle"),
    # ParamInt$new("classif.xgboost.top_k", lower = 5, upper = 20)
    ParamInt$new("classif.xgboost.nrounds", lower = 50, upper = 100) # maximum number of iterations, or tree
    # ParamInt$new("classif.xgboost.nrounds", special_vals =  1000) # maximum number of iterations, or tree

  ))
)

# check results
# readd(rf_train)$archive(unnest = "params")[, c("classif.xgboost.scale_pos_weight",
#                                                "classif.xgboost.max_depth",
#                                                "classif.xgboost.eta",
#                                                "classif.xgboost.gamma",
#                                                "classif.auc")] %>% arrange(-classif.auc)
#
#
# readd(rf_train)$archive(unnest = "params")[, c("classif.xgboost.scale_pos_weight",
#                                         "classif.xgboost.max_depth",
#                                         "classif.xgboost.eta",
#                                         "classif.xgboost.gamma",
#                                         "classif.auc")] %>% cor()


## SET PARAMETER FOR RANGER
# param_sets_plan = drake_plan(
#   ps = ParamSet$new(list(
#     # ParamDbl$new("information_gain.filter.frac", lower = .8, upper = .9),
#     # ParamInt$new("smote.K", lower = 3, upper = 6),
#     ParamInt$new("classif.ranger.num.trees", lower = 500, upper = 600),
#     ParamInt$new("classif.ranger.mtry", lower = 20, upper = 40),
#     ParamDbl$new("classif.ranger.sample.fraction", lower = 0.2, upper = 0.8)
#   ))
# )
