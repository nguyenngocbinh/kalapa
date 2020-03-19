# readd(glrn)$param_set %>% as.data.table()

# SET PARAMETER FOR XGBOOST
param_sets_plan = drake_plan(
  ps = ParamSet$new(list(
    #ParamFct$new("classif.xgboost.booster", levels = c("gbtree", "gblinear", "dart")),
    ParamFct$new("classif.xgboost.booster", levels = "gblinear"),
    ParamInt$new("classif.xgboost.scale_pos_weight", lower = 2, upper = 60),
    ParamInt$new("classif.xgboost.max_depth", lower = 45, upper = 50),
    ParamDbl$new("classif.xgboost.eta", lower = .01, upper = .1),
    ParamInt$new("classif.xgboost.gamma", lower = 3, upper = 7)

  ))
)


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
