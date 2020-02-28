param_sets_plan = drake_plan(
  ps = ParamSet$new(list(
    # ParamDbl$new("information_gain.filter.frac", lower = .8, upper = .9),
    # ParamInt$new("smote.K", lower = 3, upper = 6),
    ParamInt$new("num.trees", lower = 500, upper = 600),
    ParamInt$new("mtry", lower = 20, upper = 40)
  ))
)

# readd(glrn)$param_set %>% as.data.table()


