# guide
# as.data.table(mlr_resamplings)

resampling_plan = drake_plan(
  resampling_inner = target({
    rsmp_in = rsmp("cv")
    rsmp_in$param_set$values = list(folds = 3)
    return(rsmp_in)
  }),

  resampling_outer = target({
    rsmp_out = rsmp("repeated_cv")
    rsmp_out$param_set$values = list(folds = 3, repeats = 1)
    return(rsmp_out)
  })
)
