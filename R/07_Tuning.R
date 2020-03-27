#
# tuning_plan = drake_plan(
#   # AutoTuner = mlr::makeTunerWrapper()
#   terminator = target(term("evals", n_evals = 1)),
#
#   rf_tuned = target(AutoTuner$new(
#     learner = glrn, # from learner.R
#     resampling = resampling_inner,
#     measures = measures,
#     tune_ps = ps, # from param-sets.R
#     terminator = terminator, # tuning budget
#     #tuner = tnr("random_search")
#     tuner = tnr("grid_search")
#   ))
# )



tuning_plan = drake_plan(
  # AutoTuner = mlr::makeTunerWrapper()
  terminator = target(term("evals", n_evals = 30)),

  rf_tuned = target(AutoTuner$new(
    learner = glrn, # from learner.R
    resampling = resampling_inner,
    measures = measures,
    tune_ps = ps, # from param-sets.R
    terminator = terminator, # tuning budget
    tuner = tnr("random_search")

  ))
)
