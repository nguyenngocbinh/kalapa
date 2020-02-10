# Load packages, functions and plan --------------------------------------------

source("_packages.R")

sourceDirectory("R")

# set mlr3 options globally: suppress progress output of `benchmark()`
#lgr::get_logger("mlr3")$set_threshold("warn")

# Create plans -----------------------------------------------------------------

plan = bind_plans(clean_plan, task_plan, learner_plan, param_sets_plan,
                  resampling_plan, measures_plan,  tuning_plan, train_plan
)

# Set the config ---------------------------------------------------------------

drake_config(plan, verbose = 2, lock_envir = FALSE,
             # internal parallelization
             prework = quote(future::plan(future.callr::callr, workers = 4)),
             # logging
             console_log_file = here::here("log/drake.log"))

make(plan)
