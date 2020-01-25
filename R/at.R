## 4.4. Auto tuner to evaluate performance
library(mlr3tuning)
tuner = tnr("grid_search", resolution = 10)
tuner = tnr("random_search")
at = AutoTuner$new(learner = glrn,
                   resampling = resampling_inner,
                   measures = measures,
                   tune_ps = ps,
                   terminator = terminator,
                   tuner = tuner)

resampling_outer = rsmp("cv", folds = 3)
rr = resample(task = task,
              learner = at,
              resampling = resampling_outer,
              store_models = TRUE)

# learner hyperparameters are stored in at
rr$aggregate()
rr$errors
rr$prediction()
rr$prediction()$confusion

rr$prediction() %>% as.data.table() %>% filter(truth == "bad") %>% pull(prob.bad) %>% mean()
