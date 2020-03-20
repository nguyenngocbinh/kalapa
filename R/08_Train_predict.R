
# train model
train_plan = drake_plan(
  rf_train = rf_tuned$train(task_classif)
)

#=============================================================================
# Predict
predict_plan = drake_plan(
  rf_predict = rf_train$predict(task_classif, row_ids = test_idx),
  #
  export_result = rf_predict %>%
    as.data.table() %>%
    select(id = row_id, label = prob.bad) %>%
    mutate(id = id - 1) %>%
    rio::export(paste0("results/result",stringr::str_replace_all(as.character(Sys.time()), ":", "-"), ".csv")),

  # export results
  export_parameter = rf_train$archive(unnest = "params") %>%
    select(contains("classif")) %>%
    rio::export(paste0("results/para", stringr::str_replace_all(as.character(Sys.time()), ":", "-"), ".xlsx"))
)

