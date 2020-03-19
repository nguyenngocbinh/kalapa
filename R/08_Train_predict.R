
# train model
train_plan = drake_plan(
  rf_train = rf_tuned$train(task_classif)
)

# check results
readd(rf_train)$archive(unnest = "params") %>% arrange(-classif.auc)

#=============================================================================
# Predict
predict_plan = drake_plan(
  rf_predict = rf_train$predict(task_classif, row_ids = test_idx),
  export = rf_predict %>%
    as.data.table() %>%
    select(id = row_id, label = prob.bad) %>%
    mutate(id = id - 1) %>%
    rio::export("results/xgboost_output.csv")
)
