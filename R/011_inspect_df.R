inspect_plan = drake_plan(
  ## 1.1. Check target variable
  ### 1.1.1. Missing
  target_mis = train$label %>% is.na() %>% table(),  # No missing

  ### 1.1.2. Imbalance
  target_imb = train$label %>% table() %>% prop.table(),  # imbalanced


  ## 1.2. Check missing predictors
  check_missing = inspect_na(train, test),  # some fields can be removed
  # check_missing %>% show_plot()

  ## 1.3. check inbalance predictors
  check_imb = inspect_imb(train, test, include_na = TRUE),

  ## 1.4 check type
  check_types = inspect_types(train, test)
)

