

## 2.4 Imbalanced adjustment
# pop = po("smote")
opb = po("classbalancing")
opb$param_set$values = list(ratio = 21, reference = "minor",
                            adjust = "minor", shuffle = FALSE)
# check result
result_opb = opb$train(list(task))[[1L]]
table(result_opb$truth())
