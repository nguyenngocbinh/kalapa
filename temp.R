"classif.auc",	"classif.xgboost.eta",	"classif.xgboost.gamma",
"classif.xgboost.max_depth",	"classif.xgboost.min_child_weight",
"classif.xgboost.subsample",	"classif.xgboost.max_delta_step",
"classif.xgboost.nrounds",	"classif.xgboost.verbose",
"classif.xgboost.scale_pos_weight"

readd(rf_train)$archive(unnest = "params")[, c("classif.xgboost.eta",
                                               "classif.xgboost.gamma",
                                               "classif.xgboost.max_depth",
                                               "classif.xgboost.min_child_weight",
                                               "classif.xgboost.subsample",
                                               "classif.xgboost.max_delta_step",
                                               "classif.xgboost.nrounds",
                                               "classif.xgboost.scale_pos_weight",
                                               "classif.auc")] %>% cor()



x <- Sys.glob("results/*.xlsx") %>%
  purrr::map_dfr(readxl::read_xlsx)


x[, c("classif.xgboost.eta",
      "classif.xgboost.gamma",
      "classif.xgboost.max_depth",
      "classif.xgboost.min_child_weight",
      "classif.xgboost.subsample",
      "classif.xgboost.max_delta_step",
      "classif.xgboost.nrounds",
      "classif.xgboost.scale_pos_weight",
      "classif.auc")] %>% cor()

library(ggplot2)
x %>%
  ggplot(aes(classif.xgboost.nrounds, classif.auc))+
  geom_point()

