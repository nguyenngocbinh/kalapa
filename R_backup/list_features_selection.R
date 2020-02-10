

# 3. Feature selection ------------------------------------------------------------
filter = mlr_pipeops$get("filter",
                         filter = mlr3filters::FilterImportance$new(),
                         param_vals = list(filter.frac = 0.2, xval = 5))
# filter = flt("auc")
# filter <- flt("importance", learner = lrn("classif.ranger", predict_type = "prob", importance = "impurity"))
#print(filter)
#new_filter <- filter$calculate(imputer_task)
#head(as.data.table(new_filter), 20)
#sel_importance <- head(as.data.table(new_filter), 10)$feature
