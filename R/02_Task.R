#=============================================================================
# Task
#=============================================================================
task_plan = drake_plan(
  train_idx = 1:30000,
  test_idx = 30001:50000,
  task_regr = target({
    task = TaskRegr$new(id = "kalapa_reg", backend = dt_woe_regr, target = "label")
    task$col_roles$feature
    task$col_roles$feature = setdiff(task$col_roles$feature, "id")
    task$col_roles
    # split row_roles
    task$row_roles$use = train_idx
    task$row_roles$validation = test_idx
    return(task)
    }),
  task_classif = target({
    task = TaskClassif$new(id = "kalapa", backend = dt_woe_classif, target = "label")
    task$col_roles$feature
    task$col_roles$feature = setdiff(task$col_roles$feature, "id")
    task$col_roles
    # split row_roles
    task$row_roles$use = train_idx
    task$row_roles$validation = test_idx
  return(task)
  })
)
