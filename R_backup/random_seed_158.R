# Clear work space:
# rm(list = ls())

source("_packages.R")

f_recode_str <- function(st){
  library(stringr)
  st <- st %>%
    stringi::stri_trans_general(id = "latin-ascii" ) %>%
    str_replace_all('\\?', replacement = '') %>%
    stringr::str_squish() %>%
    toupper()
}

#' @title impute numeric variables
f_impute_numeric <- function(feature, seed = 158) {
  outlen <- sum(is.na(feature))
  non_na_feature <- na.omit(feature)
  set.seed(seed)
  filldata <- sample(non_na_feature, outlen, replace = TRUE)
  feature[is.na(feature)] <- filldata
  feature
}


#=============================================================================
other_plan <- drake_plan(
  #---------------------------------------------------------------------------
  # 1. read data from disk
  #---------------------------------------------------------------------------
  df_train = read_csv("data/train.csv") %>%
    mutate_if(is.character, str_to_upper),

  df_test = read_csv("data/test.csv") %>%
    mutate_if(is.character, str_to_upper) %>%
    mutate(FIELD_36 = as.logical(FIELD_36)),

  #---------------------------------------------------------------------------
  # 2. Binning and woe tranformation
  #---------------------------------------------------------------------------
  bins_var = woebin(df_train,
                    y = "label",
                    positive = "1",
                    # breaks_list = new_breaks_list,
                    save_breaks_list = "tmp/train_brk_list",
                    var_skip = "id"),

  # select vars to bin
  sel_bins = bins_var,

  # For check binning graph
  graphlist = woebin_plot(bins_var),

  # arrange importance variables by information value
  iv_values = bins_var %>%
    map_dfr(bind_rows) %>%
    distinct(variable, .keep_all = TRUE) %>%
    arrange(-total_iv) %>%
    mutate(variable = factor(variable, levels = variable)),

  # list vars by decreasing information value
  iv_reorder = iv_values %>%
    filter(total_iv >= 0) %>%
    pull(variable) %>%
    as.character(),

  # Apply woe values for original data
  train_woe = woebin_ply(df_train, sel_bins) %>%
    as.data.frame() %>%
    select(c("label", paste0(iv_reorder, "_woe"))) %>%
    mutate(label = case_when(label == 1 ~ "Bad", TRUE ~ "Good")) %>%
    mutate(label = as.factor(label)),

  # Convert original test data to woe data
  test_woe = woebin_ply(df_test, sel_bins) %>%
    as.data.frame() %>%
    select(paste0(iv_reorder, "_woe")),

  # Data transformation
  train_woe_scaled = train_woe %>%
    mutate_if(is.numeric, function(x) {(x - min(x)) / (max(x) - min(x))}),

  sel_var = c(paste0(c("FIELD_7", "district", "maCv", "province", "FIELD_13", "FIELD_30", "FIELD_51",
                       "FIELD_17", "FIELD_31", "FIELD_22", "FIELD_12", "FIELD_9", "FIELD_3", "FIELD_6",
                       "age_source1"), "_woe" ), "label"),

  best_var = c("FIELD_7", "district", "maCv", "province", "FIELD_13", "FIELD_30", "FIELD_51",
               "FIELD_17", "FIELD_31", "FIELD_22", "FIELD_12", "FIELD_9", "FIELD_3", "FIELD_6",
               "age_source1"),

  df_sel = train_woe_scaled[, sel_var],

  # Train Random Forest:
  rf_model_scaled = ranger(label ~ .,
                           data = df_sel,
                           probability = TRUE,
                           num.trees = 1200,
                           # class.weights = c(1, 60),
                           importance = "impurity"),

  # Impute missing value in test_woe data
  # test_woe_imputed = test_woe %>%
  #   mutate_if(is.numeric, f_impute_numeric),


  test_woe_scaled = scorecard::var_scale(test_woe, type = "minmax") %>%
    as.data.frame(),
  # Impute missing value in test_woe data with amelia
  a.out = amelia(test_woe_scaled[, paste0(best_var, "_woe")]),

  # df_test_sel = a.out$imputations$imp1, # 2544
  # df_test_sel = a.out$imputations$imp2, # 29488
  # df_test_sel = a.out$imputations$imp3, # 28293
  # df_test_sel = a.out$imputations$imp4, # 28293

  # Impute missing value in test_woe data with mice
  imp = mice(test_woe_scaled[, paste0(best_var, "_woe")], method = "rf"),
  # df_test_sel = mice::complete(imp, action = 3),
  df_test_sel = test_woe %>%
    mutate(FIELD_7_woe = case_when(is.na(FIELD_7_woe) &
                                     round(maCv_woe, 2) == .26 ~  -5.25,
                                   is.na(FIELD_7_woe) &
                                     round(maCv_woe, 2)  == -.14 ~ -0.245657065010189,
                                   is.na(FIELD_7_woe) &
                                     round(maCv_woe, 2)  == 1.08 ~ 0.168444328058472,
                                   # is.na(FIELD_7_woe) ~ 0.168444328058472, #2.0313654519161,
                                   TRUE ~ FIELD_7_woe)) %>%
    mutate(maCv_woe = tidyr::replace_na(maCv_woe, 0.258055990053911)) %>%
    mutate_if(is.numeric, f_impute_numeric) %>%
    mutate_if(is.numeric, function(x) {(x - min(x)) / (max(x) - min(x))}),

  # Predict
  rf_pred_scaled = predict(rf_model_scaled, df_test_sel, type = "response"),

  # Export results to submit
  rf_export = rf_pred_scaled$predictions %>%
    as.data.frame() %>%
    mutate(id = 30000:49999) %>%
    select(id, label = Bad) %>%
    write_csv(paste0("results/rf",stringr::str_replace_all(as.character(Sys.time()), ":", "-"), ".csv"))
)

# make(other_plan)
