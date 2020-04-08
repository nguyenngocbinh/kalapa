# Clear work space:
# rm(list = ls())

# Load data:
# library(drake)
# library(tidyverse)
# library(scorecard)
# library(ranger)

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
# A function imputes NA observations:
replace_na_categorical <- function(x, y) {

  y %>%
    table() %>%
    as.data.frame() %>%
    arrange(-Freq) -> my_df

  n_obs <- sum(my_df$Freq)

  pop <- my_df$. %>% as.character() %>% as.numeric()

  set.seed(29)

  x[is.na(x)] <- sample(pop, sum(is.na(x)), replace = TRUE, prob = my_df$Freq)

  return(x)
}

#=============================================================================
other_plan <- drake_plan(
  df_train = read_csv("data/train.csv") %>%
    mutate_if(is.character, str_to_upper),

  df_test = read_csv("data/test.csv") %>%
    mutate_if(is.character, str_to_upper) %>%
    mutate(FIELD_36 = as.logical(FIELD_36)),

  bins_var = woebin(df_train,
                    y = "label",
                    positive = "1",
                    # breaks_list = new_breaks_list,
                    save_breaks_list = "tmp/train_brk_list",
                    var_skip = "id"),

  sel_bins = bins_var,

  graphlist = woebin_plot(bins_var),

  iv_values = bins_var %>%
    map_dfr(bind_rows) %>%
    distinct(variable, .keep_all = TRUE) %>%
    arrange(-total_iv) %>%
    mutate(variable = factor(variable, levels = variable)),

  iv_reorder = iv_values %>%
    filter(total_iv >= 0) %>%
    pull(variable) %>%
    as.character(),

  # Conduct data transformation based on IV/WoE and filter features with IV > 0.1:
  #train_woe = woebin_ply(df_train, bins_var) %>%
  train_woe = woebin_ply(df_train, sel_bins) %>%
    as.data.frame() %>%
    select(c("label", paste0(iv_reorder, "_woe"))) %>%
    mutate(label = case_when(label == 1 ~ "Bad", TRUE ~ "Good")) %>%
    mutate(label = as.factor(label)),

  # Data transformation for actual test data:
  #test_woe = woebin_ply(df_test, bins_var) %>%
  test_woe = woebin_ply(df_test, sel_bins) %>%
    as.data.frame() %>%
    select(paste0(iv_reorder, "_woe")),

  test_woe_imputed = test_woe %>%
    mutate(FIELD_13_woe = replace_na_categorical(FIELD_13_woe, train_woe$FIELD_13_woe)) %>%
    mutate(maCv_woe = replace_na_categorical(maCv_woe, train_woe$maCv_woe)) %>%
    mutate(district_woe = replace_na_categorical(district_woe, train_woe$district_woe)) %>%
    mutate(FIELD_7_woe = replace_na_categorical(FIELD_7_woe, train_woe$FIELD_7_woe)) %>%
    mutate(FIELD_41_woe = replace_na_categorical(FIELD_41_woe, train_woe$FIELD_41_woe)) %>%
    mutate(FIELD_10_woe = replace_na_categorical(FIELD_10_woe, train_woe$FIELD_10_woe)) %>%
    mutate(FIELD_39_woe = replace_na_categorical(FIELD_39_woe, train_woe$FIELD_39_woe)) %>%
    mutate(FIELD_11_woe = replace_na_categorical(FIELD_11_woe, train_woe$FIELD_11_woe)) %>%
    mutate(FIELD_9_woe = replace_na_categorical(FIELD_9_woe, train_woe$FIELD_9_woe)) %>%
    mutate(FIELD_12_woe = replace_na_categorical(FIELD_12_woe, train_woe$FIELD_12_woe)),

  #======================================================
  # Attempt 4: Default Random Forest with Scaled Data
  #======================================================

  # For convinience, convert binary target variable to factor:

  train_woe_scaled = train_woe %>%
    mutate_if(is.numeric, function(x) {(x - min(x)) / (max(x) - min(x))}),

  test_woe_scaled = test_woe_imputed %>%
    mutate_if(is.numeric, function(x) {(x - min(x)) / (max(x) - min(x))}) ,

  # rf_for_importance = ranger(label ~ .,
  #                            data = train_woe_scaled,
  #                            probability = TRUE,
  #                            num.trees = 500,
  #                            importance = "impurity"),
  # vars_imp = importance(rf_for_importance),

  # df_sel = train_woe_scaled %>% select(c(paste0(iv_reorder[c(1:19, 22)], "_woe"), "label")),
  # df_sel_tmp = f_filter_cor(train_woe_scaled, y = "label", cutoff = 0.83),
  # sel_var = vars_imp %>%
  #   as.data.frame() %>%
  #   set_colnames("imp") %>%
  #   tibble::rownames_to_column() %>%
  #   arrange(-imp) %>%
  #   slice(1:15) %>%
  #   pull(rowname),
  # sel_var = names(df_sel_tmp),
  # sel_var = c(paste0(iv_reorder[c(1:14, 16, 19, 23, 26)], "_woe"), "label"), # AUC 28538
  # sel_var = c(paste0(iv_reorder[c(1:12, 14, 16, 19, 23, 26)], "_woe"), "label"), # AUC 28584 # 12 most importance
  # sel_var = c(paste0(iv_reorder[c(1:6, 8:12, 14, 16, 19, 23, 26)], "_woe"), "label"), # AUC 2859
  # sel_var = c(paste0(iv_reorder[c(1:5, 8:12, 14, 16, 19, 23, 26)], "_woe"), "label"), # AUC 29857
  # sel_var = c(paste0(iv_reorder[c(2:5, 8:12, 14, 16, 19, 23, 26)], "_woe"), "label"), # AUC 28961
  # sel_var = c(paste0(iv_reorder[c(1:5, 8:12, 14, 16, 19, 23, 26, 27)], "_woe"), "label"), # AUC 2832
  # sel_var = c(paste0(iv_reorder[c(2:5, 8:12, 14, 16, 19, 23, 26, 27)], "_woe"), "label"), # AUC 28309
  # sel_var = c(paste0(iv_reorder[c(1:5, 8:12, 14, 16, 19, 23, 26, 32)], "_woe"), "label"), # AUC 0.28633
  # sel_var = c(paste0(iv_reorder[c(1:5, 8:12, 14, 16, 19, 23, 26, 33)], "_woe"), "label"), # AUC 28604
  # sel_var = c(paste0(iv_reorder[c(1:5, 8:12, 14, 16, 19, 23, 26, 34)], "_woe"), "label"), # AUC 27602
  # sel_var = c(paste0(iv_reorder[c(1:5, 8:12, 14, 16, 19, 23, 26, 35)], "_woe"), "label"), # AUC 29141
  # sel_var = c(paste0(iv_reorder[c(1:5, 8:12, 14, 16, 19, 23, 26, 37)], "_woe"), "label"), # AUC 28856
  # sel_var = c(paste0(iv_reorder[c(1:5, 8:12, 14, 16, 19, 23, 26, 38)], "_woe"), "label"), # AUC 28253
  # sel_var = c(paste0(iv_reorder[c(1:5, 8:12, 14, 16, 19, 23, 26, 39)], "_woe"), "label"), # AUC 27976
  # sel_var = c(paste0(iv_reorder[c(1:5, 8:12, 14, 16, 19, 23, 26, 40)], "_woe"), "label"), # AUC 29251
  # sel_var = c(paste0(iv_reorder[c(1:5, 8:12, 14, 16, 19, 23, 26, 41)], "_woe"), "label"), # AUC 28114
  # sel_var = c(paste0(iv_reorder[c(1:5, 8:12, 14, 16, 19, 23, 26, 42)], "_woe"), "label"), # AUC 28742
  # sel_var = c(paste0(iv_reorder[c(1:5, 8:12, 14, 16, 19, 23, 26, 43)], "_woe"), "label"), # AUC 28742
  # sel_var = c(paste0(iv_reorder[c(1:5, 8:12, 14, 16, 19, 23, 26, 44)], "_woe"), "label"), # AUC 28742 # AUC ko tang ko giam so voi 42, 43
  # sel_var = c(paste0(iv_reorder[c(1:3, 5, 8, 9, 11, 12, 14, 16, 19, 23, 26)], "_woe"), "label"), # 27988
  # sel_var = c(paste0(iv_reorder[c(1:3, 5, 8, 9, 10, 12, 14, 16, 19, 23, 26)], "_woe"), "label"), # 29267
  # sel_var = c(paste0(iv_reorder[c(1:4, 8, 9, 10, 12, 14, 16, 19, 23, 26)], "_woe"), "label"), # 28691
  # sel_var = c(paste0(iv_reorder[c(2, 3, 9, 12)], "_woe"), "label"), # AUC 21045
  # sel_var = c(paste0(iv_reorder[c(2, 3, 9:12)], "_woe"), "label"), # AUC 227
  # sel_var = c(paste0(iv_reorder[c(2, 3, 9:12, 14)], "_woe"), "label"), # AUC 21655
  # sel_var = c(paste0(iv_reorder[c(2, 3, 9:12, 16)], "_woe"), "label"), # AUC 23301
  # sel_var = c(paste0(iv_reorder[c(2, 3, 9:12, 16, 19)], "_woe"), "label"), # AUC 2614
  # sel_var = c(paste0(iv_reorder[c(2, 3, 9:12, 16, 19, 23)], "_woe"), "label"), # AUC 27231
  # sel_var = c(paste0(iv_reorder[c(2, 3, 9:12, 16, 19, 23, 26)], "_woe"), "label"), # AUC 27572
  # sel_var = c(paste0(iv_reorder[c(2:4, 9:12, 16, 19, 23, 26)], "_woe"), "label"), # AUC 27532
  # sel_var = c(paste0(iv_reorder[c(2, 3, 5, 9:12, 16, 19, 23, 26)], "_woe"), "label"), # AUC 28276
  # sel_var = c(paste0(iv_reorder[c(1:3, 5, 9:12, 16, 19, 23, 26)], "_woe"), "label"), # AUC 28064
  # sel_var = c(paste0(iv_reorder[c(2, 3, 5, 9:12, 14, 16, 19, 23)], "_woe"), "label"), # AUC 26568 # bo 14, bien kem nhat
  # can nhac bo bien 9
  # sel_var = c(paste0(iv_reorder[c(2, 3, 5, 10:12, 14, 16, 19, 23)], "_woe"), "label"), #26947
  # sel_var = c(paste0(iv_reorder[c(1:3, 5, 10:12, 14, 16, 19, 23)], "_woe"), "label"), # 27348
  # sel_var = c(paste0(iv_reorder[c(1:3, 5, 10:12, 16, 19, 23)], "_woe"), "label"), # 26333
  # sel_var = c(paste0(iv_reorder[c(1:5, 8:12, 14, 16, 19, 23, 26, 55)], "_woe"), "label"),

  sel_var = c(paste0(c("FIELD_7", "district", "maCv", "province", "FIELD_13", "FIELD_30", "FIELD_51",
               "FIELD_17", "FIELD_31", "FIELD_22", "FIELD_12", "FIELD_9", "FIELD_3", "FIELD_6",
               "age_source1"), "_woe" ), "label"),

  best_var = c("FIELD_7", "district", "maCv", "province", "FIELD_13", "FIELD_30", "FIELD_51",
               "FIELD_17", "FIELD_31", "FIELD_22", "FIELD_12", "FIELD_9", "FIELD_3", "FIELD_6",
               "age_source1"),

  df_sel = train_woe_scaled[, sel_var],
  # df_sel = f_filter_cor(train_woe_scaled, y = "label", cutoff = 0.67),

  # train_woe_no_scaled = train_woe %>%
  #   mutate_if(is.numeric, function(x) {xx <- bestNormalize(x) %>% predict()}),
  #
  # df_sel_no_scaled = train_woe_scaled %>%
  #   select(c(paste0(iv_reorder[1:15], "_woe"), "label")),
  #
  # test_no_scaled = test_woe_imputed %>%
  #   mutate_if(is.numeric, function(x) {xx <- bestNormalize(x) %>% predict()}),

  # Train Random Forest:
  rf_model_scaled = ranger(label ~ .,
                           data = df_sel,
                           probability = TRUE,
                           num.trees = 900,
                           # class.weights = c(1, 60),
                           importance = "impurity"),

    # Use the RF Classifier for predicting PD (Probability of Default):
  rf_pred_scaled = predict(rf_model_scaled, test_woe_scaled, type = "response"),
  # rf_pred_no_scaled = predict(rf_model_no_scaled, test_no_scaled, type = "response")

  # Export
  rf_export = rf_pred_scaled$predictions %>%
    as.data.frame() %>%
    mutate(id = 30000:49999) %>%
    select(id, label = Bad) %>%
    write_csv(paste0("results/rf",stringr::str_replace_all(as.character(Sys.time()), ":", "-"), ".csv"))

)

# make(other_plan)


# # Use the function:
#
#
# # Save results for submission:
#
# readd(rf_pred_scaled)$predictions %>% as.data.frame() %>% pull(Bad) -> rf_pred_scaled
# df_sub <- data.frame(id = 30000:49999, label = rf_pred_scaled)
# write_csv(df_sub, paste0("results/rf",stringr::str_replace_all(as.character(Sys.time()), ":", "-"), ".csv"))

