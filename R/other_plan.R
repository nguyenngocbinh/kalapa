#=========================================
#  A Naive Approach for Data Processing
#=========================================

# Clear work space:
# rm(list = ls())

# Load data:
# library(drake)
# library(tidyverse)
# library(scorecard)
# library(ranger)

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
                    no_cores = 8,
                    positive = "label|1",
                    save_breaks_list = "tmp/train_brk_list.R",
                    var_skip = "id"),

  iv_values = bins_var %>%
    map_dfr(bind_rows) %>%
    distinct(variable, .keep_all = TRUE) %>%
    arrange(-total_iv) %>%
    mutate(variable = factor(variable, levels = variable)),

  var_IV_10 = iv_values %>%
    filter(total_iv >= 0) %>%
    pull(variable) %>%
    as.character(),

  # Conduct data transformation based on IV/WoE and filter features with IV > 0.1:
  train_woe = woebin_ply(df_train, bins_var) %>%
    as.data.frame() %>%
    select(c("label", paste0(var_IV_10, "_", "woe"))),


  # Data transformation for actual test data:
  test_woe = woebin_ply(df_test, bins_var) %>%
    as.data.frame() %>%
    select(paste0(var_IV_10, "_", "woe")),

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

  df_forGBM = train_woe %>%
    mutate(label = case_when(label == 1 ~ "Bad", TRUE ~ "Good")) %>%
    mutate(label = as.factor(label)),

  # Scale our data:

  df_forGBM_Scaled = df_forGBM %>%
    mutate_if(is.numeric, function(x) {(x - min(x)) / (max(x) - min(x))}),

  df_test_Scaled = test_woe_imputed %>%
    mutate_if(is.numeric, function(x) {(x - min(x)) / (max(x) - min(x))}) ,

  df_sel = df_forGBM_Scaled %>% select(c(paste0(var_IV_10[1:19], "_", "woe"), "label")),

  # Train Random Forest:

  RF_default = ranger(label ~ ., data = df_sel, probability = TRUE, num.trees = 500),

  # Use the RF Classifier for predicting PD (Probability of Default):
  pd_sub_RF = predict(RF_default, df_test_Scaled, type = "response")

)

# make(other_plan)


# # Use the function:
#
#
# # Save results for submission:
#
# readd(pd_sub_RF)$predictions %>% as.data.frame() %>% pull(Bad) -> pd_sub_RF
# df_sub <- data.frame(id = 30000:49999, label = pd_sub_RF)
# write_csv(df_sub, paste0("results/rf",stringr::str_replace_all(as.character(Sys.time()), ":", "-"), ".csv"))

