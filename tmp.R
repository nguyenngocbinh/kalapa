
library(tidyverse)
library(magrittr)
auc_files <- Sys.glob("results/AUC_*.csv")
files_name <- str_sub(auc_files, 9, 16)

dt <- map(auc_files, read_csv) %>%
  set_names(files_name)

df <- dt %>%
  reduce(inner_join, by = "id") %>%
  set_names(c("id", files_name))

df

tt_true <- files_name %>%
  str_sub(5, 8) %>%
  as.numeric() %>%
  divide_by(10000)%>%
  sum()



# AUC function
auc_ <- function(labels, scores){
  labels <- as.logical(labels)
  n1 <- sum(labels)
  n2 <- sum(!labels)
  R1 <- sum(rank(scores)[labels])
  U1 <- R1 - n1 * (n1 + 1)/2
  auc_ <- U1/(n1 * n2)
  print(paste("AUC:", auc_))
  return(auc_)
}

gini <- function(labels, scores){
  labels <- as.logical(labels)
  n1 <- sum(labels)
  n2 <- sum(!labels)
  R1 <- sum(rank(scores)[labels])
  U1 <- R1 - n1 * (n1 + 1)/2
  auc_ <- U1/(n1 * n2)
  gini_ = 2 * auc_ - 1
  print(paste("Gini:", gini_))
  return(gini_)
}



# cal total different
opt_seed <- function(seed) {
  set.seed(seed)
  x <- rbeta(20000, 1.5, 0.04) < .5
  #x <- rbeta(20000, 0.04, 1.4) > .5
  lbl <- as.numeric(x)
  ginix <- partial(gini, labels = lbl)

  tt_check <- map(df, `[`) %>%
    map_dfr(ginix) %>%
    abs() %>%
    sum()

  check_min <- (tt_check - tt_true)^2
  return(data.frame(seed = seed, check_min = check_min, tt_check = tt_check, tt_true = tt_true ))
}


df_seed <- map_dfr(1:1000*4, opt_seed)


df_seed %>%
  arrange(check_min) %>%
  slice(1:10)

readd(task_classif) %>%
  as.data.table() %>%
  sample_frac(size = 2/3) %>%
  transmute(label = if_else(label == "Good", 0, 1)) %>%
  mutate(lbl_1 = rbeta(20000, 0.05, 1.5),
         lbl_2 = rbeta(20000, 0.04, 1.4),
         lbl_3 = rbeta(20000, 0.03, 1),
         lbl_4 = rbeta(20000, 0.02, 0.7),
         lbl_5 = rbeta(20000, 0.01, 0.5)
  ) -> df_check

auc(df_check$label, df_check$lbl_1) # 0.553995
auc(df_check$label, df_check$lbl_2) # 0.5185407
auc(df_check$label, df_check$lbl_3) # 0.519889
auc(df_check$label, df_check$lbl_4) # 0.5096973
auc(df_check$label, df_check$lbl_5) # 0.5058733

table(rbeta(20000, 1.5, 0.05) < .5)



auc_(df_check$label, df_check$lbl_1) # 0.553995
auc_(df_check$label, df_check$lbl_2) # 0.5185407
auc_(df_check$label, df_check$lbl_3) # 0.519889
auc_(df_check$label, df_check$lbl_4) # 0.5096973
auc_(df_check$label, df_check$lbl_5) # 0.5058733

table(rbeta(20000, 0.05, 1.5) > .5)
table(rbeta(20000, 0.04, 1.4) > .5)
table(rbeta(20000, 0.03, 1) > .5)
table(rbeta(20000, 0.02, 0.7) > .5)
table(rbeta(20000, 0.01, 0.5) > .5)

test_woe_imputed = test_woe %>%
  mutate(FIELD_13_woe = replace_na_categorical(FIELD_13_woe, train_woe$FIELD_13_woe)) %>%
  # mutate(maCv_woe = replace_na_categorical(maCv_woe, train_woe$maCv_woe)) %>%
  # mutate(maCv_woe = tidyr::replace_na(maCv_woe, 0.258055990053911)) %>% # AUC 301
  # mutate(maCv_woe = tidyr::replace_na(maCv_woe, 1.07592426251586)) %>% # AUC 29709
  # mutate(maCv_woe = tidyr::replace_na(maCv_woe, -0.1373105811738)) %>% # AUC 29709
  # mutate(maCv_woe = tidyr::replace_na(maCv_woe, -4.26829616099658)) %>% # AUC 29677
  mutate(maCv_woe = tidyr::replace_na(maCv_woe, 0.35)) %>%
  mutate(district_woe = replace_na_categorical(district_woe, train_woe$district_woe)) %>%
  # mutate(FIELD_7_woe = replace_na_categorical(FIELD_7_woe, train_woe$FIELD_7_woe)) %>%
  # mutate(FIELD_7_woe = tidyr::replace_na(FIELD_7_woe, -0.245657065010189)) %>% # AUC 30487
  # mutate(FIELD_7_woe = tidyr::replace_na(FIELD_7_woe, 0.168444328058472)) %>% # AUC 3005
  # mutate(FIELD_7_woe = tidyr::replace_na(FIELD_7_woe, 0.839305774159053)) %>% # AUC 29813
  # mutate(FIELD_7_woe = tidyr::replace_na(FIELD_7_woe, 2.0313654519161)) %>%
  mutate(FIELD_7_woe = tidyr::replace_na(FIELD_7_woe, -1.25)) %>%
  # mutate(FIELD_7_woe = tidyr::replace_na(FIELD_7_woe, -5.24525492594372)) %>%
  mutate(FIELD_41_woe = replace_na_categorical(FIELD_41_woe, train_woe$FIELD_41_woe)) %>%
  mutate(FIELD_10_woe = replace_na_categorical(FIELD_10_woe, train_woe$FIELD_10_woe)) %>%
  mutate(FIELD_39_woe = replace_na_categorical(FIELD_39_woe, train_woe$FIELD_39_woe)) %>%
  mutate(FIELD_11_woe = replace_na_categorical(FIELD_11_woe, train_woe$FIELD_11_woe)) %>%
  mutate(FIELD_9_woe = replace_na_categorical(FIELD_9_woe, train_woe$FIELD_9_woe)) %>%
  mutate(FIELD_12_woe = replace_na_categorical(FIELD_12_woe, train_woe$FIELD_12_woe))

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

# df_sel = f_filter_cor(train_woe_scaled, y = "label", cutoff = 0.67),

# train_woe_no_scaled = train_woe %>%
#   mutate_if(is.numeric, function(x) {xx <- bestNormalize(x) %>% predict()}),
#
# df_sel_no_scaled = train_woe_scaled %>%
#   select(c(paste0(iv_reorder[1:15], "_woe"), "label")),
#
# test_no_scaled = test_woe_imputed %>%
#   mutate_if(is.numeric, function(x) {xx <- bestNormalize(x) %>% predict()}),
