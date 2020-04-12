
readd(test_woe)[, setdiff(readd(sel_var), "label")] -> dfx
readd(test_woe_imputed)[, setdiff(readd(sel_var), "label")] -> dfxx
readd(test_woe_imputed) -> dfxxx
readd(dif_woe_scaled) -> df_dif

rf_pred_train = predict(readd(rf_model_scaled), readd(train_woe_scaled), type = "response")
rf_pred_train$predictions %>%
  as.data.frame() %>%
  mutate(id = 0:29999) %>%
  select(id, Bad) %>%
  cbind(readd(train_woe)) -> df_trainx


df_trainx %>%
  group_by(FIELD_7_woe, maCv_woe) %>%
  summarise(mean(Bad), min(Bad), max(Bad))

# check id missing
dfx %>%
  mutate(id = 30000:49999) %>%
  filter(is.na(FIELD_7_woe) | is.na(maCv_woe)) -> df_missing

df_missing %>% pull(id)

#
names(dfx)

dfx %>%
  map(is.na) %>%
  map(table)

dfx$FIELD_7_woe %>%
  table() %>%
  prop.table()

dfxx$FIELD_7_woe %>%
  table() %>%
  prop.table()

rf_auc <- read_csv("results/AUC_30487_rf_field_7_mid.csv")


rf_field_7_max <- read_csv("results/rf_field_7_max.csv") %>%
  transmute(low_auc = label)

rf_field_7_min <- read_csv("results/rf_field_7.csv") %>%
  transmute(low_auc2 = label)

AUC_29488_a2 <- read_csv("results/AUC_29488_a2.csv") %>%
  transmute(low_auc3 = label)

AUC_2544_a1 <- read_csv("results/AUC_2544_a1.csv") %>%
  transmute(low_auc4 = label)

dfx %>%
  bind_cols(rf_auc) %>%
  bind_cols(rf_field_7_max) %>%
  bind_cols(rf_field_7_min) %>%
  bind_cols(AUC_29488_a2) %>%
  bind_cols(AUC_2544_a1) %>%
  mutate(id = 30000:49999) %>%
  # mutate(new_lbl = case_when(is.na(FIELD_7_woe) | is.na(maCv_woe) ~ label - 0.001,
  #                            TRUE ~ label)) %>%
  mutate(new_lbl = case_when(is.na(FIELD_7_woe) ~ label + 0.002,
                             TRUE ~ label)) %>%
  mutate(rnk_hi = rank(label),
         rnk_new = rank(new_lbl),
         rnk_lo = rank(low_auc),
         rnk_lo2 = rank(low_auc2),
         rnk_lo3 = rank(low_auc3),
         rnk_lo4 = rank(low_auc4),
         dif1 = abs(rnk_hi - rnk_lo),
         dif2 = abs(rnk_hi - rnk_lo2),
         dif3 = abs(rnk_hi - rnk_lo3),
         dif4 = abs(rnk_hi - rnk_lo4),
         difx = abs(rnk_lo3 - rnk_lo4)
         )-> dfy

# preddd <- dfy %>%
#   select(id, label = new_lbl)

# write_csv(preddd, paste0("results/rf",stringr::str_replace_all(as.character(Sys.time()), ":", "-"), ".csv"))

dfy %>%
  filter(is.na(FIELD_7_woe) | is.na(maCv_woe)) -> df_missing

inspect_num(df_missing) -> insx

insx %>% show_plot()

dfy %>%
  filter(!is.na(FIELD_7_woe) & !is.na(maCv_woe)) -> df_nomissing

df_missing %>%
  select(label, new_lbl, low_auc, low_auc2, low_auc3, low_auc4, starts_with("rnk"), starts_with("dif")) %>%
  summary()

df_missing %>%
  group_by(FIELD_7_woe) %>%
  summarise(median(label), median(low_auc), mean(label), mean(low_auc))

table(df_missing$FIELD_7_woe, df_missing$maCv_woe, useNA = "always")
table(df_missing$maCv_woe, df_missing$FIELD_7_woe, useNA = "always")

df_nomissing %>%
  select(label, new_lbl, low_auc, low_auc2, low_auc3, low_auc4, starts_with("rnk"), starts_with("dif")) %>%
  summary()


df_missing$label %>% hist()
df_missing$low_auc3 %>% hist()

df_missing %>%
  filter(label != low_auc) -> dfz



dfz$rnk_hi %>% sum()
dfz$rnk_lo %>% sum()
dfz$rnk_lo2 %>% sum()

dfz %>%
  group_by(FIELD_7_woe) %>%
  summarise(mean(rnk_hi), mean(rnk_lo), mean(rnk_lo2), n())

dfz %>%
  group_by(maCv_woe) %>%
  summarise(mean(rnk_hi), mean(rnk_lo), mean(rnk_lo2), n())

dfy %>%
  group_by(maCv_woe, FIELD_7_woe) %>%
  summarise(mean(rnk_hi), mean(rnk_lo), mean(rnk_lo2), n(), mean(label)) %>%
  rio::export("field_7_macv.xlsx")

dfy %>%
  group_by(FIELD_7_woe, maCv_woe) %>%
  summarise(mean(rnk_hi), mean(rnk_lo), mean(rnk_lo2), n(), mean(label)) %>%
  rio::export("field_7_macv_x.xlsx")

dfx$maCv_woe %>%
  table() %>%
  prop.table()

dfxxx$maCv_woe %>%
  table() %>%
  prop.table()


dfxx$FIELD_7_woe %>%
  table() %>%
  prop.table()


dfxxx$FIELD_7_woe %>%
  table() %>%
  prop.table()

