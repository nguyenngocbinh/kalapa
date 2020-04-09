rank

readd(test_woe)[, setdiff(readd(sel_var), "label")] -> dfx
readd(test_woe_imputed)[, setdiff(readd(sel_var), "label")] -> dfxx

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


rf_field_7_max <- read_csv("results/rf_field_7_max.csv")
rf_field_7_max <- rf_field_7_max %>% transmute(low_auc = label)

rf_field_7_min <- read_csv("results/rf_field_7.csv") %>%
  transmute(low_auc2 = label)


dfx %>%
  bind_cols(rf_auc) %>%
  bind_cols(rf_field_7_max) %>%
  bind_cols(rf_field_7_min) %>%
  mutate(rnk_hi = rank(label),
         rnk_lo = rank(low_auc),
         rnk_lo2 = rank(low_auc2))-> dfy

dfy %>%
  filter(is.na(FIELD_7_woe)) -> dfz

dfz$rnk_hi %>% sum()
dfz$rnk_lo %>% sum()

dfy %>%
  group_by(FIELD_7_woe) %>%
  summarise(mean(rnk_hi), mean(rnk_lo), mean(rnk_lo2), n())

dfy %>%
  group_by(maCv_woe) %>%
  summarise(mean(rnk_hi), mean(rnk_lo), mean(rnk_lo2), n())

dfy %>%
  group_by(maCv_woe, FIELD_7_woe) %>%
  summarise(mean(rnk_hi), mean(rnk_lo), mean(rnk_lo2), n(), mean(label)) %>%
  rio::export("field_7_macv.xlsx")


dfx$maCv_woe %>%
  table() %>%
  prop.table()

dfxx$FIELD_7_woe %>%
  table() %>%
  prop.table()

