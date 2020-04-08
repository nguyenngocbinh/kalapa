
readd(df_sel) %>% names()
readd(var_IV_10)[1:19]

f_filter_cor(readd(train_woe_scaled), y = "label", cutoff = 0.6) %>% names()
f_filter_cor(readd(train_woe_scaled), y = "label", cutoff = 0.61) %>% names()
f_filter_cor(readd(train_woe_scaled), y = "label", cutoff = 0.62) %>% names()
f_filter_cor(readd(train_woe_scaled), y = "label", cutoff = 0.63) %>% names()
f_filter_cor(readd(train_woe_scaled), y = "label", cutoff = 0.64) %>% names()
f_filter_cor(readd(train_woe_scaled), y = "label", cutoff = 0.65) %>% names() # = .66
f_filter_cor(readd(train_woe_scaled), y = "label", cutoff = 0.66) %>% names()
f_filter_cor(readd(train_woe_scaled), y = "label", cutoff = 0.67) %>% names() # = .68
f_filter_cor(readd(train_woe_scaled), y = "label", cutoff = 0.68) %>% names()
f_filter_cor(readd(train_woe_scaled), y = "label", cutoff = 0.69) %>% names() # = .7
f_filter_cor(readd(train_woe_scaled), y = "label", cutoff = 0.7) %>% names()
f_filter_cor(readd(train_woe_scaled), y = "label", cutoff = 0.71) %>% names()
f_filter_cor(readd(train_woe_scaled), y = "label", cutoff = 0.72) %>% names()
f_filter_cor(readd(train_woe_scaled), y = "label", cutoff = 0.73) %>% names()
f_filter_cor(readd(train_woe_scaled), y = "label", cutoff = 0.74) %>% names()
f_filter_cor(readd(train_woe_scaled), y = "label", cutoff = 0.75) %>% names()
f_filter_cor(readd(train_woe_scaled), y = "label", cutoff = 0.76) %>% names() #
f_filter_cor(readd(train_woe_scaled), y = "label", cutoff = 0.77) %>% names()
f_filter_cor(readd(train_woe_scaled), y = "label", cutoff = 0.77) %>% names()
f_filter_cor(readd(train_woe_scaled), y = "label", cutoff = 0.78) %>% names()
f_filter_cor(readd(train_woe_scaled), y = "label", cutoff = 0.79) %>% names() #
f_filter_cor(readd(train_woe_scaled), y = "label", cutoff = 0.81) %>% names()
f_filter_cor(readd(train_woe_scaled), y = "label", cutoff = 0.82) %>% names()#
f_filter_cor(readd(train_woe_scaled), y = "label", cutoff = 0.83) %>% names()
f_filter_cor(readd(train_woe_scaled), y = "label", cutoff = 0.84) %>% names()


x %>%
  inner_join(y, by = "id") %>%
  mutate(dif = label.x - label.y)-> z
