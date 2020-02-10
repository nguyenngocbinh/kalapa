
df %>%
  group_by(maCv) %>%
  count() -> xx

View(xx)

df %>% filter(!is.na(label)) -> dx

scorecard::iv(dt = dx, y = "label", x = "maCv")

get_auc(dx$maCv, dx$label)
get_auc(dx$maCv, dx$label) # 0.5881506
get_auc(dx$FIELD_7, dx$label)

scorecard::iv(dt = dx, y = "label", x = "maCv")

?scorecard::woebin_plot()

bins <- scorecard::woebin(cleaned_dt, y = "label", var_skip = c("id"), bin_num_limit = 8, check_cate_num = FALSE)
plotlist = scorecard::woebin_plot(bins)


for (i in 1:length(plotlist)) {
  ggplot2::ggsave(
     paste0("output/",names(plotlist[i]), ".png"), plotlist[[i]],
     width = 15, height = 9, units="cm" )
  }

increase_iv <- function(x) {
  dset[, x]
}

x <- "age_source1"

d_sel <- dset[, c(x, "label")]
op1 <- scorecard::iv(dt = d_sel, y = "label", x = x)

d_sel2 <- d_sel
d_sel2[[x]] <- fimpute_numeric(d_sel[[x]])
op2 <- scorecard::iv(dt = d_sel2, y = "label", x = x)

bin_x <- scorecard::woebin(d_sel, y = "label", bin_num_limit = 10)
bin_x <- scorecard::woebin(d_sel2, y = "label", bin_num_limit = 10)
bin_x$age_source1$total_iv
d_woe <- woebin_ply(dt = d_sel2, bins = bin_x,to = "woe" )

d_woe <- d_woe %>% filter(!is.na(label))
d_sel2 <- d_sel2 %>% filter(!is.na(label))

get_auc(d_woe$age_source1_woe, d_woe$label)
get_auc(d_sel2$age_source1, d_sel2$label)



f_return_impute_vars <- function() {
  d_impute <- dset %>% filter(!is.na(label)) %>% mutate_at(new_numeric_vars, fimpute_numeric) %>% select(c(new_numeric_vars, "label"))
  d_no_impute <- dset %>% filter(!is.na(label)) %>% select(c(new_numeric_vars, "label"))

  bin_1 <- woebin(d_impute, y = "label")
  d_woe_1 <- woebin_ply(dt = d_impute, bins = bin_1, to = "woe" )

  bin_2 <- woebin(d_no_impute, y = "label")
  d_woe_1 <- woebin_ply(dt = d_no_impute, bins = bin_2,to = "woe" )

  auc1 <- d_impute %>%
    select(new_numeric_vars) %>%
    map_df(get_auc, y = d_impute$label)

  auc2 <- d_no_impute %>%
    select(new_numeric_vars) %>%
    map_df(get_auc, y = d_no_impute$label)

  auc_check <- rbind(auc1, auc2) %>%
    t() %>%
    as.data.frame() %>%
    mutate(v3 = V1 > V2) %>%
    set_rownames(new_numeric_vars) %>%
    tibble::rownames_to_column()

  list_impute_var <- auc_check %>% filter(v3 == TRUE) %>% pull(rowname)

}
