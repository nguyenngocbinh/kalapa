# First: 15-jan-2020
# Update: 20-jan-2020
# Author: NNB
# Contents: ultility functions

#=============================================================================
# III. Các hàm thống kê
f_numeric_stats <- function(x){
  table_stats <- data.frame(N = length(x),
                            Min = min(x, na.rm = T), 
                            Max = max(x, na.rm = T),
                            Mean = mean(x, na.rm = T),
                            Std = sd(x, na.rm = T),
                            `#Zero` = length(x[x %in% 0]),
                            `%Zero` = length(x[x %in% 0])/length(x)*100,
                            `#NA` = is.na(x) %>% sum(),
                            `%NA` = is.na(x) %>% sum()/length(x)*100)
  return(table_stats)
}

# check range function
f_check_range <- function(df, file_name = "check_range"){
  output_file <- paste0("output/", file_name, ".xlsx")
  
  df %>% 
   # mutate_all(as.factor) %>% 
    inspect_cat() %>% 
    pull(levels) %>% 
    writexl::write_xlsx(output_file)
  
  print(output_file)
  
}

