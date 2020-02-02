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

f_recode_macv <- function(st){
  library(stringr)
  st <- st %>%
    toupper() %>%
    stringi::stri_trans_general(id = "latin-ascii" ) %>%
    stringr::str_squish()

  clean_st <-  case_when(st %>% str_detect('CONG NHAN') ~ 'CONG NHAN',
                         st %>% str_detect('C.N') ~ 'CONG NHAN',
                         st %>% str_detect('C.NHAN') ~ 'CONG NHAN',
                         st %>% str_detect('CON GNHAN') ~ 'CONG NHAN',
                         st %>% str_detect('CN') ~ 'CONG NHAN',
                         st %>% str_detect('COONG NHAON') ~ 'CONG NHAN',
                         st %>% str_detect('LAO DONG') ~ 'CONG NHAN',
                         st %>% str_detect('LDPT') ~ 'CONG NHAN',
                         st %>% str_detect('VAN HANH') ~ 'CONG NHAN',
                         st %>% str_detect('THO') ~ 'CONG NHAN',

                         st %>% str_detect('NHAN VIEN') ~ 'NHAN VIEN',
                         st %>% str_detect('NV') ~ 'NHAN VIEN',
                         st %>% str_detect('N.V') ~ 'NHAN VIEN',
                         st %>% str_detect('BAN HANG') ~ 'NHAN VIEN',
                         st %>% str_detect('GDV') ~ 'NHAN VIEN',
                         st %>% str_detect('GIAO DICH VIEN') ~ 'NHAN VIEN',
                         st %>% str_detect('CHUYEN VIEN') ~ 'NHAN VIEN',

                         st %>% str_detect('TRO LY') ~ 'TRO LY - THU KY',
                         st %>% str_detect('THU KY') ~ 'TRO LY - THU KY',

                         st %>% str_detect('BAO VE') ~ 'BAO VE',
                         st %>% str_detect('B. VE') ~ 'BAO VE',
                         st %>% str_detect('B.VE') ~ 'BAO VE',
                         st %>% str_detect('BVE') ~ 'BAO VE',

                         st %>% str_detect('BAC SI') ~ 'BAC SY',
                         st %>% str_detect('BAC SY') ~ 'BAC SY',
                         st %>% str_detect('BS.') ~ 'BAC SY',
                         st %>% str_detect('DUOC SY') ~ 'DUOC SY',
                         st %>% str_detect('DUOC SI') ~ 'DUOC SY',
                         st %>% str_detect('DUOC TA') ~ 'DUOC SY',
                         st %>% str_detect('Y SY') ~ 'DUOC SY',
                         st %>% str_detect('Y SI') ~ 'DUOC SY',
                         st %>% str_detect('DIEU DUONG') ~ 'DIEU DUONG',
                         st %>% str_detect('HO SINH') ~ 'DIEU DUONG',
                         st %>% str_detect('HO LY') ~ 'DIEU DUONG',
                         st %>% str_detect('Y TA') ~ 'DIEU DUONG',



                         st %>% str_detect('BI THU') ~ 'BI THU',
                         st %>% str_detect('BI TU XA DOAN') ~ 'BI THU',

                         st %>% str_detect('CAN BO') ~ 'CAN BO',
                         st %>% str_detect('CB') ~ 'CAN BO',
                         st %>% str_detect('CHU TICH') ~ 'CAN BO',
                         st %>% str_detect('DIA CHINH') ~ 'CAN BO',
                         st %>% str_detect('TU PHAP') ~ 'CAN BO',
                         st %>% str_detect('VAN PHoNG') ~ 'CAN BO',
                         st %>% str_detect('VIEN CHUC') ~ 'CAN BO',

                         st %>% str_detect('DAI DIEN BAN HANG') ~ 'DAI DIEN BAN HANG',
                         st %>% str_detect('DIEN VIEN') ~ 'DIEN VIEN',
                         st %>% str_detect('DAU BEP') ~ 'DAU BEP',
                         st %>% str_detect('DONG GOI') ~ 'DONG GOI',

                         st %>% str_detect('GV') ~ 'GIAO VIEN',
                         st %>% str_detect('GIAO VIEN') ~ 'GIAO VIEN',
                         st %>% str_detect('GIOO VION') ~ 'GIAO VIEN',
                         st %>% str_detect('HIEU PHO') ~ 'GIAO VIEN',
                         st %>% str_detect('HIEU TRUONG') ~ 'GIAO VIEN',
                         st %>% str_detect('GIANG VIEN') ~ 'GIAO VIEN',

                         st %>% str_detect('GIAM DOC') ~ 'GIAM DOC',
                         st %>% str_detect('GIAM SAT') ~ 'GIAM SAT',
                         st %>% str_detect('KE TOAN') ~ 'KE TOAN',
                         st %>% str_detect('LAI XE') ~ 'LAI XE',
                         st %>% str_detect('TAI XE') ~ 'LAI XE',

                         st %>% str_detect('KY THUAT') ~ 'KY THUAT',
                         st %>% str_detect('KTV') ~ 'THUAT VIEN',
                         st %>% str_detect('KT') ~ 'THUAT VIEN',

                         st %>% str_detect('QUAN LY') ~ 'QUAN LY',
                         st %>% str_detect('QUAN LI') ~ 'QUAN LY',
                         st %>% str_detect('TO TRUONG') ~ 'QUAN LY',
                         st %>% str_detect('TRUONG') ~ 'QUAN LY',
                         st %>% str_detect('QUAN DOC') ~ 'QUAN LY',
                         st %>% str_detect('QL') ~ 'QUAN LY',


                         st %>% str_detect('KY SU') ~ 'KY SU',
                         TRUE ~  as.character(st))
}
