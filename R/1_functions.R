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
                         st %>% str_detect('MAY') ~ 'CONG NHAN',
                         st %>% str_detect('DONG GOI') ~ 'CONG NHAN',

                         st %>% str_detect('NHAN VIEN') ~ 'NHAN VIEN',
                         st %>% str_detect('NV') ~ 'NHAN VIEN',
                         st %>% str_detect('N.V') ~ 'NHAN VIEN',
                         st %>% str_detect('BAN HANG') ~ 'NHAN VIEN',
                         st %>% str_detect('GDV') ~ 'NHAN VIEN',
                         st %>% str_detect('GIAO DICH VIEN') ~ 'NHAN VIEN',
                         st %>% str_detect('CHUYEN VIEN') ~ 'NHAN VIEN',

                         st %>% str_detect('TRO LY') ~ 'TRO LY - THU KY',
                         st %>% str_detect('THU KY') ~ 'TRO LY - THU KY',
                         st %>% str_detect('KE TOAN') ~ 'TRO LY - THU KY',
                         st %>% str_detect('THU KHO') ~ 'TRO LY - THU KY',
                         st %>% str_detect('TAP VU') ~ 'TRO LY - THU KY',
                         st %>% str_detect('CAP DUONG') ~ 'TRO LY - THU KY',
                         st %>% str_detect('THU QUY') ~ 'TRO LY - THU KY',
                         st %>% str_detect('THU QUI') ~ 'TRO LY - THU KY',
                         st %>% str_detect('THU NGAN') ~ 'TRO LY - THU KY',

                         st %>% str_detect('BAO VE') ~ 'BAO VE',
                         st %>% str_detect('B. VE') ~ 'BAO VE',
                         st %>% str_detect('B.VE') ~ 'BAO VE',
                         st %>% str_detect('BVE') ~ 'BAO VE',

                         st %>% str_detect('BAC SI') ~ 'BAC SY - DUOC SY',
                         st %>% str_detect('BAC SY') ~ 'BAC SY - DUOC SY',
                         st %>% str_detect('BS.') ~ 'BAC SY - DUOC SY',
                         st %>% str_detect('DUOC SY') ~ 'BAC SY - DUOC SY',
                         st %>% str_detect('DUOC SI') ~ 'BAC SY - DUOC SY',
                         st %>% str_detect('DUOC TA') ~ 'BAC SY - DUOC SY',
                         st %>% str_detect('Y SY') ~ 'BAC SY - DUOC SY',
                         st %>% str_detect('Y SI') ~ 'BAC SY - DUOC SY',
                         st %>% str_detect('TRiNH DUOC VIEN') ~ 'BAC SY - DUOC SY',
                         st %>% str_detect('DIEU DUONG') ~ 'DIEU DUONG',
                         st %>% str_detect('HO SINH') ~ 'DIEU DUONG',
                         st %>% str_detect('HO LY') ~ 'DIEU DUONG',
                         st %>% str_detect('Y TA') ~ 'DIEU DUONG',
                         st %>% str_detect('Y TE') ~ 'DIEU DUONG',
                         st %>% str_detect('TRI LIEU') ~ 'DIEU DUONG',
                         st %>% str_detect('TRAM PHO') ~ 'DIEU DUONG',

                         st %>% str_detect('BI THU') ~ 'CAN BO NHA NUOC',
                         st %>% str_detect('BI TU XA DOAN') ~ 'CAN BO NHA NUOC',
                         st %>% str_detect('CAN BO') ~ 'CAN BO NHA NUOC',
                         st %>% str_detect('CB') ~ 'CAN BO NHA NUOC',
                         st %>% str_detect('CHU TICH') ~ 'CAN BO NHA NUOC',
                         st %>% str_detect('DIA CHINH') ~ 'CAN BO NHA NUOC',
                         st %>% str_detect('TU PHAP') ~ 'CAN BO NHA NUOC',
                         st %>% str_detect('VAN PHoNG') ~ 'CAN BO NHA NUOC',
                         st %>% str_detect('VIEN CHUC') ~ 'CAN BO NHA NUOC',
                         st %>% str_detect('VAN HOA') ~ 'CAN BO NHA NUOC',
                         st %>% str_detect('THANH TRA') ~ 'CAN BO NHA NUOC',
                         st %>% str_detect('TO CHUC') ~ 'CAN BO NHA NUOC',


                         st %>% str_detect('BAN HANG') ~ 'KINH DOANH',
                         st %>% str_detect('CUA HANG PHO') ~ 'KINH DOANH',
                         st %>% str_detect('KINH DOANH') ~ 'KINH DOANH',

                         st %>% str_detect('DIEN VIEN') ~ 'DIEN VIEN',

                         st %>% str_detect('BEP') ~ 'NAU AN',

                         st %>% str_detect('GV') ~ 'GIAO VIEN',
                         st %>% str_detect('GIAO VIEN') ~ 'GIAO VIEN',
                         st %>% str_detect('GIOO VION') ~ 'GIAO VIEN',
                         st %>% str_detect('HIEU PHO') ~ 'GIAO VIEN',
                         st %>% str_detect('HIEU TRUONG') ~ 'GIAO VIEN',
                         st %>% str_detect('GIANG VIEN') ~ 'GIAO VIEN',


                         st %>% str_detect('LAI XE') ~ 'LAI XE',
                         st %>% str_detect('TAI XE') ~ 'LAI XE',
                         st %>% str_detect('LAI MAY') ~ 'LAI XE',
                         st %>% str_detect('LAI CAU') ~ 'LAI XE',

                         st %>% str_detect('KY THUAT') ~ 'KY THUAT',
                         st %>% str_detect('KTV') ~ 'KY THUAT',
                         st %>% str_detect('KT') ~ 'KY THUAT',
                         st %>% str_detect('KI THUAT') ~ 'KY THUAT',
                         st %>% str_detect('KIEN TRUC SU') ~ 'KY THUAT',
                         st %>% str_detect('LAP TRiNH VIEN') ~ 'KY THUAT',
                         st %>% str_detect('KY SU') ~ 'KY THUAT',
                         st %>% str_detect('THIET KE') ~ 'KY THUAT',

                         st %>% str_detect('QUAN LY') ~ 'QUAN LY',
                         st %>% str_detect('QUAN LI') ~ 'QUAN LY',
                         st %>% str_detect('TO TRUONG') ~ 'QUAN LY',
                         st %>% str_detect('TO PHO') ~ 'QUAN LY',
                         st %>% str_detect('TRUONG') ~ 'QUAN LY',
                         st %>% str_detect('QUAN DOC') ~ 'QUAN LY',
                         st %>% str_detect('QL') ~ 'QUAN LY',
                         st %>% str_detect('GIAM DOC') ~ 'QUAN LY',
                         st %>% str_detect('GIAM SAT') ~ 'QUAN LY',
                         st %>% str_detect('KIEM SOAT VIEN') ~ 'QUAN LY',
                         st %>% str_detect('PHO PHoNG') ~ 'QUAN LY',
                         st %>% str_detect('CHI HUY') ~ 'QUAN LY',
                         st %>% str_detect('DOI PHO') ~ 'QUAN LY',

                         TRUE ~  NA_character_)


}
