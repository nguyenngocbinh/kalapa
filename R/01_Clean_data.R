
#' @title clean maCv
f_recode_macv <- function(st){
  library(stringr)
  st <- st %>%
    stringi::stri_trans_general(id = "latin-ascii" ) %>%
    str_replace_all('\\?', replacement = '') %>%
    stringr::str_squish() %>%
    toupper()

  clean_st <-  case_when(st %>% str_detect('CONG NHAN') ~ 'CONG NHAN',
                         st %>% str_detect('C.N') ~ 'CONG NHAN',
                         st %>% str_detect('C.NHAN') ~ 'CONG NHAN',
                         st %>% str_detect('CON GNHAN') ~ 'CONG NHAN',
                         st %>% str_detect('CN') ~ 'CONG NHAN',
                         st %>% str_detect('COONG NHAON') ~ 'CONG NHAN',
                         st %>% str_detect('THO') ~ 'CONG NHAN',
                         st %>% str_detect('MAY') ~ 'CONG NHAN',
                         st %>% str_detect('DONG GOI') ~ 'CONG NHAN',

                         st %>% str_detect('LAO DONG') ~ 'LDPT',
                         st %>% str_detect('LDPT') ~ 'LDPT',
                         st %>% str_detect('TAP VU') ~ 'LDPT',

                         st %>% str_detect('TIEP VIEN') ~ 'TIEP VIEN',
                         st %>% str_detect('TONG DAI') ~ 'TONG DAI',

                         st %>% str_detect('NHAN VIEN') ~ 'NHAN VIEN',
                         st %>% str_detect('NV') ~ 'NHAN VIEN',
                         st %>% str_detect('N.V') ~ 'NHAN VIEN',
                         st %>% str_detect('GDV') ~ 'GDV',
                         st %>% str_detect('GIAO DICH VIEN') ~ 'GDV',
                         st %>% str_detect('CHUYEN VIEN') ~ 'CHUYEN VIEN',

                         st %>% str_detect('TRO LY') ~ 'TRO LY',
                         st %>% str_detect('THU KY') ~ 'THU KY',
                         st %>% str_detect('KE TOAN') ~ 'KE TOAN',
                         st %>% str_detect('THU KHO') ~ 'THU KHO',
                         st %>% str_detect('TAP VU') ~ 'TAP VU',
                         st %>% str_detect('CAP DUONG') ~ 'CAP DUONG',
                         st %>% str_detect('THU QUY') ~ 'THU QUY',
                         st %>% str_detect('THU QUI') ~ 'THU QUY',
                         st %>% str_detect('THU NGAN') ~ 'THU NGAN',

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
                         st %>% str_detect('TRiNH DUOC VIEN') ~ 'DUOC SY',
                         st %>% str_detect('Y SY') ~ 'Y SY',
                         st %>% str_detect('Y SI') ~ 'Y SY',
                         st %>% str_detect('DIEU DUONG') ~ 'DIEU DUONG',
                         st %>% str_detect('HO SINH') ~ 'DIEU DUONG',
                         st %>% str_detect('HO LY') ~ 'DIEU DUONG',
                         st %>% str_detect('Y TA') ~ 'DIEU DUONG',
                         st %>% str_detect('TRI LIEU') ~ 'DIEU DUONG',

                         st %>% str_detect('Y TE') ~ 'TRAM Y TE',
                         st %>% str_detect('TRAM PHO') ~ 'TRAM Y TE',
                         st %>% str_detect('TRAM TRUONG') ~ 'TRAM Y TE',
                         st %>% str_detect('TRUONG TRAM') ~ 'TRAM Y TE',

                         st %>% str_detect('BI THU') ~ 'LANH DAO XA',
                         st %>% str_detect('BI TU XA DOAN') ~ 'LANH DAO XA',
                         st %>% str_detect('CHU TICH') ~ 'LANH DAO XA',
                         st %>% str_detect('CAN BO') ~ 'CAN BO NHA NUOC',
                         st %>% str_detect('CB') ~ 'CAN BO NHA NUOC',
                         st %>% str_detect('DIA CHINH') ~ 'CAN BO NHA NUOC',
                         st %>% str_detect('TU PHAP') ~ 'CAN BO NHA NUOC',
                         st %>% str_detect('VAN PHoNG') ~ 'CAN BO NHA NUOC',
                         st %>% str_detect('VIEN CHUC') ~ 'CAN BO NHA NUOC',
                         st %>% str_detect('VAN HOA') ~ 'CAN BO NHA NUOC',
                         st %>% str_detect('THANH TRA') ~ 'CAN BO NHA NUOC',
                         st %>% str_detect('TO CHUC') ~ 'CAN BO NHA NUOC',

                         st %>% str_detect('DIEU HANH') ~ 'DIEU HANH',
                         st %>% str_detect('OPERATOR') ~ 'DIEU HANH',
                         st %>% str_detect('TRUONG CA') ~ 'TRUONG CA',
                         st %>% str_detect('TRUONG PHONG') ~ 'TRUONG PHONG',
                         st %>% str_detect('TRUONG BO PHAN') ~ 'TRUONG PHONG',


                         st %>% str_detect('BAN HANG') ~ 'KINH DOANH',
                         st %>% str_detect('CUA HANG PHO') ~ 'KINH DOANH',
                         st %>% str_detect('CUA HANG TRUONG') ~ 'KINH DOANH',
                         st %>% str_detect('KINH DOANH') ~ 'KINH DOANH',
                         st %>% str_detect('SALE') ~ 'KINH DOANH',

                         st %>% str_detect('DIEN VIEN') ~ 'DIEN VIEN',

                         st %>% str_detect('BEP') ~ 'NAU AN',
                         st %>% str_detect('NAU AN') ~ 'NAU AN',

                         st %>% str_detect('GV') ~ 'GIAO VIEN',
                         st %>% str_detect('GIAO VIEN') ~ 'GIAO VIEN',
                         st %>% str_detect('GIOO VION') ~ 'GIAO VIEN',
                         st %>% str_detect('GIANG VIEN') ~ 'GIAO VIEN',
                         st %>% str_detect('HIEU PHO') ~ 'HIEU PHO',
                         st %>% str_detect('HIEU TRUONG') ~ 'HIEU TRUONG',

                         st %>% str_detect('LAI XE') ~ 'LAI XE',
                         st %>% str_detect('TAI XE') ~ 'LAI XE',
                         st %>% str_detect('LAI MAY') ~ 'LAI MAY',
                         st %>% str_detect('LAI CAU') ~ 'LAI MAY',
                         st %>% str_detect('VAN HANH') ~ 'LAI MAY',

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
                         st %>% str_detect('PHO PHoNG') ~ 'QUAN LY',
                         st %>% str_detect('CHI HUY') ~ 'QUAN LY',
                         st %>% str_detect('DOI PHO') ~ 'QUAN LY',
                         st %>% str_detect('QL') ~ 'QUAN LY',
                         st %>% str_detect('TO TRUONG') ~ 'QUAN LY',
                         st %>% str_detect('TO PHO') ~ 'QUAN LY',
                         st %>% str_detect('QUAN DOC') ~ 'GIAM DOC',
                         st %>% str_detect('GIAM DOC') ~ 'GIAM DOC',
                         st %>% str_detect('GIAM SAT') ~ 'GIAM SAT',
                         st %>% str_detect('KIEM SOAT') ~ 'KIEM SOAT',
                         st %>% str_detect("[:digit:]") ~ NA_character_,
                         TRUE ~ as.character(st)

  )
}

#' @title impute numeric variables
f_impute_numeric <- function(feature, seed = 1911) {
  outlen <- sum(is.na(feature))
  non_na_feature <- na.omit(feature)
  set.seed(seed)
  filldata <- sample(non_na_feature, outlen, replace = TRUE)
  feature[is.na(feature)] <- filldata
  feature
}

#' @title clean input data
f_clean_data <- function(df, logical_vars, character_vars){
  df <-  df %>%
    mutate_at(c(logical_vars, character_vars), toupper) %>%
    mutate_at(c(logical_vars, character_vars), na_if, "NONE") %>%
    mutate(FIELD_11 = as.numeric(FIELD_11)) %>%
    mutate(FIELD_12 = case_when(FIELD_12 == "1" ~ TRUE, FIELD_12 == "0" ~ FALSE, TRUE ~ NA )) %>%
    mutate(FIELD_13 = case_when(FIELD_13 %in% c("0", "4", "8", "12") ~ NA_character_,
                                TRUE ~ as.character(FIELD_13))) %>%
    mutate(FIELD_39 = na_if(FIELD_39, "1")) %>%
    mutate(FIELD_40 = case_when(FIELD_40  %in% c("02 05 08 11", "05 08 11 02", "08 02") ~ NA_character_,
                                TRUE ~ as.character(FIELD_40)),
           FIELD_40 = as.numeric(FIELD_40)) %>%
    mutate(FIELD_43 = FIELD_43 %>% na_if("0") %>% na_if("5")) %>%
    mutate(FIELD_45 = as.numeric(FIELD_45)) %>%
    mutate(FIELD_9 = case_when(FIELD_9 == "NA" ~ "MISSING",
                               FIELD_9  %in% c("75", "79", "80", "86") ~ "RANDOM MISSING",
                               TRUE ~ as.character(FIELD_9))) %>%
    mutate(maCv =  f_recode_macv(maCv)) %>%
    # working with numeric variables
    mutate(AGE = if_else(age_source1 == age_source2, age_source1, NA_real_)) %>%
    mutate(AGE = if_else(AGE >= 18, AGE, NA_real_)) %>%
    select(-age_source1, - age_source2, - FIELD_7, - district) %>%
    # convert ti character
    mutate_if(is.factor, as.character) %>%
    mutate_if(is.logical, as.character)

  df_non_na <- df %>%
    filter(FIELD_9 != "MISSING" | is.na(FIELD_9)) %>%
    mutate_if(is.character, replace_na, "RANDOM MISSING")

  df_na <- df %>%
    filter(FIELD_9 == "MISSING") %>%
    mutate_if(is.character, replace_na, "MISSING")

  cleaned_df <- df_non_na %>%
    bind_rows(df_na) %>%
    mutate_if(is.character, as.factor) %>%
    # mutate_at(new_numeric_vars, fimpute_numeric) %>% # neu dung mutate if thi bien id va label bi loi
    # mutate_at(imputer_numeric_vars, fimpute_numeric) %>%
    arrange(id) %>% # Note
    as.data.table()

  return(cleaned_df)
}



#=============================================================================
# Task
#=============================================================================

clean_plan = drake_plan(
  col_types = cols(id = col_double(), province = col_character(), district = col_character(), age_source1 = col_double(),
                   age_source2 = col_double(), maCv = col_character(), FIELD_1 = col_double(), FIELD_2 = col_double(), FIELD_3 = col_double(),
                   FIELD_4 = col_double(), FIELD_5 = col_double(), FIELD_6 = col_double(), FIELD_7 = col_character(), FIELD_8 = col_character(),
                   FIELD_9 = col_character(), FIELD_10 = col_character(), FIELD_11 = col_character(), FIELD_12 = col_character(),
                   FIELD_13 = col_character(), FIELD_14 = col_double(), FIELD_15 = col_double(), FIELD_16 = col_double(), FIELD_17 = col_character(),
                   FIELD_18 = col_logical(), FIELD_19 = col_logical(), FIELD_20 = col_logical(), FIELD_21 = col_double(), FIELD_22 = col_double(),
                   FIELD_23 = col_logical(), FIELD_24 = col_character(), FIELD_25 = col_logical(), FIELD_26 = col_logical(), FIELD_27 = col_logical(),
                   FIELD_28 = col_logical(), FIELD_29 = col_character(), FIELD_30 = col_character(), FIELD_31 = col_character(),
                   FIELD_32 = col_double(), FIELD_33 = col_double(), FIELD_34 = col_double(), FIELD_35 = col_character(), FIELD_36 = col_character(),
                   FIELD_37 = col_character(), FIELD_38 = col_logical(), FIELD_39 = col_character(), FIELD_40 = col_character(),
                   FIELD_41 = col_character(), FIELD_42 = col_character(), FIELD_43 = col_character(), FIELD_44 = col_character(),
                   FIELD_45 = col_character(), FIELD_46 = col_double(), FIELD_47 = col_logical(), FIELD_48 = col_logical(), FIELD_49 = col_logical(),
                   FIELD_50 = col_double(), FIELD_51 = col_double(), FIELD_52 = col_double(), FIELD_53 = col_double(), FIELD_54 = col_double(),
                   FIELD_55 = col_double(), FIELD_56 = col_double(), FIELD_57 = col_double()),
  train = readr::read_csv("data/train.csv", col_types = col_types),
  test = readr::read_csv("data/test.csv", col_types = col_types),
  dset = test %>% mutate(label = NA) %>% bind_rows(train),
  numeric_vars = c("age_source1", "age_source2",
                   "FIELD_1", "FIELD_2", "FIELD_3", "FIELD_4", "FIELD_5", "FIELD_6",
                   "FIELD_14", "FIELD_15", "FIELD_16",
                   "FIELD_21", "FIELD_22",
                   "FIELD_32", "FIELD_33", "FIELD_34",
                   "FIELD_46",
                   "FIELD_50", "FIELD_51", "FIELD_52", "FIELD_53", "FIELD_54", "FIELD_55", "FIELD_56", "FIELD_57"),

  logical_vars = c("FIELD_18", "FIELD_19", "FIELD_20",
                   "FIELD_23",
                   "FIELD_25", "FIELD_26", "FIELD_27", "FIELD_28", "FIELD_29", "FIELD_30", "FIELD_31",
                   "FIELD_36", "FIELD_37", "FIELD_38",
                   "FIELD_47", "FIELD_48", "FIELD_49"),


  character_vars = c("province", "district", "maCv",
                     "FIELD_7", "FIELD_8", "FIELD_9", "FIELD_10", "FIELD_11", "FIELD_12", "FIELD_13",
                     "FIELD_17",
                     "FIELD_24",
                     "FIELD_35",
                     "FIELD_39", "FIELD_40", "FIELD_41", "FIELD_42", "FIELD_43", "FIELD_44", "FIELD_45"),

  new_character_vars = character_vars %>% setdiff(c("FIELD_11", "FIELD_12", "FIELD_40", "FIELD_45")),
  new_numeric_vars = c(numeric_vars, "FIELD_11", "FIELD_40", "FIELD_45") %>% setdiff(c("age_source1", "age_source2")),
  new_logical_vars = c(logical_vars, "FIELD_12"),

  # Use clean function
  cleaned_dt = f_clean_data(dset, logical_vars, character_vars),

  # Bin data (note skip id)
  bins = scorecard::woebin(cleaned_dt,
                           y = "label",
                           var_skip = c("id"),
                           bin_num_limit = 8,
                           check_cate_num = FALSE),

  # Bin with numeric only
  bins_numeric = scorecard::woebin(cleaned_dt,
                                   y = "label",
                                   x = new_numeric_vars,
                                   var_skip = c("id"),
                                   bin_num_limit = 8,
                                   check_cate_num = FALSE),
  # Bin with factor only
  bins_factor = scorecard::woebin(cleaned_dt,
                                   y = "label",
                                   x = setdiff(c(new_character_vars, new_logical_vars),
                                               c("district", "FIELD_7")),
                                   var_skip = c("id"),
                                   bin_num_limit = 8,
                                   check_cate_num = FALSE),

  # Create data for regr task
  dt_woe_regr = target({
    dt_woe = scorecard::woebin_ply(cleaned_dt, bins = bins, to = "woe")
    return(dt_woe)
    }),

  # Create data for classif task
  dt_woe_classif = target({
    dt_woe = scorecard::woebin_ply(cleaned_dt,
                                   bins = bins_factor, # note: using bins_factor
                                   to = "woe")
    dt_woe$label = as.factor(if_else(dt_woe$label == 1, "bad", "good", missing = NULL))
    return(dt_woe)
  }),

  # Create data with bining only
  dt_bin = scorecard::woebin_ply(cleaned_dt, bins = bins, to = "bin"),

  # Create data with numeric binning and convert to woe
  dt_woe_cat = scorecard::woebin_ply(cleaned_dt, bins = bins_numeric, to = "woe"),

  # Create data with numeric binning and no convert to woe
  dt_bin_cat = scorecard::woebin_ply(cleaned_dt, bins = bins_numeric, to = "bin")
)
