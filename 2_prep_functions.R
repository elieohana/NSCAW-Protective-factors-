# function to standardize variables and combine protective factors into a single column across ages
standardize <- function(data) {
  # standardize variables
#  aim3_final <- 
  # aim3 input
  filter(data, NSCAWID %in% criteria$NSCAWID) %>%
  dplyr::select(-contains("PHO"), -contains("YRC"), -contains("RAW"), -contains("PPS")) %>% 
  dplyr::mutate(wave=case_when(dataset=="NSCAW-I" & wave==3~2,
                        dataset=="NSCAW-I" & wave==4~3, TRUE ~ wave)) %>%
    # convert negative values to NA
    dplyr::mutate_at(vars(chdAgeY,chdRace, CWLNOOH), 
              ~ case_when(. %in% c(-4, -2, -1) ~ NA_real_, TRUE ~ .)) %>%
    dplyr::mutate(chrAbuse = if_else(chrAbuse %in% c("-1","-4"), NA_character_, chrAbuse)) %>%
    # fix values
    dplyr::mutate(chdRace = 3) %>%
    # fill values
    dplyr::group_by(NSCAWID) %>%
    fill(c("chdRaceH", "chdGendr", "chrAbuse"), .direction = "down") %>%
    dplyr::ungroup() %>%
  gather(key, value, YRP_TOT, YRR_TOT, 
         PPT_SCR,PPU_SCR) %>%
  ## combined observations 
  # first standardize
  dplyr::mutate(key2 = key, value2 = value) %>%
  dplyr::mutate(
    key2 = paste0(key2, "_z"),
    # convert negative values before standardizing
    value2 = if_else(value2>-1, value2, NA_real_)) %>% 
  dplyr::group_by(key2) %>%
  dplyr::mutate(value2 = as.numeric(scale(value2))) %>% # standardize
  #  recode to indicate who should have seen items
  dplyr::mutate(
    value = case_when(
      key=="YRP_TOT" & (YCH9A %in% c(-7,-6,-1,2,3) | !chdAgeY %in% 5:7) ~ -888, 
      key=="YRR_TOT" & (YCH9A %in% c(-7,-6,-1,2,3) | chdAgeY < 8) ~ -888, 
      key == 'PPT_SCR' & !chdAgeY %in% 6:10 ~ -888, 
      key == 'PPU_SCR' & !chdAgeY >= 11 ~ -888, 
      TRUE ~ value
    )) %>%
  # replace NA with not applicable when relevant
  dplyr::mutate(value2 = if_else(value<0, value, value2)) %>% 
  pivot_wider(names_from = c("key", "key2"), values_from = c("value", "value2")) %>%
  dplyr::rename_with(~str_replace(., "_z", ""), starts_with("value_")) %>% 
  dplyr::rename_with(~str_after_nth(., "_", 3),  starts_with('value_')) %>% 
  dplyr::rename_with(~str_after_nth(., "_", 3),  starts_with('value2_')) %>%
  dplyr::mutate(PP_combined = case_when(!PPT_SCR_z == -888 ~ PPT_SCR_z, 
                                 PPT_SCR_z  == -888  & !PPU_SCR_z == -888 ~ PPU_SCR_z, TRUE ~ NA_real_),
         YR_combined = case_when(!YRR_TOT_z == -888 ~ YRR_TOT_z,
                                 YRR_TOT_z == -888  & !YRP_TOT_z == -888 ~ YRP_TOT_z, TRUE ~ NA_real_)
  ) 
}

# format data for imputation
impute_format <- function(data) {
  #aim3_impute <- 
  #  aim3_final %>%
  data = data %>%
    dplyr::select(NSCAWID, dataset, wave, all_of(variables)) %>%
    dplyr::mutate(chrAbuse = as.character(chrAbuse),
           chrAbuse = if_else(chrAbuse %in% c("-4", "-2", "-1"), NA, chrAbuse),
           chrAbuse = recode(chrAbuse, `neglect`=1, `physical`=2, `sexual`=3, `other`=4),
          #  mean_age = mean(chdAgeY, na.rm=T),
          #  age.centered=chdAgeY-mean_age,
           age.binned = cut(chdAgeY, breaks = c(7, 10, 14, 17), labels = c("7-10", "11-14", "15-17"), include.lowest = T),
           dataset=recode(dataset,`NSCAW-I`="NSCAW1", `NSCAW-II`="NSCAW2")
           ) %>%
    # remove ages above 17
    filter(chdAgeY<=17)

  data = data %>%
    # remove age - no longer needed
    dplyr::select(-chdAgeY) %>%
    dplyr::mutate_at(vars(YCD9A, PBC18A, PBC91A, YYB18A, YYB91A), 
              ~case_when(.==1 ~ 0, . %in% 2:3 ~ 1, TRUE ~ NA_real_)) %>%
    dplyr::mutate_if(is.double, ~as.numeric(.)) %>%
    dplyr::mutate_at(vars(chrAbuse, CWLNOOH, PBC91A, PBC18A, YCD9A, YYB18A, YYB91A),  
              ~if_else(.<0,NA, .)) %>%
    dplyr::group_by(NSCAWID) %>%
    fill(c("PP_combined","YR_combined","chdGendr","chdRaceH","chrAbuse","PBC_INT","PBC_EXT", "CWLNOOH"), .direction = "down") %>%
    dplyr::ungroup()

  age_levs = levels(data$age.binned)
  waves = unique(data$wave)
  data = data %>%
    gather(key, value, age.binned, PBC18A, PBC91A, YYB18A, YYB91A, YCD9A) %>%
    unite("key_wave", key, wave, remove = T) %>%
    spread(key_wave, value)

  age_vars = paste0("age.binned_", waves)
  # specify classes
  data = data %>%
    dplyr::mutate_at(vars(age_vars), ~factor(., levels = age_levs, ordered = T)) %>% 
    dplyr::mutate_at(vars(chdRace, chdGendr, chdRaceH, chrAbuse), ~as.factor(.)) %>% 
    dplyr::mutate_at(vars(PBC18A_1:YYB91A_3), ~as.numeric(.)) # check with Gabe
}

get_imputed <- function(imp) {
age_levels = levels(imp$data$age.binned_1)
imputed_data <- complete(imp, action="long", include = T) %>%
  gather(key, value, contains("_1"), contains("_2"), contains("_3")) %>%
  separate(key, into = c("variable", "wave"), sep = "_") %>%
  spread(variable, value)
imputed_data$age.binned = factor(imputed_data$age.binned, levels = age_levels) # unordered for model
imputed_data
}

# convert imputed data to long format + perform recodes for model fitting 
make_long <- function(imputed_data) {

  ideation_cols = c("PBC91A", "PBC18A", "YCD9A", "YYB18A", "YYB91A")
  imputed_data$hold <- 0
  any_na = apply(imputed_data[, ideation_cols], 1, function(i) any(is.na(i)))
  imputed_data$hold[any_na] = NA

  any_one = apply(imputed_data[, ideation_cols], 1, function(i) any(i == 1))
  imputed_data$hold[any_one] = 1
  # imputed_data$hold = sapply(rowSums(imputed_data[, ideation_cols]), function(i) min(i, 1))
#imputed_data_long <- 
  imputed_data <- imputed_data %>%
  # code OR rule
  dplyr::mutate(
    # hold = case_when(PBC91A %in% 1|PBC18A %in% 1|     
    #                    YCD9A  %in% 1|     
    #                    YYB18A %in% 1| YYB91A %in% 1 ~ 1, TRUE ~0),
    ideation = "ideation"
  ) %>%
  unite(ideation, wave, col="ideation_wave", remove = F) %>%
  arrange(NSCAWID, .imp) %>%
  spread(ideation_wave, hold) %>%
  dplyr::group_by(.imp, NSCAWID) %>%
  fill(ideation_1, .direction = "updown") %>%
  fill(ideation_2, .direction = "updown") %>%
  fill(ideation_3, .direction = "updown") %>%
  dplyr::mutate(
    pre = if_else(wave==1, ideation_1, ideation_2),
    post= if_else(wave==1, ideation_2, ideation_3)
  ) %>%
    dplyr::ungroup() %>%
  # model coding
  dplyr::mutate(
    chdGendr = recode_factor(chdGendr,  "1" = "male", "2" = "Female"),
    chdRaceH = recode_factor(chdRaceH,  "1" = "Black/Non-Hispanic", "3" = "Hispanic", "4" = "other"),
    chrAbuse = recode_factor(chrAbuse,  "1" = "neglect", "2"="physical", "3"="sexual","4"="other"),
    chdGendr = relevel(chdGendr, ref="male"),
    chrAbuse = relevel(chrAbuse, ref="sexual"),
    chdRaceH = relevel(chdRaceH, ref="Black/Non-Hispanic")
  ) %>%
  rowwise() %>%
  dplyr::mutate(.id=paste0(.id, wave)) %>% # dedup
  filter(wave %in% 1:2) %>% 
    dplyr::ungroup()
}

# format for table

tab_format <- function(data) {
#  aim3_final %>%
  data %>%
    dplyr::select(NSCAWID, dataset, wave, all_of(variables)) %>%
    # move this step
    dplyr::mutate(chrAbuse = as.character(chrAbuse),
           chrAbuse = if_else(chrAbuse %in% c("-4", "-2", "-1"), NA, chrAbuse)) %>%
    dplyr::mutate(chrAbuse = recode(chrAbuse, `neglect`=1, `physical`=2, `sexual`=3, `other`=4)) %>%
    dplyr::mutate_if(is.double, ~as.numeric(.)) %>%
    dplyr::mutate_at(vars(chrAbuse, CWLNOOH, PBC91A, PBC18A, YCD9A, YYB18A, YYB91A),  
              ~if_else(.<0,NA, .)) %>%
    dplyr::mutate_at(vars(YCD9A, PBC18A, PBC91A, YYB18A, YYB91A), 
              ~case_when(.==1 ~ 0, . %in% 2:3 ~ 1, TRUE ~ NA_real_)) %>%
    dplyr::mutate(dataset=recode(dataset,`NSCAW-I`="NSCAW1", `NSCAW-II`="NSCAW2")) %>% 
    dplyr::mutate(
      ideation = case_when(PBC91A %in% 1|PBC18A %in% 1|     
                             YCD9A  %in% 1|     
                             YYB18A %in% 1| YYB91A %in% 1 ~ 1, TRUE ~0))
}
## make descriptive table ##
two_cat <- function(data, rows, col,text) {
  dplyr::select(data, all_of(rows), all_of(col)) %>%
    tbl_summary(by = col,
                missing="ifany",
                include=c(rows,col),
                percent="col",
                statistic = list(all_categorical() ~ "{n} ({p}%)",
                                 all_continuous() ~ "{mean} ({sd})"
                                 ),
                digits = list(everything() ~ c(0,1)),
                missing_text = "Missing",
                type = all_dichotomous() ~ "categorical",
                label = list(
                  age.binned = "Age",
                  chdRace    = "Race",
                  chdGendr   = "Sex",
                  chdRaceH   = "Hispanic",
                  chrAbuse   = "Abuse",
                  PBC_INT    = "Internalizing",
                  PBC_EXT    = "Externalizing",
                  PP_combined= "Parent/teacher reported Social Skills",
                  YY_conbined= "Child reported total Loneliness and Social Dissatisfaction",
                  ideation   = "ideation",
                  PBC91A     = "CBCL deliberately harms self or attempts suicide",
                  PBC18A     = "CBCL talks about killing self",
                  YCD9A      = "CDI ideation",
                  YYB18A     = "YSR deliberately harms self or attempts suicide",
                  YYB91A     = "YSR talks about killing self"
                )
    ) 

}


