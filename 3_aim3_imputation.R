source('Elie/aim3/Code/1_aim3_data_prep.R')
source('Elie/aim3/Code/2_prep_functions.R')
# read data
aim3 <- read.csv("data/processed/aim3/Aim3_data.csv")
sample_criteria(aim3) # create data with ID's for analysis
criteria <- read.csv("data/processed/aim3/criteria.csv")
recode_aim3(aim3) # recoded data for descriptives
aim3_recode <- read.csv("data/processed/aim3/Aim3_recoded_data.csv")

# variables in imputation
variables <- c("PP_combined", "YR_combined",
               "chdAgeY", "chdRace", "chdGendr","chdRaceH","chrAbuse","PBC_INT","PBC_EXT",
               "CWLNOOH",
               "PBC91A", "PBC18A", "YCD9A", "YYB18A", "YYB91A")

# move
aim3_final <- standardize(aim3)
aim3_impute <- standardize(aim3) %>% 
               impute_format()

# create object for imputation
imp0 <- mice(aim3_impute, m=1)

imp0_mat <- imp0$predictorMatrix

# specify columns and rows not to be used in imputation
dont_impute = c("NSCAWID", "dataset", "chdRace")
for (var in dont_impute) {
  imp0_mat[,var] <- 0
  imp0_mat[var,] <- 0
}


# change to 100 when code is done
overwrite <- F
path <- "Elie/aim3/output/imp.RDS"
if(!file.exists(path) | overwrite == T) {
imp <- suppressMessages(mice(aim3_impute, m=100, predictorMatrix = imp0_mat, seed = 999)) # change once finalized
saveRDS(imp, path)
} else {
 imp <-  readRDS(path)
}

imputed_data <- get_imputed(imp)

imputed_data_long <- make_long(imputed_data)

######## Imputation descriptives ######## 
theme_gtsummary_language("en", big.mark = " ")  # no commas in order to modify post imputation

pre <- two_cat(
  tab_format(aim3_final) %>% 
               mutate(age.binned = case_when(chdAgeY %in% 7:10 ~ "7-10", chdAgeY %in% 11:14 ~ "11-14", 
                                             chdAgeY %in% 15:17 ~ "15-17", TRUE ~ NA_character_),
                      age.binned = factor(age.binned, levels = c("7-10","11-14", "15-17"), ordered = T)
             ) %>%
    mutate_at(vars("ideation","PBC91A","PBC18A","YCD9A","YYB18A","YYB91A"), ~recode(., `1` = "yes", `0`="no")) %>%
  mutate(
    chdRace = recode(chdRace, `3` = "Black"),
    chdGendr = recode(chdGendr, `1` = "male", `2` = "Female"),
    chdRaceH = recode(chdRaceH,
                        `1`  = "Black/Non-Hispanic",
                        `2`  = "White/Non-Hispanic",
                        `3`  = "Hispanic",
                        `4`  = "Other"),
    chrAbuse = recode(chrAbuse,  `1` = "neglect", `2`="physical", `3`="sexual",`4`="other")
  )
    , 
        rows=c("dataset", "wave", "age.binned","chdRace", "chdGendr", "chdRaceH", "chrAbuse", #"PBC_INT", "PBC_EXT",
               "PP_combined","YR_combined",
               "ideation","PBC91A","PBC18A","YCD9A","YYB18A","YYB91A"), 
        col="wave")


post <- two_cat(filter(imputed_data, .imp>0) %>% 
          mutate(
            ideation = case_when(PBC91A   %in% "1"|PBC18A %in% "1"|     
                                   YCD9A  %in% "1"|     
                                   YYB18A %in% "1"| YYB91A %in% "1" ~ "1", TRUE ~"0")) %>%
            mutate_at(vars("ideation","PBC91A","PBC18A","YCD9A","YYB18A","YYB91A"), ~recode(., `1` = "yes", `0`="no")) %>%
            mutate(
              chdRace = recode(chdRace, `3` = "Black"),
              chdGendr = recode(chdGendr, `1` = "male", `2` = "Female"),
              chdRaceH = recode(chdRaceH,
                                `1`  = "Black/Non-Hispanic",
                                `2`  = "White/Non-Hispanic",
                                `3`  = "Hispanic",
                                `4`  = "Other"),
              chrAbuse = recode(chrAbuse,  `1` = "neglect", `2`="physical", `3`="sexual",`4`="other")
            )
            , 
        rows=c("dataset", "wave", "age.binned","chdRace", "chdGendr", "chdRaceH", "chrAbuse", #"PBC_INT", "PBC_EXT",
               "PP_combined","YR_combined",
               "ideation","PBC91A","PBC18A","YCD9A","YYB18A","YYB91A"), 
        col="wave")

# divide by number of imputations
post$table_body <- post$table_body %>%
  mutate_at(vars(stat_1, stat_2, stat_3), 
           ~stringr::str_replace(gsub(" ","", .),"^\\d+",\(x) str_pad(round(as.numeric(x)/100, 0), width = 5, "right")))


pre_post <-
  tbl_merge(
    tbls = list(pre, post),
    tab_spanner = c("**Pre**", "**Post**")
  ) 

########################## missing data #################################################### 
tab_format(aim3_final) %>%  
  gather(key, value, dataset:ideation, -wave) %>% 
  dplyr::group_by(wave, key) %>% 
  summarise(missing = round(sum(is.na(value))/1235*100, 1)) %>% 
  spread(wave, missing) %>%
rename(w1 = `1`, w2 = `2`, w3 = `3`) %>%
  arrange(w1) %>%
  mutate_at(vars(w1,w2,w3), ~paste0(., "%"))

missing_tot <- tab_format(aim3_final) %>% 
  gather(key, value, dataset, PP_combined:YYB91A) %>% 
  summarise(missing = sum(is.na(value)))

rows <- tab_format(aim3_final) %>% 
  gather(key, value, dataset, PP_combined:YYB91A) 

missing_tot$missing/nrow(rows)

var1 <- c("PBC18A_1","PBC91A_1", "YCD9A_1","YYB18A_1","YYB91A_1")
var2 <- c("PBC18A_2","PBC91A_2","YCD9A_2","YYB18A_2","YYB91A_2")
var3 <- c("PBC18A_3","PBC91A_3","YCD9A_3","YYB18A_3","YYB91A_3")

missing_imputed <-
aim3_impute %>%
  mutate(
    all_na1= rowSums(is.na(.[var1]))==5,
    all_na2= rowSums(is.na(.[var2]))==5,
    all_na3= rowSums(is.na(.[var3]))==5,
    info1 = rowSums(is.na(.[var1]))==5 | ((rowSums(.[var1], na.rm=T)==0) & rowSums(is.na(.[var1]))>0),
    info2 = rowSums(is.na(.[var2]))==5 | ((rowSums(.[var2], na.rm=T)==0) & rowSums(is.na(.[var2]))>0),
    info3 = rowSums(is.na(.[var3]))==5 | ((rowSums(.[var3], na.rm=T)==0) & rowSums(is.na(.[var3]))>0),
    missing_any1 = rowSums(is.na(.[var1]))>0,
    missing_any2 = rowSums(is.na(.[var2]))>0,
    missing_any3 = rowSums(is.na(.[var3]))>0,
    nmiss1 = rowSums(is.na(.[var1])), 
    nmiss2 = rowSums(is.na(.[var2])),
    nmiss3 = rowSums(is.na(.[var3]))
  )  %>%
  rowwise() %>%
  mutate(
    all_na = if_else(all_na1 == T & all_na2 == T & all_na2 == T, T, F),
    all_noinfo = info1==T & info2==T & info3==T) %>% 
  dplyr::ungroup()

missing_imputed %>%
  gather(key, value, all_na1:info3, all_noinfo, all_na) %>% 
  dplyr::group_by(key, value) %>%
    count() %>%
  dplyr::group_by(key) %>%
  mutate(percent=paste0(round(n/sum(n)*100, 1), "%")) %>%
  dplyr::ungroup() %>%
  arrange(key) %>%
  filter(value==T) %>%
  mutate(wave=c("all waves", "wave 1", "wave 2", "wave 3", "all waves", "wave 1", "wave 2", "wave 3"),
         key=c("all_na", "all_na", "all_na", "all_na", "info", "info", "info", "info")
         ) %>%
  select(-n) %>%
  spread(wave, percent)


missing_imputed %>% 
  gather(key, value, missing_any1:missing_any3) %>% 
  filter(value==T) %>% 
  spread(key, value) %>% 
  summarise(`all_waves` = (sum(nmiss1)+sum(nmiss2)+sum(nmiss3))/(sum(missing_any1, na.rm=T)+sum(missing_any2, na.rm=T)+sum(missing_any2, na.rm=T)),
            `wave 1` = sum(nmiss1)/sum(missing_any1, na.rm=T), 
            `wave 2` = sum(nmiss2)/sum(missing_any2, na.rm=T), 
            `wave 3` = sum(nmiss3)/sum(missing_any3, na.rm=T))

ids <- missing_imputed %>% 
  gather(key, value, info1:info3) %>% 
  separate(key, c("key", "wave"), sep=4) 

filter(imputed_data, .imp>0) %>%
  mutate(
    any_ideation = rowSums(.[c("PBC18A","PBC91A","YCD9A","YYB18A","YYB91A")]>0),
    one_imputed = any_ideation>0
  ) %>% 
  left_join(select(ids, NSCAWID, key, wave, value), by = c("NSCAWID", "wave")) %>% 
  dplyr::group_by(wave, value) %>%
  count(one_imputed) %>% 
  filter(value==T) %>% # only those who were missing info
  dplyr::group_by(wave) %>%
  mutate(prop=n/sum(n)*100) %>% filter(one_imputed==T)
########################## ########################## ########################## 
gt::gtsave(as_gt(pre_post), "Elie/aim3/output/descriptives.docx")
write.csv(imputed_data_long, "Elie/aim3/output/long_data_BA.csv")
