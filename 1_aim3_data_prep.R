# packages 
library(tidyverse)
library(haven)
library(gtsummary)
library(twang)
library(sjmisc)
library(readxl)
library(corrplot)
library(VIM)
library(mice)
library(strex)
library(survey)
library(srvyr)
library(sjPlot)
library(gtsummary)
library(psych)
library(gt)
library(kableExtra)
library(lme4)
library(broom.mixed)
library(modelsummary)
library(ggplot2)
library(emmeans)

# Should datasets be merged?
path1 <- "data/processed/aim3/Aim3_data.csv"
path2 <- "data/processed/aim3/criteria.csv"
path3 <- "data/processed/aim3/Aim3_recoded_data.csv"

overwrite=F
if(!file.exists(path1) | !file.exists(path2) | !file.exists(path3) | overwrite==T) {
cps1_w1 <- read_sav("data/nscaw_1/cps.sav") %>%
  mutate(dataset="NSCAW-I", wave=1)
cps1_w2 <- read_sav("data/nscaw_1/cps_w2.sav") %>%
  mutate(dataset="NSCAW-I", wave=2)
cps1_w3 <- read_sav("data/nscaw_1/cps_w3.sav") %>%
  mutate(dataset="NSCAW-I", wave=3)
cps1_w4 <- read_sav("data/nscaw_1/cps_w4.sav") %>%
  mutate(dataset="NSCAW-I", wave=4)
cps1_w5 <- read_sav("data/nscaw_1/cps_w5.sav") %>%
  mutate(dataset="NSCAW-I", wave=5)
cps_acr <- read_sav("data/nscaw_1/cps_acr.sav")
# CPS 2
cps2_w1 <- read_sav("data/nscaw_2/cps_n2.SAV") %>%
  mutate(dataset="NSCAW-II", wave=1)
cps2_w2 <- read_sav("data/nscaw_2/cps_n2_w2.SAV") %>%
  mutate(dataset="NSCAW-II", wave=2)
cps2_w3 <- read_sav("data/nscaw_2/cps_n2_w3.SAV") %>%
  mutate(dataset="NSCAW-II", wave=3)
cps_acr2 <- read_sav("data/nscaw_2/cps_acr_wv.SAV")

################################ Merge datasets ################################
aim3 <- 
  # NSCAW 1
  cps1_w1 %>% 
  dplyr::select(NSCAWID, dataset, wave, chdAgeY, chdRace, 
                chdGendr,chdRaceH,chrAbuse,PBC_INT,PBC_EXT,NANALWT,
                PBC91A, PBC18A, YCD9A, YYB18A, YYB91A, NTS_E18, NTS_E91, # outcomes
                YRP_TOT,YRR_TOT,PHO_CS3, PHO_ES3,PHO_HM3,YRC_ESA,YRC_ESB,YRC_INA,YRC_INB,YRC_ATA,YRC_ATB,YRC_STA,YRC_STB,
                PPS_RAW,PPS_SCR,PPT_RAW,PPT_SCR,PPU_RAW,PPU_SCR,
                # filter criteria 
                YCH9A) %>% 
  mutate(chrAbuse = as.character(chrAbuse),
         chrAbuse = recode(chrAbuse,
                           # don't know and refused dropped for now (-1, -2)
                           `1` = "physical",
                           `2` = "sexual",
                           `3` = "emotional",
                           `4` = "neglect (physical)",
                           `5` = "neglect (supervision)",
                           `6` = "abandonment",
                           `7` = "moral/legal",
                           `8` = "educational",
                           `9` = "exploitation",
                           `10` = "other")) %>%
  # wave 3
  bind_rows(cps1_w3 %>% dplyr::select(NSCAWID, dataset, wave, ch3AgeY, ch3Race,
                                      CD39A, BC318A, BC391A, YB318A, YB391A) %>% 
              rename(chdAgeY = ch3AgeY,
                     chdRace = ch3Race,
                     # outcomes
                     PBC91A  = BC391A, 
                     PBC18A  = BC318A,
                     YCD9A   = CD39A,
                     YYB18A  = YB318A, 
                     YYB91A  = YB391A)) %>%
  # wave 4
  bind_rows(cps1_w4 %>% dplyr::select(NSCAWID, dataset, wave, ch4AgeY, ch4Race,
                                      CD49A, BC418A, BC491A, YB418A, YB491A) %>% 
              rename(chdAgeY = ch4AgeY,
                     chdRace = ch4Race,
                     PBC91A  = BC491A,
                     PBC18A  = BC418A,
                     YCD9A   = CD49A,
                     YYB18A  = YB418A, 
                     YYB91A  = YB491A)) %>%
  
#  NSCAW 2
# wave 1
bind_rows(cps2_w1 %>% dplyr::select(NSCAWID, dataset, wave, chdAgeY, chdrace,
                                    chdGendr,chdraceh,chrabuse, PBC_INT,PBC_EXT,nanalwt,
                                    PBC91a, PBC18a, YCD9a, YYB18a, YYB91a,NTS_E18, NTS_E91,
                                    YRP_TOT,YRR_TOT,PHO_CS3,PHO_ES3,PHO_HM3,YRC_ESA,YRC_ESB,YRC_INA,YRC_INB,YRC_ATA,YRC_ATB,YRC_STA,YRC_STB,
                                    PPS_RAW,PPS_SCR,PPT_RAW,PPT_SCR,PPU_RAW,PPU_SCR) %>% 
            rename(PBC91A = PBC91a, 
                   PBC18A = PBC18a,
                   YCD9A  = YCD9a,
                   YYB18A  = YYB18a,
                   YYB91A  = YYB91a,
                   chdRace = chdrace,
                   chdRaceH = chdraceh,
                   chrAbuse = chrabuse,
                   NANALWT  = nanalwt
            )  %>%
            mutate(chrAbuse = as.character(chrAbuse),
                   chrAbuse= recode(chrAbuse,
                                    `1` = "physical",
                                    `2` = "sexual",
                                    `3` = "emotional",
                                    `4` = "neglect (physical)",
                                    `5` = "neglect (supervision)",
                                    `6` = "abandonment",
                                    `7` = "moral/legal",
                                    `8` = "educational",
                                    `9` = "exploitation",
                                    `10` = "other",
                                    `11` = "other",
                                    `12` = "other",
                                    `13` = "other",
                                    `14` = "other",
                                    `15` = "other",
                                    `16` = "other",
                                    `17` = "other"))) %>%
  # wave 2
  bind_rows(cps2_w2 %>% dplyr::select(NSCAWID, dataset, wave, ch2AgeY, ch2race, 
                                      BC291a, BC218a, CD29a, YB218a, YB291a) %>% 
              rename(chdAgeY = ch2AgeY,
                     chdRace = ch2race,
                     PBC91A  = BC291a,
                     PBC18A  = BC218a,
                     YCD9A   = CD29a,
                     YYB18A  = YB218a,
                     YYB91A  = YB291a)) %>%
  # wave 3
  bind_rows(cps2_w3 %>% dplyr::select(NSCAWID, dataset, wave, ch3AgeY, ch3race, 
                                      BC391a, BC318a, CD39a, YB318a, YB391a) %>% 
              rename(chdAgeY = ch3AgeY,
                     chdRace = ch3race,
                     PBC91A  = BC391a,
                     PBC18A  = BC318a,
                     YCD9A  = CD39a,
                     YYB18A  = YB318a,
                     YYB91A  = YB391a)) %>%
  # merge variables
  left_join(
    bind_rows(
      dplyr::select(cps_acr, NSCAWID, CWLNOOH), 
      dplyr::select(cps_acr2, NSCAWID, CWLNOOH)
    ),
    by="NSCAWID") %>%
  # reocde to factor
  mutate(chrAbuse=
           recode_factor(chrAbuse,
                         "physical" = "physical",
                         "sexual" = "sexual",
                         "emotional" = "other",
                         "neglect (physical)" = "neglect",
                         "neglect (supervision)" = "neglect",
                         "abandonment" = "other",
                         "moral/legal" = "other",
                         "educational" = "other",
                         "exploitation" = "other",
                         "other" = "other")) %>%
  # create wide age columns
  mutate(
    chdAgeY = if_else(chdAgeY %in% c(-4, -2, -1), NA_real_, chdAgeY),
    w1_age = if_else(wave==1 & dataset=="NSCAW-I", chdAgeY, NA),
#    w2_age = if_else(wave==2 & dataset=="NSCAW-I", chdAgeY, NA),
    w3_age = if_else(wave==3 & dataset=="NSCAW-I", chdAgeY, NA),
    w4_age = if_else(wave==4 & dataset=="NSCAW-I", chdAgeY, NA),
#    w5_age = if_else(wave==5 & dataset=="NSCAW-I", chdAgeY, NA),
    w1.2_age = if_else(wave==1 & dataset=="NSCAW-II", chdAgeY, NA),
    w2.2_age = if_else(wave==2 & dataset=="NSCAW-II", chdAgeY, NA),
    w3.2_age = if_else(wave==3  & dataset=="NSCAW-II", chdAgeY, NA)
  )

if (!dir.exists("data/processed/aim3")) {
  dir.create("data/processed/aim3")
}
write.csv(aim3, "data/processed/aim3/Aim3_data.csv")
}
########################### Eligible IDs for Arielle's paper ###########################
##### Revised criteria ####
#criteria_as2 <- aim3 %>%
#  dplyr::select(NSCAWID, dataset, wave, chdAgeY, chdRace, w1_age, w3_age, w4_age,w1.2_age, w2.2_age, w3.2_age) %>% 
#  mutate(chdRace = as.character(chdRace),
#         chdRace = if_else(chdRace <3, NA_character_, chdRace)) %>%
#  dplyr::group_by(NSCAWID) %>%
##  fill(chdRace, .direction = "updown") %>%
#  filter(any(chdRace == "3") & !any(chdRace %in% c(1,2,4,5))) %>% # check with team
#  mutate(wave=if_else(dataset=="NSCAW-I" & wave == 3, 2, wave)) %>%
#  dplyr::group_by(NSCAWID, dataset) %>%
#  mutate(check = case_when(  (any(w1_age >=7) & any(w3_age %in% 7:17)) | 
#                             (any(w1_age >=7) & any(w4_age %in% 7:17)) |
#                             (any(w1.2_age >=7) & any(w3.2_age %in% 7:17)) |
#                             (any(w1.2_age >=7) & any(w2.2_age %in% 7:17)) ~ 1, TRUE ~ 0)) %>%
#  dplyr::ungroup() %>% arrange(NSCAWID) %>% filter(check==1)  %>% dplyr::select(NSCAWID)
#write.csv(criteria_as2, "data/processed/aim3/criteria.csv")
######################### Recode aim 3 data and subset to sample #######################3
recode_aim3  <- function(data=aim3) { 
  aim3_recode <- 
  data %>%
  # reconsider location
  mutate_if(is.double, as.numeric) %>%
  filter(wave==1) %>% ### WAVE 1 only
  filter(NSCAWID %in% criteria$NSCAWID) %>% ### Eligible IDs
#  mutate(PLEGAL = na_if(PLEGAL,"")) %>%
  # subset measure age
  gather(key, value, YRP_TOT, YRR_TOT, PHO_CS3,PHO_ES3, PHO_HM3, YRC_ESA,YRC_ESB,YRC_INA,YRC_INB,YRC_ATA,YRC_ATB,
         YRC_STA,YRC_STB,PPS_RAW,PPS_SCR,PPT_RAW,PPT_SCR,PPU_RAW,PPU_SCR) %>%
  ## combined observations based on age group
  # first standardize
  mutate(key2 = key, value2 = value) %>%
  mutate(
    key2 = paste0(key2, "_z"),
    # convert negative values before standardizing
    value2 = if_else(value2>-1, value2, NA_real_)) %>% 
  dplyr::group_by(key2, dataset, wave) %>%
  mutate(value2 = as.numeric(scale(value2))) %>%
  dplyr::ungroup() %>%
  #  recode to indicate who should have seen items
  mutate(
    value = case_when(
      key=="YRP_TOT" & (YCH9A %in% c(-7,-6,-1,2,3) | !chdAgeY %in% 5:7) ~ -888, 
      key=="YRR_TOT" & (YCH9A %in% c(-7,-6,-1,2,3) | chdAgeY < 8) ~ -888, 
      key %in% c("PHO_CS3","PHO_ES3","PHO_HM3") & !chdAgeY %in% 6:10 ~ -888,
      key %in% c('YRC_ESA','YRC_ESB','YRC_INA','YRC_INB','YRC_ATA','YRC_ATB','YRC_STA','YRC_STB') & 
        !chdAgeY %in% 11:17 ~ -888, 
      key %in% c('PPS_RAW', 'PPS_SCR')  & !chdAgeY %in% 3:5 ~ -888, 
      key %in% c('PPT_RAW', 'PPT_SCR')  & !chdAgeY %in% 6:10 ~ -888, 
      key %in% c('PPU_RAW', 'PPU_SCR')  & !chdAgeY >= 11 ~ -888, 
      TRUE ~ value
    )) %>%
  # replace NA with not applicable when relevant
  mutate(value2 = if_else(value<0, value, value2)) %>% 
  # spread(key,value) %>%
  pivot_wider(names_from = c("key", "key2"), values_from = c("value", "value2")) %>%
  dplyr::rename_with(~str_replace(., "_z", ""), starts_with("value_")) %>% 
  dplyr::rename_with(~str_after_nth(., "_", 3),  starts_with('value_')) %>% 
  dplyr::rename_with(~str_after_nth(., "_", 3),  starts_with('value2_')) %>%
  mutate(PP_combined = case_when(!PPS_SCR == -888 ~ PPS_SCR, # standardized already
                                 PPS_SCR  == -888  & !PPT_SCR == -888 ~ PPT_SCR,
                                 PPS_SCR  == -888  & !PPU_SCR == -888 ~ PPU_SCR, TRUE ~ NA_real_), 
         # NA is play here because when filtered by age, those who should have data are left
         YR_combined = case_when(!YRR_TOT_z == -888 ~ YRR_TOT_z,
                                 YRR_TOT_z == -888  & !YRP_TOT_z == -888 ~ YRP_TOT_z, TRUE ~ NA_real_),
         PBC91A_binary = case_when(PBC91A %in% 2:3 ~ 1,
                                   PBC91A %in% 1 ~ 0, TRUE ~ NA_real_),
         YYB91A_binary = case_when(YYB91A %in% 2:3 ~ 1,
                                   YYB91A %in% 1 ~ 0, TRUE ~ NA_real_),
         NTS_E91_binary = case_when(NTS_E91 %in% 1:2 ~ 1,
                                    NTS_E91 %in% 0 ~ 0, TRUE ~ NA_real_),
         YCD9A_binary = case_when(YCD9A %in% 2:3 ~ 1,
                                  YCD9A %in% 1 ~ 0, TRUE ~ NA_real_),
         chdGendr_r = recode(chdGendr,
                             `1` = "male",
                             `2` = "Female"),
         chdRace_r=recode(chdRace, 
                          `1`  = "American Indian",           
                          `2`  = "Asian/Hawaiian/Pacific Is", 
                          `3`  = "Black",                     
                          `4`  = "White",                    
                          `5`  = "Other",                     
                          `-1` = "Don't Know",                
                          `-2` = "Refused",	                  
                          `-4` = "Missing"),
         chdRaceH_r = recode(chdRaceH,
                             `1`  = "Black/Non-Hispanic",
                             `2`  = "White/Non-Hispanic",
                             `3`  = "Hispanic",
                             `4`  = "Other",
                             `-1` = "Don't Know",
                             `-2` = "Refused",
                             `-3` = "Not Applicable",
                             `-4` = "Missing"),
         PBC18A_recode =  
           recode(PBC18A, `-8`	= NA_character_,
                  `-7`	= NA_character_,
                  `-6`	= NA_character_,
                  `-5` = NA_character_,
                  `-3`	= NA_character_,
                  `-2`	= NA_character_,
                  `-1`	= NA_character_,
                  `1`	= 'NOT TRUE',
                  `2`	= 'SOMEWHAT OR SOMETIMES TRUE',
                  `3`	= 'VERY TRUE OR OFTEN TRUE'),
         PBC18A_recode = factor(PBC18A_recode, levels = c('NOT TRUE','SOMEWHAT OR SOMETIMES TRUE','VERY TRUE OR OFTEN TRUE'), ordered = T),
         PBC91A_recode = 
           recode(PBC91A, `-8`	= NA_character_,
                  `-7`	= NA_character_,
                  `-6`	= NA_character_,
                  `-5` = NA_character_,
                  `-3`	= NA_character_,
                  `-2`	= NA_character_,
                  `-1`	= NA_character_,
                  `1`	= 'NOT TRUE',
                  `2`	= 'SOMEWHAT OR SOMETIMES TRUE',
                  `3`	= 'VERY TRUE OR OFTEN TRUE'),
         PBC91A_recode = factor(PBC91A_recode, levels = c('NOT TRUE','SOMEWHAT OR SOMETIMES TRUE','VERY TRUE OR OFTEN TRUE'), ordered = T),
         YYB18A_recode = recode(YYB18A, 
                                `-8`	= NA_character_,
                                `-7`	= NA_character_,
                                `-6`	= NA_character_,
                                `-5` = NA_character_,
                                `-3`	= NA_character_,
                                `-2`	= NA_character_,
                                `-1`	= NA_character_,
                                `1`	= 'NOT TRUE',
                                `2`	= 'SOMEWHAT OR SOMETIMES TRUE',
                                `3`	= 'VERY TRUE OR OFTEN TRUE'), 
         YYB18A_recode = factor(YYB18A_recode, levels = c('NOT TRUE','SOMEWHAT OR SOMETIMES TRUE','VERY TRUE OR OFTEN TRUE'), ordered = T),
         YYB91A_recode = recode(
           YYB91A, `-8`	= NA_character_,
           `-7`	= NA_character_,
           `-6`	= NA_character_,
           `-5` = NA_character_,
           `-3`	= NA_character_,
           `-2`	= NA_character_,
           `-1`	= NA_character_,
           `1`	= 'NOT TRUE',
           `2`	= 'SOMEWHAT OR SOMETIMES TRUE',
           `3`	= 'VERY TRUE OR OFTEN TRUE'),
         YYB91A_recode = factor(YYB91A_recode, levels = c('NOT TRUE','SOMEWHAT OR SOMETIMES TRUE','VERY TRUE OR OFTEN TRUE'), ordered = T),
         YCD9A_recode = recode(YCD9A, 
                               `-8`	= NA_character_,
                               `-7`	= NA_character_,
                               `-6`	= NA_character_,
                               `-5` = NA_character_,
                               `-3`	= NA_character_,
                               `-2`	= NA_character_,
                               `-1`	= NA_character_,
                               `1`	= 'I DO NOT THINK ABOUT KILLING MYSELF',
                               `2`	= 'I THINK ABOUT KILLING MYSELF BUT I WOULD',
                               `3`	= 'I WANT TO KILL MYSELF'),
         YCD9A_recode = factor(YCD9A_recode, levels=c('I DO NOT THINK ABOUT KILLING MYSELF','I THINK ABOUT KILLING MYSELF BUT I WOULD','I WANT TO KILL MYSELF'), ordered = T),
         # NTS_E18, NTS_E91
         NTS_E18_recode = recode(NTS_E18, 
                                 `-9` = NA_character_,
                                 `-8`	= NA_character_,
                                 `-7`	= NA_character_,
                                 `-6`	= NA_character_,
                                 `-5` = NA_character_,
                                 `-3`	= NA_character_,
                                 `-2`	= NA_character_,
                                 `-1`	= NA_character_,
                                 `0`	= 'NOT TRUE',
                                 `1`	= 'SOMEWHAT OR SOMETIMES TRUE',
                                 `2`	= 'VERY TRUE OR OFTEN TRUE'), 
         NTS_E18_recode = factor(NTS_E18_recode, levels = c('NOT TRUE','SOMEWHAT OR SOMETIMES TRUE','VERY TRUE OR OFTEN TRUE'), ordered = T),
         NTS_E91_recode = recode(NTS_E91, 
                                 `-9` = NA_character_,
                                 `-8`	= NA_character_,
                                 `-7`	= NA_character_,
                                 `-6`	= NA_character_,
                                 `-5` = NA_character_,
                                 `-3`	= NA_character_,
                                 `-2`	= NA_character_,
                                 `-1`	= NA_character_,
                                 `0`	= 'NOT TRUE',
                                 `1`	= 'SOMEWHAT OR SOMETIMES TRUE',
                                 `2`	= 'VERY TRUE OR OFTEN TRUE'),
         NTS_E91_recode = factor(NTS_E91_recode, levels = c('NOT TRUE','SOMEWHAT OR SOMETIMES TRUE','VERY TRUE OR OFTEN TRUE'), ordered = T)
  ) 

write.csv(aim3_recode, "data/processed/aim3/Aim3_recoded_data.csv")
}

#### Revised criteria ####
sample_criteria <- function(data) {
  
criteria_as2 <- data %>% # aim3 data
  dplyr::select(NSCAWID, dataset, wave, chdAgeY, chdRace, w1_age, w3_age, w4_age,w1.2_age, w2.2_age, w3.2_age) %>% 
  mutate(chdRace = as.character(chdRace),
         chdRace = if_else(chdRace <3, NA_character_, chdRace)) %>%
  dplyr::group_by(NSCAWID) %>%
  #  fill(chdRace, .direction = "updown") %>%
  filter(any(chdRace == "3") & !any(chdRace %in% c(1,2,4,5))) %>% # check with team
  mutate(wave=if_else(dataset=="NSCAW-I" & wave == 3, 2, wave)) %>%
  dplyr::group_by(NSCAWID, dataset) %>%
  mutate(check = case_when(  (any(w1_age >=7) & any(w3_age %in% 7:17)) | 
                               (any(w1_age >=7) & any(w4_age %in% 7:17)) |
                               (any(w1.2_age >=7) & any(w3.2_age %in% 7:17)) |
                               (any(w1.2_age >=7) & any(w2.2_age %in% 7:17)) ~ 1, TRUE ~ 0)) %>%
  dplyr::ungroup() %>% arrange(NSCAWID) %>% filter(check==1)  %>% dplyr::select(NSCAWID)
write.csv(criteria_as2, "data/processed/aim3/criteria.csv")
}

######################### save dataset for cronbach alpha ######################
if(overwrite == T) {
  library(ltm)
  crit <- read.csv("data/processed/aim3/criteria.csv")
  ################################### Social Skills ################################### 
  # NSCAW 1 SSRS
  c1 <- select(cps1_w1, NSCAWID, dataset, wave, chdAgeY, PPT1A:PPT38A, PPT_SCR) %>% filter(chdAgeY %in% 6:10 & NSCAWID %in% crit$NSCAWID)
  names(c1) <- c("NSCAWID", "dataset", "wave", "chdAgeY", paste0("SSRS", 1:38), "PPT_SCR")
  c2 <- select(cps1_w1, NSCAWID, dataset, wave, chdAgeY, PPU1A:PPU40A, PPU_SCR) %>% filter(chdAgeY >=11)
  names(c2) <- c("NSCAWID", "dataset", "wave", "chdAgeY", paste0("SSRS", 1:40), "PPU_SCR")
  # NSCAW 2 SSRS
  c3 <- select(cps2_w1, NSCAWID, dataset, wave, chdAgeY, PPT1a:PPT38a, PPT_SCR) %>% filter(chdAgeY %in% 6:10 & NSCAWID %in% crit$NSCAWID)
  names(c3) <- c("NSCAWID", "dataset", "wave", "chdAgeY", paste0("SSRS", 1:38), "PPT_SCR")
  c4 <- select(cps2_w1, NSCAWID, dataset, wave, chdAgeY, PPU1a:PPU40a, PPU_SCR) %>% filter(chdAgeY >=11 & NSCAWID %in% crit$NSCAWID)
  names(c4) <- c("NSCAWID", "dataset", "wave", "chdAgeY", paste0("SSRS", 1:40), "PPU_SCR")
  
  ssrs1 <- bind_rows(c1, c3) %>%
    mutate(across(contains("SSRS"), ~as.numeric(.))) %>%
    mutate(across(contains("SSRS"), ~recode(., `-7`=NA_real_, `-6`=NA_real_,`-5`=NA_real_,`-2`=0, `-1`=0, `1`=0, `2`=1, `3`=2)))
  
 ca1 <- cronbach.alpha(data=dplyr::select(ssrs1, contains("SSRS")))
  
  ssrs2 <- bind_rows(c2, c4) %>%
    mutate(across(contains("SSRS"), ~as.numeric(.))) %>%
    mutate(across(contains("SSRS"), ~recode(., `-7`=NA_real_, `-6`=NA_real_,`-5`=NA_real_,`-2`=0, `-1`=0, `1`=0, `2`=1, `3`=2)))
  
  ca2 <- cronbach.alpha(data=dplyr::select(ssrs2, contains("SSRS")), na.rm=T)

ssrs_all <-   bind_rows(select(c1, NSCAWID, PPT_SCR), 
                        select(c2, NSCAWID, PPU_SCR),
                        select(c3, NSCAWID, PPT_SCR),
                        select(c4, NSCAWID, PPU_SCR)) %>% 
    mutate(PP = case_when(
      is.na(PPT_SCR) & !is.na(PPU_SCR) ~ PPU_SCR,
      is.na(PPU_SCR) & !is.na(PPT_SCR) ~ PPT_SCR, TRUE ~ NA_real_
    )) %>%
    filter(NSCAWID %in% crit$NSCAWID) 

ssrs_all %>%
    summarise(mean = mean(PP, na.rm=T), sd=sd(PP, na.rm=T))
  ################################### Loneliness ###################################  
  # NSCAW 1 Loneliness
  d1 <- select(cps1_w1, NSCAWID, dataset, wave, chdAgeY, YRP1A:YRP16A, YRP_TOT) %>% filter(chdAgeY %in% 5:7 & NSCAWID %in% crit$NSCAWID)
  names(d1) <- c("NSCAWID", "dataset", "wave", "chdAgeY", paste0("YL", 1:16), "YRP_TOT")
  d2 <- select(cps1_w1, NSCAWID, dataset, wave, chdAgeY, YRR1A:YRR16A, YRR_TOT) %>% filter(chdAgeY %in% 8:17 & NSCAWID %in% crit$NSCAWID)
  names(d2) <- c("NSCAWID", "dataset", "wave", "chdAgeY", paste0("YL", 1:16), "YRR_TOT")
  # NSCAW 2 Loneliness
  d3 <- select(cps2_w1, NSCAWID, dataset, wave, chdAgeY, YRP1ax:YRP16a, YRP_TOT) %>% filter(chdAgeY %in% 5:7 & NSCAWID %in% crit$NSCAWID)
  names(d3) <- c("NSCAWID", "dataset", "wave", "chdAgeY", paste0("YL", 1:16), "YRP_TOT")
  d4 <- select(cps2_w1, NSCAWID, dataset, wave, chdAgeY, YRP1ax:YRP16a, YRR_TOT) %>% filter(chdAgeY %in% 8:17 & NSCAWID %in% crit$NSCAWID)
  names(d4) <- c("NSCAWID", "dataset", "wave", "chdAgeY", paste0("YL", 1:16), "YRR_TOT")
  
  yl1 <- bind_rows(d1, d3) %>%
    mutate(across(contains("YL"), ~as.numeric(.))) %>%
    mutate(across(contains("YL"), ~if_else(.<0, NA_real_, .))) %>%
    mutate_at(vars(YL1, YL2, YL3, YL5, YL7, YL9, YL10, YL12, YL15, YL16), ~ recode(., `2`=3, `3`=2)) %>%
    mutate_at(vars(YL4, YL6, YL8, YL11, YL13, YL14), ~recode(., `1`=3, `2`=1, `3`=2))
  
  ca3 <- cronbach.alpha(data=dplyr::select(yl1, contains("YL")), na.rm=T)
  
  yl2 <- bind_rows(d2, d4) %>%
    mutate(across(contains("YL"), ~as.numeric(.))) %>%
    mutate(across(contains("YL"), ~if_else(.<0, NA_real_, .))) %>%
    mutate_at(vars(YL1,YL3,YL5,YL7,YL10, YL15), ~recode(., `1`=5, `2`=4, `4`=2, `5`=1))
  
ca4 <- cronbach.alpha(data=dplyr::select(yl2, contains("YL")), na.rm=T)
  
alphas <- tibble(
  `Social Skills 6-10` = ca1$alpha,
  `Social Skills 11+` = ca2$alpha,
  `Loneliness 5-7` = ca3$alpha,
  `Loneliness 8-17` = ca4$alpha
)

alphas

yl_all <-   bind_rows(  select(d1, NSCAWID, YRP_TOT), 
                        select(d2, NSCAWID, YRR_TOT),
                        select(d3, NSCAWID, YRP_TOT),
                        select(d4, NSCAWID, YRR_TOT)) %>% 
  mutate(YL = case_when(
    is.na(YRP_TOT) & !is.na(YRR_TOT) ~ YRR_TOT,
    is.na(YRR_TOT) & !is.na(YRP_TOT) ~ YRP_TOT, TRUE ~ NA_real_
  )) %>%
  filter(NSCAWID %in% crit$NSCAWID) 
yl_all %>%
  summarise(mean = mean(YL, na.rm=T), sd=sd(YL, na.rm=T))

cor_dat <- bind_cols(
  select(ssrs_all, NSCAWID, PP), select(yl_all, YL)
) 

cor(cor_dat$PP, cor_dat$YL, use = "complete.obs")

}