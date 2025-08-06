source('Elie/aim3/Code/3_aim3_imputation.R')
######### Fit model #########
imp2 <- as.mids(imputed_data_long)

fit1 <- with(data=imp2,exp=glmer(post~pre+dataset+age.binned+chdGendr+chdRaceH+chrAbuse+
                                   PP_combined + YR_combined + chdGendr*age.binned +
                                   (1|NSCAWID), family=binomial(link="logit")))

############################ Format models ########################################
options(scipen = 999)
sum1 = summary(pool(fit1), conf.int = TRUE, exponentiate = TRUE)
mod1 <- sum1 %>%
  mutate_if(is.numeric, ~round(., 4)) %>%
  mutate(CI = paste0(round(conf.low, 2), ", ", round(conf.high, 2)),
         Odds = round(estimate, 2),
         df = round(df, 2)
         ) %>%
  select(term, Odds, CI, df, p.value) %>%
  mutate(term = 
           recode(term, 
                  "datasetNSCAW2"   = "dataset (NSCAW 2)",
                  "age.binned11-14" = "Age 11-14 (ref = 7-10)",
                  "age.binned15-17" = "Age 15-17 (ref = 7-10)",
                  "chdGendrFemale"    = "Gender (female)",
                  "chrAbuseother"   = "Abuse (other)",
                  "chrAbuseneglect" = "Abuse (neglect)",
                  "chrAbusephysical"= "Abuse (physical)",
                  "chdRaceHHispanic" = "Hispanic (hispanic)",
                  "chdRaceHother" = "Hispanic (other)",
                  "PP_combined"     = "Parent/teacher reported Social Skills",
                  "YR_combined"     = "Child reported total Loneliness and Social Dissatisfaction",
                  # interactions
                  "age.binned11-14:chdGendrFemale"         =  "female x age 11-14",              
                  "age.binned15-17:chdGendrFemale"         =  "female x age 15-17"
           )) %>%
  gt(rowname_col = "term") %>%
  tab_row_group(label = "", rows = 1) %>%
  tab_row_group(label = "treatment", rows = 2) %>%
  tab_row_group(label = "covariates", rows = 3:11) %>%
  tab_row_group(label = "protective factors", rows = 12:13) %>%
  tab_row_group(label = "interactions", rows = 14:15) %>%
  row_group_order(groups = c("","treatment", "protective factors", "covariates", "interactions")) %>%
  tab_header(title = "W2 and W3 (post ideation) Logistic Regression Model with Person level Random Effects")

# plot interactions  
em_int <- emmeans(fit1, ~chdGendr*age.binned, type = "reponse")

tidy <- as_tibble(em_int) %>%
  mutate_at(vars(emmean, lower.CL, upper.CL), ~exp(.)) %>%
  mutate(chdGendr, as.character(chdGendr), chdGendr=recode_factor(chdGendr, male="Male"))

ggplot(tidy, aes(x = emmean, y = age.binned, color = chdGendr)) +
     geom_point(size = 3,  position = position_dodge(width = 0.5)) +  # Points aligned on y-axis categories
     geom_errorbarh(
           aes(xmin = lower.CL, xmax = upper.CL), 
           size = 1.2,  # Thicker lines for clarity
           height = 0.1,  # Narrow caps for neatness
           position = position_dodge(width = 0.5) 
       ) +
  scale_color_manual(values = c("Male" = "deepskyblue2", "Female" = "darkorange2")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
     labs(x = "Odds", y = "Age Binned", color="Sex")

ggplot(tidy, aes(y = emmean, x = age.binned, color = chdGendr, group=chdGendr)) +
  geom_point(aes(shape=chdGendr), size = 3,  position = position_dodge(width = 0.2)) +  # Points aligned on y-axis categories
  geom_line(size=0.5, position = position_dodge(width = 0.2)) +
  theme(text = element_text(size = 15)) + 
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL), 
    size = 0.5,  # Thicker lines for clarity
    height = 0.2,  width = 0.1, # Narrow caps for neatness
    position = position_dodge(width = 0.2) 
  ) +
  scale_color_manual(values = c("Male" = "deepskyblue2", "Female" = "darkorange2")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y = "Odds", x = "Age", color="Sex", shape="Sex")

gt::gtsave(mod1, "Elie/aim3/output/model_table.docx")
