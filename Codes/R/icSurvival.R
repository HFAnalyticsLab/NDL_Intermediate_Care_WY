# IC survival analysis - time-to-readmission

# setup -------------------------------------------------------------------
need_packages <- c(
  "tidyverse",
  "ISLR2",
  "censored",
  "survival"
)

installed <- need_packages %in% installed.packages()
if(length(need_packages[!installed]) > 0) install.packages(need_packages[!installed])
lapply(need_packages, library, character.only = TRUE)

# etl ---------------------------------------------------------------------
source(file = "icSetup.R")
# conflicted::conflicts_prefer(config::get)
setwd(dir = "~/project-4-ic/anna/final/")
source(file = "~/project-4-ic/anna/final/Hospital_IC_Continuity_AB_src.R")
setwd(dir = "~/project-4-ic/alex/")

# eda ---------------------------------------------------------------------

## final1 -------------------------------------------------------------------
dim(final1)
n_distinct(final1$Patient_ID)

final1 %>% 
  ggplot(aes(x = IC_Hosp_readmission_time)) +
  geom_histogram()

final1 %>% 
  ggplot(aes(x = hosp_next_date)) +
  geom_histogram()

final1 %>% 
  # count(is.na(Date_of_Death)) %>% 
  count(SDIC == 1, is.na(REG_DATE_OF_DEATH))

final1 %>% 
  filter(SDIC == 1) %>% 
  filter(Patient_ID == "311330285298337296298360314314") %>%
  # distinct()
  # select(Patient_ID, Attendance_Date, IC_Hosp_readmission_time) %>%
  # count(Patient_ID, Attendance_Date, sort = T) %>%
  View()

names(final1)
final1 %>%
  # count(is.na(Date_of_Death))
  count(!is.na(REG_DATE_OF_DEATH))
  # count(is.na(Date_of_Death), is.na(REG_DATE_OF_DEATH))

all <-
  final1 %>%
  arrange(Patient_ID, Attendance_Date) %>% 
  mutate(
    across(.cols = contains("Date"), .fns = ~as.Date(.))
  ) %>% 
  filter(Attendance_Date >= as.Date("2021-09-01") & 
           Attendance_Date <= as.Date("2023-09-01") &
           (Age >= 18 | is.na(Age))
  ) %>% 
  # filter(
  #   SDIC == 1 &
  #     (Attendance_Date <= IC_Attendance_Date)
  # ) %>%
  group_by(GP_Count,Last_GP_Event,UC111_Count,Last_UC111_Event,UC999_Count,Last_UC999_Event,UCOOH_Count,
           Last_UCOOH_Event,ASCCOM_Count,Last_ASCCOM_Event,ComServ_Count,Last_ComServ_Event,IC_Count,Last_IC_Event,
           AE_Count,Last_AE_Event,Patient_ID,Age,Age_Band_FW,Sex,Practice_Code,LSOA,PCN,IMD_Decile,Population_Segment,
           Smoke_status_y01,Frailty_Index,LTC_count,LTC_Dementia,Care_Home,Carer,
           Decision_to_Refer_to_Service_Date,Attendance_Date,Discharge_Date,PoD,Dimention_2,Dimention_3,
           Discharge_Location,HRG4,Diagnostic,Operat_Procedure,Provider_Code,Record_Classification,
           Discharge_Method_Code,Patient_Classification_Code,Carer_Support_Indicator,
           AE_Date_After_Spell,
           AC_LOS,Age_Band,Sex_Desc,IMD_Quintile_New,Ethnic_Category_Desc,Ethnic_Group_Desc,Discharge_Location_Desc,
           Discharge_Method_Desc,Diagnostic_Desc,Diagnostic_Chpt,Treatment_Function_Desc,Admission_Method_Desc,HRG4_Desc,
           Carer_Support_Desc,OPCS4_Desc,Patient_Classification_Desc,PoD_Desc,Provider_Desc,
           hosp_next_date,hosp_next_pod,hosp_time_gap, 
           REG_DATE_OF_DEATH,
           SDIC
           # Sensitivity
  ) %>%
  summarise(IC_Discharge_Date = max(IC_Discharge_Date),
            IC_Attendance_Date = min(IC_Attendance_Date), 
            IC_Contact_Nr = sum(IC_Contact_Nr),
            Type=paste(sort(unique(Type)), collapse = "-"),
            Group=paste(unique(Group), collapse = "-"),
            IC_Continuity=sum(IC_continuity)
  ) %>%
  mutate(HLOS = as.numeric((difftime(Discharge_Date, Attendance_Date, units='day'))),
         IC_LOS  = as.numeric((difftime(IC_Discharge_Date , IC_Attendance_Date, units='day') )),
         IC_TimeToFirstContact =as.numeric((difftime( IC_Attendance_Date, Discharge_Date, units='day'))),
         IC_Hosp_readmission_time=as.numeric((difftime(hosp_next_date, IC_Discharge_Date , units='day') )),
         Type =ifelse(grepl("CCB", Type), "CCB", Type)
  ) %>%
  ungroup() %>%
  select(
    Patient_ID,
    Attendance_Date,
    Age,
    GP_Count,
    IC_Count,
    Sex = Sex_Desc,
    Age_group = Age_Band,
    Ethnic_group = Ethnic_Group_Desc,
    IMD_quintile = IMD_Quintile_New,
    Comorbidities = LTC_count,
    Frailty_index = Frailty_Index,
    Type,
    Group,
    PoD,
    HLOS,
    IC_LOS,
    IC_TimeToFirstContact,
    LTC_Dementia,
    IC_Hosp_readmission_time, 
    IC_Continuity,
    REG_DATE_OF_DEATH,
    SDIC
    # Sensitivity
  ) %>% 
  mutate(
    Overall = "overall",
    IMD_quintile = fct_explicit_na(f = factor(x = IMD_quintile, ordered = T), 
                                   na_level = "Unknown"),
    Age_group = fct_explicit_na(f = factor(x = Age_group, ordered = T), 
                                na_level = "Unknown"),
    Comorbidities = fct_explicit_na(f = factor(x = Comorbidities, ordered = F),
                                    na_level = "Unknown"),
    Frailty_index = fct_explicit_na(f = factor(x = Frailty_index, ordered = F),
                                    na_level = "Unknown"),
    LTC_Dementia= fct_explicit_na(f = factor(x = LTC_Dementia, ordered = F),
                                  na_level = "Unknown")
  ) %>%
  mutate(Type=case_when(
    grepl("ASCCOM-CBS-CBScon",Type)| grepl("ASCCOM-CBS",Type) | grepl("ASCCOM-CBS",Type) ~ "ASCCOM-CBS", 
    grepl("CBS-CBScon" ,Type)~ "CBS",
    .default = Type)
  ) %>% 
  mutate(.data = ., across(.cols = where(is.character),
                           .fns = ~fct_explicit_na(f = ., na_level = "Unknown"))
  ) 
  # mutate(
  #   time_to_death = as.integer(REG_DATE_OF_DEATH - Attendance_Date),
  #   readmission30 = if_else(!is.na(IC_Hosp_readmission_time) & IC_Hosp_readmission_time <= 30, 1, 0),
  #   death30 = if_else(time_to_death <= 30, 1, 0),
  #   readmission60 = if_else(!is.na(IC_Hosp_readmission_time) & IC_Hosp_readmission_time <= 60, 1, 0),
  #   death60 = if_else(time_to_death <= 60, 1, 0),
  #   readmission90 = if_else(!is.na(IC_Hosp_readmission_time) & IC_Hosp_readmission_time <= 90, 1, 0),
  #   death90 = if_else(time_to_death <= 90, 1, 0)
  # )

# time_to_death & death30 needs to be relative IC_discharge as per readmission30

glimpse(all)
names(all)
dim(all)
n_distinct(all$Patient_ID)
all %>% count(Patient_ID, sort = T)

vars <- 
  c("GP_Count",
    "IC_Count",
    "Age",
    "Sex",
    "Frailty_index",
    "Comorbidities",
    "LTC_Dementia",
    "Ethnic_group",
    "HLOS",
    "PoD",
    "Type",
    "IC_LOS",
    "IC_TimeToFirstContact",
    "IC_Hosp_readmission_time",
    "time_to_death"
  )


## ic ---------------------------------------------------------------
ic <-
  final1 %>%
  arrange(Patient_ID, Attendance_Date) %>% 
  mutate(
    across(.cols = contains("Date"), .fns = ~as.Date(.))
  ) %>% 
  filter(Attendance_Date >= as.Date("2021-09-01") & 
           Attendance_Date <= as.Date("2023-09-01") &
           (Age >= 18 | is.na(Age))
  ) %>% 
  filter(
    SDIC == 1 &
      (Attendance_Date <= IC_Attendance_Date)
  ) %>%
  group_by(GP_Count,Last_GP_Event,UC111_Count,Last_UC111_Event,UC999_Count,Last_UC999_Event,UCOOH_Count,
           Last_UCOOH_Event,ASCCOM_Count,Last_ASCCOM_Event,ComServ_Count,Last_ComServ_Event,IC_Count,Last_IC_Event,
           AE_Count,Last_AE_Event,Patient_ID,Age,Age_Band_FW,Sex,Practice_Code,LSOA,PCN,IMD_Decile,Population_Segment,
           Smoke_status_y01,Frailty_Index,LTC_count,LTC_Dementia,Care_Home,Carer,
           Decision_to_Refer_to_Service_Date,Attendance_Date,Discharge_Date,PoD,Dimention_2,Dimention_3,
           Discharge_Location,HRG4,Diagnostic,Operat_Procedure,Provider_Code,Record_Classification,
           Discharge_Method_Code,Patient_Classification_Code,Carer_Support_Indicator,
           AE_Date_After_Spell,
           AC_LOS,Age_Band,Sex_Desc,IMD_Quintile_New,Ethnic_Category_Desc,Ethnic_Group_Desc,Discharge_Location_Desc,
           Discharge_Method_Desc,Diagnostic_Desc,Diagnostic_Chpt,Treatment_Function_Desc,Admission_Method_Desc,HRG4_Desc,
           Carer_Support_Desc,OPCS4_Desc,Patient_Classification_Desc,PoD_Desc,Provider_Desc,
           hosp_next_date,hosp_next_pod,hosp_time_gap, 
           REG_DATE_OF_DEATH
           # Sensitivity
           ) %>%
  summarise(IC_Discharge_Date = max(IC_Discharge_Date),
            IC_Attendance_Date = min(IC_Attendance_Date), 
            IC_Contact_Nr = sum(IC_Contact_Nr),
            Type=paste(sort(unique(Type)), collapse = "-"),
            Group=paste(unique(Group), collapse = "-"),
            IC_Continuity=sum(IC_continuity)
  ) %>%
  mutate(HLOS = as.numeric((difftime(Discharge_Date, Attendance_Date, units='day'))),
         IC_LOS  = as.numeric((difftime(IC_Discharge_Date , IC_Attendance_Date, units='day') )),
         IC_TimeToFirstContact =as.numeric((difftime( IC_Attendance_Date, Discharge_Date, units='day'))),
         IC_Hosp_readmission_time=as.numeric((difftime(hosp_next_date, IC_Discharge_Date , units='day') )),
         Type =ifelse(grepl("CCB", Type), "CCB", Type)
  ) %>%
  ungroup() %>%
  select(
    Patient_ID,
    Attendance_Date,
    Age,
    GP_Count,
    IC_Count,
    Sex = Sex_Desc,
    Age_group = Age_Band,
    Ethnic_group = Ethnic_Group_Desc,
    IMD_quintile = IMD_Quintile_New,
    Comorbidities = LTC_count,
    Frailty_index = Frailty_Index,
    Type,
    Group,
    PoD,
    HLOS,
    IC_LOS,
    IC_TimeToFirstContact,
    LTC_Dementia,
    IC_Hosp_readmission_time, 
    IC_Continuity,
    IC_Discharge_Date,
    REG_DATE_OF_DEATH
    # Sensitivity
  ) %>% 
  mutate(
    Overall = "overall",
    IMD_quintile = fct_explicit_na(f = factor(x = IMD_quintile, ordered = T), 
                                   na_level = "Unknown"),
    Age_group = fct_explicit_na(f = factor(x = Age_group, ordered = T), 
                                na_level = "Unknown"),
    Comorbidities = fct_explicit_na(f = factor(x = Comorbidities, ordered = F),
                                    na_level = "Unknown"),
    Frailty_index = fct_explicit_na(f = factor(x = Frailty_index, ordered = F),
                                    na_level = "Unknown"),
    LTC_Dementia= fct_explicit_na(f = factor(x = LTC_Dementia, ordered = F),
                                  na_level = "Unknown")
  ) %>%
  mutate(Type=case_when(
    grepl("ASCCOM-CBS-CBScon",Type)| grepl("ASCCOM-CBS",Type) | grepl("ASCCOM-CBS",Type) ~ "ASCCOM-CBS", 
    grepl("CBS-CBScon" ,Type)~ "CBS",
    .default = Type)
  ) %>% 
  mutate(.data = ., across(.cols = where(is.character),
                           .fns = ~fct_explicit_na(f = ., na_level = "Unknown"))
         ) %>% 
  mutate(
    readmission30 = if_else(!is.na(IC_Hosp_readmission_time) & IC_Hosp_readmission_time <= 30, 1, 0),
    readmission60 = if_else(!is.na(IC_Hosp_readmission_time) & IC_Hosp_readmission_time <= 60, 1, 0),
    readmission90 = if_else(!is.na(IC_Hosp_readmission_time) & IC_Hosp_readmission_time <= 90, 1, 0),
    time_to_death = as.integer(REG_DATE_OF_DEATH - IC_Discharge_Date),
    death30 = if_else(time_to_death <= 30 & !is.na(time_to_death), 1, 0),
    death60 = if_else(time_to_death <= 60 & !is.na(time_to_death), 1, 0),
    death90 = if_else(time_to_death <= 90 & !is.na(time_to_death), 1, 0)
  )

glimpse(ic)
names(ic)
dim(ic)
n_distinct(ic$Patient_ID)
ic %>% count(Patient_ID, sort = T)

# added above
# ic$readmission30<-ifelse(!is.na(ic$IC_Hosp_readmission_time) & ic$IC_Hosp_readmission_time<=30, 1, 0)
# REG_DATE_OF_DEATH
# time_to_death
# death30

# final1 ------------------------------------------------------------------
# right censoring - see above
glimpse(all)
all %>% 
  select(vars, SDIC) %>%
  count(time_to_death < 30)

all_surv <-
  all %>% 
  select(
    SDIC,
    vars,
    time_to_readmission = IC_Hosp_readmission_time,
    readmission30,
    time_to_death,
    death30
  ) %>% 
  mutate(
    time_to_readmission = if_else(readmission30 == 0, 30, time_to_readmission),
    time = case_when(
      !is.na(time_to_death) & (time_to_death <= time_to_readmission) ~ time_to_death,
      !is.na(time_to_readmission) & (time_to_death >= time_to_readmission) ~ time_to_readmission,
      TRUE ~ time_to_readmission
    ),
    status = if_else(time < 30, 1, 0)
  )

all_surv %>% 
  filter(time_to_death <= 30 & readmission30 == 0)
View(all_surv)

all_surv %>% 
  count(time, status)
View()

all_surv %>%
  ggplot(aes(x = time, fill = as.character(status), group = as.character(status))) +
  # geom_density(alpha = 0.6) +
  geom_histogram() +
  facet_wrap(~status) +
  theme(legend.position = "top")

names(all_surv)

# check
all_surv %>% 
  count(status)

all_surv %>% 
  select(time_to_death, death30, time_to_readmission, readmission30, time, status) %>%
  filter(!is.na(time_to_death)) %>% 
  View()

## surv --------------------------------------------------------------------
attach(all_surv)    
fit.surv <- 
  survfit(Surv(time, status) ~ 1)
plot(fit.surv, xlab = "Days", ylab = "Estimated Probability of Survival")

# stratify by sex
fit.sex <- 
  survfit(Surv(time, status) ~ Sex)
plot(fit.sex, xlab = "Days", ylab = "Estimated Probability of Survival", col = c(2,4))
legend("bottomleft", levels(Sex), col = c(2,4), lty = 1)

# logrank
logrank.test <- survdiff(Surv(time, status) ~ Sex)
broom::tidy(logrank.test) %>% 
  write.xlsx(., 
             file = "local/survival/all_surv_km_sex_logrank_crosstab.xlsx", 
             asTable = TRUE)
broom::glance(logrank.test) %>% 
  write.xlsx(., 
             file = "local/survival/all_surv_km_sex_logrank_statistic.xlsx", 
             asTable = TRUE)

logrank.test$chisq[1]

## coxph -------------------------------------------------------------------
fit.cox <- coxph(Surv(time, status) ~ Sex)
summary(fit.cox)

broom::tidy(fit.cox) %>% 
  write.xlsx(., 
             file = "local/survival/all_surv_coxph_sex_coeffs.xlsx", 
             asTable = TRUE)

broom::glance(fit.cox) %>% 
  pivot_longer(., cols = dplyr::everything(), names_to = "metric", values_to = "values") %>% 
  write.xlsx(., 
             file = "local/survival/all_surv_coxph_sex_summ.xlsx", 
             asTable = TRUE)


# metrics
summary(fit.cox)$logtest[1]
summary(fit.cox)$waldtest[1]
summary(fit.cox)$sctest[1]
# sctest == chisq

fit.sdic <- coxph(Surv(time, status) ~ SDIC)
summary(fit.sdic)

broom::tidy(fit.sdic) %>% 
  write.xlsx(., 
             file = "local/survival/all_surv_coxph_sdic_coeffs.xlsx", 
             asTable = TRUE)

broom::glance(fit.sdic) %>% 
  pivot_longer(., cols = dplyr::everything(), names_to = "metric", values_to = "values") %>% 
  write.xlsx(., 
             file = "local/survival/all_surv_coxph_sdic_summ.xlsx", 
             asTable = TRUE)

# metrics
summary(fit.sdic)$logtest[1]
summary(fit.sdic)$waldtest[1]
summary(fit.sdic)$sctest[1]
# sctest == chisq

# coxph add predictors
glimpse(all_surv)
all_surv <- 
  all_surv %>% 
  mutate(
    Frailty_index = as.integer(Frailty_index),
    Comorbidities = as.integer(Comorbidities)
  )

attach(all_surv)
# fit.all <- coxph(Surv(time, status) ~ Sex + Type + PoD + Age + HLOS)
fit.all <- 
  coxph(Surv(time, status) ~ GP_Count + IC_Count + Age + HLOS + Sex + 
          Frailty_index + Comorbidities + SDIC)
levels(Sex)

broom::tidy(fit.all, exp = TRUE) %>% 
  # pivot_longer(., cols = dplyr::everything(), names_to = "metric", values_to = "values") %>% 
  write.xlsx(., 
             file = "local/survival/all_surv_coxph_all_coeff.xlsx", 
             asTable = TRUE)

broom::augment(fit.all)
broom::glance(fit.all) %>% 
  pivot_longer(., cols = dplyr::everything(), names_to = "metric", values_to = "values") %>% 
  write.xlsx(., 
             file = "local/survival/all_surv_coxph_all_summ.xlsx", 
             asTable = TRUE)

summary(fit.all)
summary(fit.all)$logtest[1]
summary(fit.all)$waldtest[1]
summary(fit.all)$sctest[1]

# ic ------------------------------------------------------------------
# right censoring - independent mechanisms??
# 30 days from ac discharge
# days from ac discharge to death
# days from ac discharge to IC discharge (or IC LOS)??

# event
# days to readmission

# left censoring??
# time to 1st IC contact?

# possible events
# SDIC + readmission (readmission before IC discharge)
# SDIC + readmission/death
# SDIC + death
# SDIC + 30 days
# SDIC ++ readmission (readmission after IC discharge)
# SDIC ++ readmission/death
# SDIC ++ death
# SDIC ++ 30 days

# SDIC + other dropout (moving OOA)
# SDIC + private admission (SPIRE etc)
# etc

glimpse(ic)
ic %>% 
  select(vars) %>%
  count(time_to_death < 30)

ic_surv <-
  ic %>% 
  select(
    vars,
    time_to_readmission = IC_Hosp_readmission_time,
    readmission30,
    readmission60,
    readmission90,
    time_to_death,
    death30,
    death60,
    death90
  ) %>% 
  mutate(
    time = case_when(
      is.na(time_to_death) & !is.na(time_to_readmission) ~ time_to_readmission,
      !is.na(time_to_death) & is.na(time_to_readmission) ~ time_to_death,
      # is.na(time_to_death) & is.na(time_to_readmission) ~ NA_integer_,
      time_to_death <= time_to_readmission ~ time_to_death,
      time_to_death > time_to_readmission ~ time_to_readmission,
      # !is.na(time_to_readmission) & (time_to_death >= time_to_readmission) ~ time_to_readmission,
      # is.na(time_to_readmission) & !is.na(time_to_death) ~ time_to_death,
      TRUE ~ time_to_readmission
    ),
    time30 = if_else(time > 30 | is.na(time), 30, time),
    status30 = if_else(time < 30 & !death30 != 1, 0, readmission30),
    time60 = if_else(time > 60 | is.na(time), 60, time),
    status60 = if_else(time < 60 & !death30 != 1, 0, readmission60),
    time90 = if_else(time > 90 | is.na(time), 90, time),
    status90 = if_else(time < 90 & !death30 != 1, 0, readmission90),
    age_strata = case_when(
      between(Age, 0, 74) ~ "0-74",
      between(Age, 75, 84) ~ "75-84",
      Age >= 85 ~ "85+",
      TRUE ~ NA_character_
    ),
    fi_strata = if_else(Frailty_index %in% c("0", "1", "2", "3", "4", "5"), 
                        "fi less than 5", 
                        "fi greater than 5")
  )

ic_surv %>% count(Frailty_index)
ic_surv %>% count(fi_strata)

ic_surv %>% 
  # count(age_strata)
  select(Age, age_strata) %>% 
  slice_sample(., prop = 0.01, by = "Age") %>% 
  View()

names(ic)
ic_surv %>% 
  select(starts_with("time"), starts_with("readmission"), 
         starts_with("death"), starts_with("status")
         ) %>% 
  filter(time_to_death < 90) %>%
  View()

ic_surv %>% 
  count(readmission30, readmission60, readmission90)

ic_surv %>% 
  select(REG_DATE_OF_DEATH, 
         starts_with("time"), 
         # starts_with("readmission"), 
         # starts_with("death"), 
         starts_with("status")
  ) %>% 
  # filter(!is.na(time_to_death)) %>%
  # filter(is.na(death30), is.na(death60), is.na(death90)) %>% 
  View()

ic_surv %>% 
  # names()
  # count(!is.na(REG_DATE_OF_DEATH), death30, death60, death90)
  # count(!is.na(REG_DATE_OF_DEATH), !is.na(time_to_death))
  # count(!is.na(time_to_death), death30, death60, death90)
  # count(!is.na(time_to_death), death30, death60, death90)
  count(death30, death60, death90)

View(ic_surv)

ic_surv %>% 
  count(time, status)
  View()

ic_surv %>%
  ggplot(aes(x = time, fill = as.character(status), group = as.character(status))) +
  # geom_density(alpha = 0.6) +
  geom_histogram() +
  facet_wrap(~status) +
  theme(legend.position = "top")

## surv --------------------------------------------------------------------
attach(ic_surv)    
fit.surv <- 
  survfit(Surv(time, status) ~ 1)
plot(fit.surv, xlab = "Days", ylab = "Estimated Probability of Survival")

# stratify by sex
fit.sex <- 
  survfit(Surv(time, status) ~ Sex)
plot(fit.sex, xlab = "Days", ylab = "Estimated Probability of Survival", col = c(2,4))
legend("bottomleft", levels(Sex), col = c(2,4), lty = 1)

# logrank
logrank.test <- survdiff(Surv(time, status) ~ Sex)

broom::tidy(logrank.test) %>% 
  write.xlsx(., 
             file = "local/survival/prelim_surv_km_sex_logrank_crosstab.xlsx", 
             asTable = TRUE)
broom::glance(logrank.test) %>% 
  write.xlsx(., 
             file = "local/survival/prelim_surv_km_sex_logrank_statistic.xlsx", 
             asTable = TRUE)

logrank.test$chisq[1]

## coxph -------------------------------------------------------------------
fit.cox <- coxph(Surv(time, status) ~ Sex)
summary(fit.cox)

broom::tidy(fit.cox) 
  write.xlsx(., 
             file = "local/survival/prelim_surv_coxph_sex_coeffs.xlsx", 
             asTable = TRUE)

broom::glance(fit.cox) %>% 
  pivot_longer(., cols = dplyr::everything(), names_to = "metric", values_to = "values") %>% 
  write.xlsx(., 
             file = "local/survival/prelim_surv_coxph_sex_summ.xlsx", 
             asTable = TRUE)

broom::augment(fit.cox)

# metrics
summary(fit.cox)$logtest[1]
summary(fit.cox)$waldtest[1]
summary(fit.cox)$sctest[1]
# sctest == chisq

# coxph add predictors
glimpse(ic_surv)
ic_surv <- 
  ic_surv %>% 
  mutate(
    Frailty_index = as.integer(Frailty_index),
    Comorbidities = as.integer(Comorbidities)
  )

# fit.all <- coxph(Surv(time, status) ~ Sex + Type + PoD + Age + HLOS)

fit.all <- 
  coxph(Surv(time, status) ~ GP_Count + IC_Count + Age + HLOS + IC_LOS + 
          IC_TimeToFirstContact + strata(Sex) + Frailty_index + Comorbidities + Type)
levels(Sex)

broom::tidy(fit.all, exp = TRUE) %>% 
  # pivot_longer(., cols = dplyr::everything(), names_to = "metric", values_to = "values") %>% 
  write.xlsx(., 
             file = "local/survival/prelim_surv_coxph_all_coeff.xlsx", 
             asTable = TRUE)

# broom::augment(fit.all)
broom::glance(fit.all) %>% 
  pivot_longer(., cols = dplyr::everything(), names_to = "metric", values_to = "values") %>% 
  write.xlsx(., 
             file = "local/survival/prelim_surv_coxph_all_summ.xlsx", 
             asTable = TRUE)

summary(fit.all)
summary(fit.all)$logtest[1]
summary(fit.all)$waldtest[1]
summary(fit.all)$sctest[1]

# preproc -----------------------------------------------------------------
# missingness
# impute mean
# factor levels


# multi -------------------------------------------------------------------
all_surv
ic_surv %>% names()

ic_surv30 <- 
  ic_surv %>%
  select(
    vars[-c(7:8, 14:15)],
    age_strata,
    fi_strata,
    time = time30,
    status = status30
    )
glimpse(ic_surv30)

ic_surv60 <- 
  ic_surv %>%
  select(
    vars[-c(7:8, 14:15)],
    age_strata,
    fi_strata,
    time = time60,
    status = status60
  )
glimpse(ic_surv60)

ic_surv90 <- 
  ic_surv %>%
  select(
    vars[-c(7:8, 14:15)],
    age_strata,
    fi_strata,
    time = time90,
    status = status90
  )
glimpse(ic_surv90)

# add strata and regressors
# cohort all to be removed
glimpse(ic_surv)

# strata
# sex
# age 0-75, 75-85 and 85+ 
# frailty < 5 and > 5

surv_models <- 
  tribble(
    ~cohort, ~survX, ~strata, ~func, ~models, ~data,
    'ic', 'surv30', 'sex_strata', 'survfit', Surv(time, status) ~ Sex, ic_surv30,
    'ic', 'surv30', 'sex_strata', 'survdiff', Surv(time, status) ~ Sex, ic_surv30,
    'ic', 'surv30', 'all_sex_strata1', 'coxph', Surv(time, status) ~ Sex + GP_Count + IC_Count + Age + 
      HLOS + IC_LOS + IC_TimeToFirstContact + Frailty_index + Comorbidities + Type, ic_surv30,
    'ic', 'surv30', 'all_sex_strata2', 'coxph', Surv(time, status) ~ Sex + GP_Count + IC_Count + Age + HLOS + 
      IC_LOS + IC_TimeToFirstContact + Frailty_index + Comorbidities + Type + strata(Sex), ic_surv30,
    'ic', 'surv60', 'sex_strata', 'survfit', Surv(time, status) ~ Sex, ic_surv60,
    'ic', 'surv60', 'sex_strata', 'survdiff', Surv(time, status) ~ Sex, ic_surv60,
    'ic', 'surv60', 'all_sex_strata1', 'coxph', Surv(time, status) ~ Sex + GP_Count + IC_Count + Age +
      HLOS + IC_LOS + IC_TimeToFirstContact + Frailty_index + Comorbidities + Type, ic_surv60,
    'ic', 'surv60', 'all_sex_strata2', 'coxph', Surv(time, status) ~ Sex + GP_Count + IC_Count + Age + HLOS +
      IC_LOS + IC_TimeToFirstContact + Frailty_index + Comorbidities + Type + strata(Sex), ic_surv60,
    'ic', 'surv90', 'sex_strata', 'survfit', Surv(time, status) ~ Sex, ic_surv90,
    'ic', 'surv90', 'sex_strata', 'survdiff', Surv(time, status) ~ Sex, ic_surv90,
    'ic', 'surv90', 'all_sex_strata1', 'coxph', Surv(time, status) ~ Sex + GP_Count + IC_Count + Age + 
      HLOS + IC_LOS + IC_TimeToFirstContact + Frailty_index + Comorbidities + Type, ic_surv90,
    'ic', 'surv90', 'all_sex_strata2', 'coxph', Surv(time, status) ~ Sex + GP_Count + IC_Count + Age + HLOS + 
      IC_LOS + IC_TimeToFirstContact + Frailty_index + Comorbidities + Type + strata(Sex), ic_surv90,
    'ic', 'surv90', 'age_strata', 'survfit', Surv(time, status) ~ age_strata, ic_surv90,
    'ic', 'surv90', 'age_strata', 'survdiff', Surv(time, status) ~ age_strata, ic_surv90,
    'ic', 'surv90', 'all_age_strata1', 'coxph', Surv(time, status) ~ Sex + GP_Count + IC_Count + age_strata + 
      HLOS + IC_LOS + IC_TimeToFirstContact + Frailty_index + Comorbidities + Type, ic_surv90,
    'ic', 'surv90', 'all_age_strata2', 'coxph', Surv(time, status) ~ Sex + GP_Count + IC_Count + Age + HLOS + 
      IC_LOS + IC_TimeToFirstContact + Frailty_index + Comorbidities + Type + strata(age_strata), ic_surv90,
    'ic', 'surv90', 'fi_strata', 'survfit', Surv(time, status) ~ Sex, ic_surv90,
    'ic', 'surv90', 'fi_strata', 'survdiff', Surv(time, status) ~ Sex, ic_surv90,
    'ic', 'surv90', 'all_fi_strata1', 'coxph', Surv(time, status) ~ Sex + GP_Count + IC_Count + Age + 
      HLOS + IC_LOS + IC_TimeToFirstContact + fi_strata + Comorbidities + Type, ic_surv90,
    'ic', 'surv90', 'all_fi_strata2', 'coxph', Surv(time, status) ~ Sex + GP_Count + IC_Count + Age + HLOS + 
      IC_LOS + IC_TimeToFirstContact + Frailty_index + Comorbidities + Type + strata(fi_strata), ic_surv90
    ) %>% 
  mutate(
    params = pmap(list(models, data), list),
    fit = invoke_map(func, params),
    # summ = map(fit, broom::tidy),
    # coeffs = map(fit, broom::glance),
    filename = paste0(cohort, "_", survX, "_", strata, "_", func)
    )

surv_models_coxph <- 
  surv_models %>% 
  filter(func == "coxph") %>% 
  mutate(
    summ = map(.x = fit, 
               .f = ~broom::glance(.x) %>% 
                 pivot_longer(., 
                              cols = dplyr::everything(), 
                              names_to = "metric", values_to = "values")
    ),
    coeffs = map(.x = fit, .f = ~broom::tidy(.x, exp = T, conf.int = T)),
    zph = map(.x = fit, 
               .f = ~cox.zph(.x) 
              ),
    pzph = map(.x = fit, 
              .f = ~cox.zph(.x) %>% 
                survminer::ggcoxzph(., df = 3)
              ),
    diag = map(.x = fit,
               .f = ~survminer::ggcoxdiagnostics(.x,
                 type = "schoenfeld",
                 ox.scale = "observation.id")
               )
    )

surv_models_coxph$zph[[1]]
surv_models_coxph$pzph[[1]]
surv_models_coxph$diag[[1]]

survfit_models <- 
  surv_models %>% 
  filter(func == "survfit") %>% 
  mutate(
    km_plot = map(.x = fit, 
                  .f = ~survminer::ggsurvplot(fit = .x, 
                                            # data = ic_surv90,
                                            conf.int = T,
                                            pval = T, 
                                            ggtheme = theme_bw(),
                                            risk.table = F)
                  )
    )

survfit_models$km_plot[[1]]

survdiff_models <- 
  surv_models %>% 
  filter(func == "survdiff") %>% 
  mutate(
      logrank_summ = map(.x = fit, 
                 .f = ~broom::glance(.x) %>% 
                   pivot_longer(., 
                                cols = dplyr::everything(), 
                                names_to = "metric", values_to = "values")
      ),
      logrank_coeffs = map(.x = fit, .f = ~broom::tidy(.x, exp = T, conf.int = T))
    )

surv_models_coxph_summ <- 
  surv_models_coxph %>% 
  select(filename, summ) %>% 
  unnest(., summ) %>% 
  pivot_wider(., names_from = "metric", values_from = "values")

surv_models_coxph_coeffs <- 
  surv_models_coxph %>% 
  select(filename, coeffs) %>% 
  unnest(., coeffs)
  # View()

survdiff_models_logrank_summ <- 
  survdiff_models %>%   
  select(filename, logrank_summ) %>% 
  unnest(., logrank_summ) %>% 
  pivot_wider(., names_from = "metric", values_from = "values")

survdiff_models_logrank_coeffs <- 
  survdiff_models %>% 
  select(filename, logrank_coeffs) %>% 
  unnest(., logrank_coeffs)
# View()

surv_model_info <- 
  list(
    coxph_summ = surv_models_coxph_summ,
    coxph_coeffs = surv_models_coxph_coeffs,
    logrank_summ = survdiff_models_logrank_summ,
    logrank_coeffs = survdiff_models_logrank_coeffs
  )

saveRDS(surv_model_info, "local/survival/surv_model_info.rds")

write.xlsx(surv_model_info, file = "local/survival/surv_model_info.xlsx", asTable = T)
ggpubr::ggarrange(survfit_models)
saveRDS(survfit_models, "local/survival/survfit_models.rds")


# p1 <- 
#   ggpubr::ggarrange(survfit_models$km_plot, nrow = 1, common.legend = T)

# survfit_models$km_plot[[1]] %>% as_ggplot()
# cowplot::plot_grid(plotlist = survfit_models$km_plot)
# survfit_models$km_plot[[1]] %>% as_ggplot()
# survfit_models$km_plot[[3]]


# report / appx --------------------------------------------------------------------
survX_sex_strata_models <- 
  surv_models %>% 
  filter(str_detect(strata, pattern = "sex_strata2")) %>% 
  mutate(
    summ = map(.x = fit, 
                       .f = ~broom::glance(.x) %>% 
                         pivot_longer(., 
                                      cols = dplyr::everything(), 
                                      names_to = "metric", values_to = "values")
    ),
    coeffs = map(.x = fit, .f = ~broom::tidy(.x, exp = T, conf.int = T))
  )

survX_sex_strata_models_summ <-
  survX_sex_strata_models %>%
  select(survX, summ) %>% 
  unnest(., summ) %>% 
  pivot_wider(., names_from = "metric", values_from = "values") %>% 
  select(survX, statistic.log, statistic.wald, concordance, starts_with("r"), AIC, BIC)
  
survX_sex_strata_models_coeffs <-
  survX_sex_strata_models %>%
  select(survX, coeffs) %>% 
  unnest(., coeffs) %>% 
  select(survX, term, estimate) %>%  
  pivot_wider(., names_from = "survX", values_from = "estimate") %>% 
  mutate(
    pct_diff60 = ((surv60 - surv30)/surv30)*100,
    pct_diff90 = ((surv90 - surv30)/surv30)*100 
  )  

survX_model_info <- 
  list(
    model_summ = survX_sex_strata_models_summ,
    model_coeffs = survX_sex_strata_models_coeffs
)

write.xlsx(survX_model_info, "local/survival/survX_model_info.xlsx", asTable = T)

# km
p1 <- 
  survminer::ggsurvplot(fit = surv_models$fit[[1]], 
                      data = ic_surv30,
                      conf.int = T,
                      pval = T, 
                      ggtheme = theme_bw(),
                      risk.table = F)

# diag
surv_models
p2 <- 
  surv_models$fit[[4]] %>% 
  survminer::ggcoxdiagnostics(
    type = "schoenfeld",
    ox.scale = "observation.id", 
  )



# plots -------------------------------------------------------------------
survminer::ggsurvplot(fit = surv_models$fit[[5]], 
                      data = ic_surv90,
                      conf.int = T,
                      pval = T, 
                      ggtheme = theme_bw(),
                      risk.table = F)

survminer::ggadjustedcurves(fit = surv_models$fit[[4]], method = "average")
survminer::ggadjustedcurves()

surv_models$fit[[3]] %>% 
  broom::glance()

surv_models$fit[[7]] %>% 
  broom::tidy(exp = T, conf.int = T)

surv_models$fit[[8]] %>% 
  broom::tidy(exp = T, conf.int = T)

cox.zph(surv_models$fit[[3]])

cox.zph(surv_models$fit[[3]]) %>% 
  survminer::ggcoxzph(., df = 3)

surv_models$fit[[7]] %>% 
  survminer::ggcoxdiagnostics(
    type = "deviance",
    ox.scale = "linear.predictions"
  )

surv_models$fit[[7]] %>% 
  survminer::ggcoxdiagnostics(
    type = "schoenfeld",
    ox.scale = "observation.id", 
  )


## to do
# preproc - missingness / dummy vars / impute mean
# add strata / regressors
# km plots
# coxph adj plots
# add ci
# add p vals
# diagnostics

ic_surv90 %>% 
  filter(time < 90 & status == 1) %>% 
  View()


# save --------------------------------------------------------------------
save(list = ls(), file = ".RData")

# end ---------------------------------------------------------------------

ic_my_surv<-ic %>%
  group_by(Patient_ID) %>%
  slice(1) %>%
  ungroup()

ic_my_surv <- ic_my_surv %>%
  select(Patient_ID, Sex, IC_Hosp_readmission_time, time_to_death) %>%
  mutate(
  time_to_readmission = ifelse(is.na(IC_Hosp_readmission_time) |  IC_Hosp_readmission_time> 30, 31, IC_Hosp_readmission_time),
# Code time to death as NA if it is less than 30 days
 time_to_death = ifelse(is.na(time_to_death) | time_to_death>30,31, time_to_death),
# Combine time to event and event indicator (1 for death, 0 for readmission)
event_time = pmin(time_to_readmission, time_to_death),
event = ifelse(!is.na(IC_Hosp_readmission_time) & IC_Hosp_readmission_time<=30, 1,0),
# Calculate time to censoring (time to minimum of readmission and death)
) %>%
  # Select relevant columns
  select(Patient_ID,time_to_readmission, time_to_death, event_time, event)

library(ggsurvfit)
# Fit Cox proportional hazards regression model
cox_model <- coxph(Surv(event_time, event) ~ ., data = ic_my_surv)

surv_object_by_sex <- survfit(cox_model, strata = ic_my_surv$sex)
plot(surv_object_by_sex, col = c("blue", "red"), main = "Survival Curves by Sex", xlab = "Time", ylab = "Survival Probability")
legend("topright", legend = levels(ic_my_surv$sex), col = c("blue", "red"), lty = 1)

# Assess the model
summary(cox_model)

# Make predictions
# For example, predict survival probabilities at certain time points
new_data <- data.frame(patient_id = c("patient1", "patient2"), event_time = c(100, 200))
predict(cox_model, newdata = new_data, type = "survival")
