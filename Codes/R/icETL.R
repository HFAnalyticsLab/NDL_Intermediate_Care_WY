# etl from AP SQL query

# setup -------------------------------------------------------------------
source("icSetup.R")
# rm(list = ls())
# gc()

# memory ------------------------------------------------------------------
# memory.limit()
# memory.profile()
# memory.size()

# etl ---------------------------------------------------------------------
# see etl from icACap.R / icSDICap.R for acute care records
ap <- 
  get_query(read_file(file = "sql/HOSP_SD_IC_Activ_AB.sql")) %>% 
  as_tibble() %>%
  janitor::clean_names()

ap_df <- 
  ap %>%
  select(-age_band_fw) %>%
  rename(pod = po_d,
         pod_desc = po_d_desc,
         ic_type = type,
         ic_group = group) %>% 
  mutate(
    across(.cols = contains("date"), .fn = ~ymd(.)),
    ltc_dementia = as.character(ltc_dementia),
    ethnic_group = str_trim(ethnic_group, "right")
  )

ap_df1 <- 
  ap_df %>%
  # dim()
  filter(attendance_date >= ymd("2022-04-01") &
           attendance_date <= ymd("2023-09-30")
  ) %>%
  filter(
    (ic_attendance_date >= ymd("2021-09-01") & ic_attendance_date <= ymd("2023-09-30") |
       is.na(ic_attendance_date))
  ) %>%
  # filter(phm_segment != "Maternity") %>% 
  # dim()
  mutate(
    ic_sd = if_else(
      (is.na(ic_type) | 
         ic_group %in% c("Childrens Services", 
                         "Adult Dietetics", 
                         "Adult Domiciliary Physiotherapy",
                         "Adult Speech and Language Therapy", 
                         "Childrens Mental Health", 
                         "Community Falls Service", 
                         "Community Neurology", 
                         "LTCs Management - Cardiac",
                         "LTCs Management - Cardiac Rehabilitation", 
                         "LTCs Management - Diabetes",
                         "LTCs Management - Diabetes X-PERT",
                         "LTCs Management - Home Oxygen Service",
                         "LTCs Management - Pulmonary Rehabilitation",
                         "MSK", 
                         "Other Specialist Services", 
                         "Podiatry",
                         "Primary Care Mental Health IAPT Partnership", 
                         "Psychological Support Service", 
                         "Specialist Nursing",
                         "Specialist Nursing - Continence",
                         "Specialist Nursing - Tissue Viability", 
                         "TB Service")
      ), "none", "sdic")
  ) %>% 
  mutate(
    ic_pathway = 
      case_when(
        is.na(ic_type) ~ "0",
        ic_type == "CCB" ~ "2",
        !(ic_type == "CCB" & is.na(ic_type)) ~ "1",
        TRUE ~ NA_character_
      )
  )

ap_df2 <- 
  ap_df1 %>%
  rename(
    imd_decile = imd_decile_new) %>% 
  mutate(
    imd_quintile =
      case_when(
        between(imd_decile, 1, 2) ~ 1,
        between(imd_decile, 3, 4) ~ 2,
        between(imd_decile, 5, 6) ~ 3,
        between(imd_decile, 7, 8) ~ 4,
        between(imd_decile, 9, 10) ~ 5,
        TRUE ~ NA_real_)
  ) %>% 
  mutate(
    overall = "overall",
    imd_decile = fct_explicit_na(f = factor(x = imd_decile, ordered = F), na_level = "Unknown"),
    imd_quintile = fct_explicit_na(f = factor(x = imd_quintile, ordered = F), na_level = "Unknown")
  ) %>%
  # mutate(.data = ., across(.cols = c(where(is.numeric), -age),
  #                          .fns = as.character)
  # ) %>%
  mutate(.data = ., across(.cols = where(is.character),
                           .fns = ~fct_explicit_na(f = ., na_level = "Unknown"))
  ) 

ap_df3 <- 
  ap_df2 %>% 
  mutate(
    sex_desc = if_else(sex == "2", "Female", "Male") %>% factor(.)
  ) %>% 
  # dim()
  nest(., dat = -c(patient_id, attendance_date)) %>%
  # dim()
  rownames_to_column(.data = ., var = "spell_id") %>% 
  unnest(cols = dat) %>% 
  mutate(
    spell_id = factor(spell_id)
  )

## ac ----------------------------------------------------------------------
ac <-
  ap_df3 %>% 
  relocate(., c(imd_quintile, overall), .after = imd_decile) %>% 
  select(spell_id:ltc_count) %>% 
  mutate(spell_id = factor(spell_id))

rm(list = ls(pattern = "^ap"))
gc()

## ae and op ----------------------------------------------------------------------
# to be added

## ad -------------------------------------------------------------------
ad <-
  get_query(read_file(file = "sql/ic_MS_ActivityData.sql")) %>%
  as_tibble() %>%
  janitor::remove_empty("cols") %>% 
  janitor::clean_names()

# check duplicates
# ad_dupes <- 
#   ad %>% 
#   filter(!is.na(patient_id)) %>% 
#   janitor::get_dupes() %>% 
#   arrange(desc(dupe_count)) %>% 
#   count(patient_id, dupe_count, sort = F)
# 
# saveRDS(ad_dupes, file = "local/interim/ad_dupes.rds")


## cd ----------------------------------------------------------------------

# eda ---------------------------------------------------------------------
# skimr::skim_without_charts(ad)
# skimr::skim_without_charts(ac)

# skim_ac <-
#   ac %>%
#   skimr::skim_without_charts()

# saveRDS(skim_ac, file = "local/interim/skim_ac.rds")
# rm(skim_ac)

# skim_ad <- 
#   ad %>% 
# filter(patient_id %in% ac$patient_id) %>%
# distinct() %>% 
# dim()
# skimr::skim_without_charts()

# saveRDS(skim_ad, file = "local/interim/skim_ad.rds")
# rm(skim_ad)

# format ad ------------------------------------------------------------------
# remove duplicates
# discharge date before attendance date etc
# -ve length of stay  
# change record classification == unknowns to NAs
# change type == NA to ASC  

ad_df <-
  ad %>% 
  # slice_sample(., prop = 0.1, by = "type") %>%
  mutate(
    across(.cols = contains("date"), .fn = ~ymd(as.Date(.))),
    discharge_date = 
      if_else(
        discharge_date < attendance_date, attendance_date, discharge_date
      ),
    record_classification = 
      if_else(
        record_classification == "Unknown", NA_character_, record_classification
      ),
    length_of_stay = as.integer(discharge_date-attendance_date)
  ) %>% 
  relocate(referal_date, .before = attendance_date) %>% 
  replace_na(., list(type = "ASC")) %>% 
  distinct()

# filter
# df_ap <- 
#   df %>%
#   # dim()
#   filter(attendance_date >= ymd("2022-04-01") &
#            attendance_date <= ymd("2023-09-30")
#   ) %>%
#   filter(
#     (ic_attendance_date >= ymd("2021-09-01") & ic_attendance_date <= ymd("2023-09-30") |
#        is.na(ic_attendance_date))
#   ) %>%
#   # filter(phm_segment != "Maternity") %>% 
#   # dim()
#   mutate(
#     ic_sd = if_else(
#       (is.na(ic_type) | 
#          ic_group %in% c("Childrens Services", 
#                          "Adult Dietetics", 
#                          "Adult Domiciliary Physiotherapy",
#                          "Adult Speech and Language Therapy", 
#                          "Childrens Mental Health", 
#                          "Community Falls Service", 
#                          "Community Neurology", 
#                          "LTCs Management - Cardiac",
#                          "LTCs Management - Cardiac Rehabilitation", 
#                          "LTCs Management - Diabetes",
#                          "LTCs Management - Diabetes X-PERT",
#                          "LTCs Management - Home Oxygen Service",
#                          "LTCs Management - Pulmonary Rehabilitation",
#                          "MSK", 
#                          "Other Specialist Services", 
#                          "Podiatry",
#                          "Primary Care Mental Health IAPT Partnership", 
#                          "Psychological Support Service", 
#                          "Specialist Nursing",
#                          "Specialist Nursing - Continence",
#                          "Specialist Nursing - Tissue Viability", 
#                          "TB Service")
#       ), "none", "sdic")
#   ) %>% 
#   mutate(
#     ic_pathway = 
#       case_when(
#         is.na(ic_type) ~ "0",
#         ic_type == "CCB" ~ "2",
#         !(ic_type == "CCB" & is.na(ic_type)) ~ "1",
#         TRUE ~ NA_character_
#       )
#   )
# 
# df_ap1 <- 
#   df_ap %>%
#   rename(
#     imd_decile = imd_decile_new) %>% 
#   mutate(
#     imd_quintile =
#       case_when(
#         between(imd_decile, 1, 2) ~ 1,
#         between(imd_decile, 3, 4) ~ 2,
#         between(imd_decile, 5, 6) ~ 3,
#         between(imd_decile, 7, 8) ~ 4,
#         between(imd_decile, 9, 10) ~ 5,
#         TRUE ~ NA_real_)
#   ) %>% 
#   mutate(
#     overall = "overall",
#     imd_decile = fct_explicit_na(f = factor(x = imd_decile, ordered = F), na_level = "Unknown"),
#     imd_quintile = fct_explicit_na(f = factor(x = imd_quintile, ordered = F), na_level = "Unknown")
#   ) %>%
#   # mutate(.data = ., across(.cols = c(where(is.numeric), -age),
#   #                          .fns = as.character)
#   # ) %>%
#   mutate(.data = ., across(.cols = where(is.character),
#                            .fns = ~fct_explicit_na(f = ., na_level = "Unknown"))
#   ) 

# ac prep ----------------------------------------------------------
ac_df <- ac

ac_df1 <-
  ac_df %>% 
  # slice_head(., n = 1000) %>%
  rename(los = ac_los) %>% 
  rename_with(., .fn = ~paste0("ac_", .), -c(patient_id, spell_id)) %>% 
  drop_na(patient_id) %>% 
  distinct() %>% 
  nest(.data = ., ac_df = -c(patient_id, spell_id,
                             ac_attendance_date, ac_discharge_date,
                             ac_record_classification
  )
  )

# ad (other) prep -----------------------------------------------------------------
ad_df1 <- ad_df

# nest ad --------------------------------------------------------------
ad_df2 <-
  ad_df1 %>% 
  # slice_head(n = 1000) %>%
  select(patient_id,
         attendance_date,
         discharge_date,
         activity_type = type,
         activity_group = group,
         contact_type = record_classification,
         length_of_stay
  ) %>% 
  group_by(patient_id) %>%
  nest(other = -patient_id) %>%
  mutate(n_other = map_int(other, ~nrow(.))) %>%
  arrange(desc(n_other))

# join ac ad -----------------------------------------------------------
ac_df2 <- 
  ac_df1

# year(ac_ad$ac_attendance_date[1]) %>% class()
# month(ac_ad$ac_attendance_date[1]) %>% class()

ac_ad <-
  left_join(
    ac_df2 %>% select(-ac_df),
    ad_df2,
    by = "patient_id"
    # suffix = c(".ac", ".ad"),
  ) %>% 
  # slice_head(., prop = 0.25) %>% 
  # filter((year(ac_attendance_date) == 2022 &
  #          month(ac_attendance_date) %in% c(6,7)) |
  #          (year(ac_attendance_date) == 2023 &
  #             month(ac_attendance_date) %in% c(3,4))
  #        ) %>% 
  # count(year(ac_attendance_date),
  #        month(ac_attendance_date))
  unnest(cols = other, names_sep = "_") %>%
  group_by(patient_id, spell_id, 
           ac_attendance_date, ac_discharge_date,
           ac_record_classification, n_other) %>%
  nest(other = -c(patient_id, spell_id, 
                  ac_attendance_date, ac_discharge_date,
                  ac_record_classification, n_other),
       .names_sep = "_") %>%
  mutate(
    other = map(.x = other,
                .f = ~mutate(.x,
                             days_from_ac_discharge = attendance_date - ac_discharge_date))
  )


# save --------------------------------------------------------------------
saveRDS(object = ac_ad, file = "local/interim/ac_ad.rds")


# end ---------------------------------------------------------------------


