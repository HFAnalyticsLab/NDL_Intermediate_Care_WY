## scripted from icACtoAD.R for aggregation of SU and SD care

# setup -------------------------------------------------------------------
source("icSetup.R")
# rm(list = ls())
# gc()

# memory ------------------------------------------------------------------
# memory.limit()
# memory.profile()
# memory.size()

# etl ---------------------------------------------------------------------
# see icETL.R for preprocessing / format
ac_ad <- 
  readRDS(file = "local/interim/ac_ad.rds")
# glimpse(ac_ad)

# format -----------------------------------------------------------
# lvls_ad_ac <-
#   ac_ad %>% 
#   ungroup() %>% 
#   slice_sample(., prop = 0.25) %>%
#   select(patient_id, other) %>% 
#   unnest(., other)  

# setup lvls
lvls_activity_type <-
    c("ASC",
      "ASCCOM",
      "CBS",
      "CCB",
      "GP",
      "UC111",
      "UC999",
      "UCOOH"
      )
  # lvls_ad_ac %>%
  # slice_sample(., prop = 0.05, by = activity_type) %>%
  # count(activity_type) %>%
  # pull(activity_type)

lvls_time_band <- 
  c("prior_4wks",
    "prior_0_4wks",
    "post_0_4wks",
    "post_4_6wks",
    "post_6_8wks",
    "post_8_10_wks",
    "post_10_wks"
  )

# other -------------------------------------------------------------------
ac_ad_out <- function(d0, d1, file_suffix){
  ac_ad %>%
    ungroup() %>%
    filter(ac_attendance_date >= d0 &
             ac_attendance_date <= d1) %>% 
    # slice_head(., n = 1000) %>%
    mutate(
      other = furrr::future_map(
        .x = other,
        .f = ~mutate(.x,
                     days_from_ac_discharge = as.integer(days_from_ac_discharge),
                     time_band = case_when(
                       days_from_ac_discharge < -4*7 ~ "prior_4wks",
                       between(days_from_ac_discharge, -4*7, 0) ~ "prior_0_4wks",
                       between(days_from_ac_discharge, 0, 4*7) ~ "post_0_4wks",
                       between(days_from_ac_discharge, 4*7, 6*7) ~ "post_4_6wks",
                       between(days_from_ac_discharge, 6*7, 8*7) ~ "post_6_8wks",
                       between(days_from_ac_discharge, 8*7, 10*7) ~ "post_8_10_wks",
                       days_from_ac_discharge > 10*7 ~ "post_10_wks",
                       TRUE ~ NA_character_
                     ),
                     activity_type = factor(activity_type, levels = lvls_activity_type),
                     time_band = factor(time_band, levels = lvls_time_band)
        ), 
        .progress = F  
      ),
      post_1 = furrr::future_map(
        .x = other,
        .f = ~filter(.x, days_from_ac_discharge >= 0) %>% 
          slice_min(., order_by = days_from_ac_discharge, with_ties = F) %>%
          select(activity_type:time_band),
        .progress = F  
      ),
      prior_1 = furrr::future_map(
        .x = other,
        .f = ~filter(.x, days_from_ac_discharge < 0) %>% 
          slice_max(., order_by = days_from_ac_discharge, with_ties = F) %>%
          select(activity_type:time_band),
        .progress = F  
      ),
      post_n = furrr::future_map(.x = other,
                                 .f = ~filter(.x, days_from_ac_discharge > 0) %>%
                                   group_by(., activity_type, .drop = F) %>%
                                   summarise(
                                     n = n()
                                   ) %>% 
                                   pivot_wider(., names_from = "activity_type", values_from = "n"),
                                 .progress = F  
      ),
      prior_n = furrr::future_map(.x = other,
                                  .f = ~filter(.x, days_from_ac_discharge < 0) %>%
                                    group_by(., activity_type, .drop = F) %>%
                                    summarise(
                                      n = n()
                                    ) %>% 
                                    pivot_wider(., names_from = "activity_type", values_from = "n"),
                                  .progress = F  
      ),
      tb_act_type = furrr::future_map(.x = other,
                                      .f = ~group_by(.x, time_band, activity_type, .drop = F) %>%
                                        summarise(
                                          n = n(), 
                                          .groups = "keep"
                                        ),
                                      .progress = F  
      ),
      ic = furrr::future_map(.x = other,
                             .f = ~group_by(.x, time_band, activity_type, .drop = F) %>%
                               summarise(
                                 n = n(), 
                                 .groups = "keep"
                               ) %>%
                               filter(
                                 (time_band == "prior_0_4wks" & activity_type == "CBS") |
                                   (time_band == "post_0_4wks" & activity_type == "CBS")
                               ) %>%
                               ungroup() %>% 
                               pivot_wider(., names_from = c("time_band", "activity_type"), 
                                           values_from = "n") %>% 
                               mutate(
                                 continuity_CBS = if_else(
                                   prior_0_4wks_CBS > 0 & post_0_4wks_CBS > 0,
                                   TRUE,
                                   FALSE
                                 )
                               ),
                             .progress = F  
      )
    ) %>% 
    select(-other) %>% 
    saveRDS(., 
            file = paste0("local/interim/ac_ad_df5", 
                          file_suffix, 
                          ".rds")
    )
  gc()
}

# nt ----------------------------------------------------------------------
ac_nt_out <- function(d0, d1, file_suffix){
  ac_ad %>%
    ungroup() %>%
    filter(ac_attendance_date >= d0 &
             ac_attendance_date <= d1) %>% 
    # slice_head(., n = 1000) %>%
    mutate(
      nt = furrr::future_map(
        .x = other,
        .f = ~mutate(.x,
                       days_from_ac_discharge = as.integer(days_from_ac_discharge),
                     time_band = case_when(
                       days_from_ac_discharge < -4*7 ~ "prior_4wks",
                       between(days_from_ac_discharge, -4*7, -1) ~ "prior_0_4wks",
                       between(days_from_ac_discharge, 0, 4*7) ~ "post_0_4wks",
                       between(days_from_ac_discharge, 4*7+1, 6*7) ~ "post_4_6wks",
                       between(days_from_ac_discharge, 6*7+1, 8*7) ~ "post_6_8wks",
                       between(days_from_ac_discharge, 8*7+1, 10*7) ~ "post_8_10_wks",
                       days_from_ac_discharge > 10*7 ~ "post_10_wks",
                       TRUE ~ NA_character_
                     ),
                     post_prior = if_else(days_from_ac_discharge >= 0, "post", "prior"),
                     post_prior = factor(post_prior, levels = c("post", "prior")),
                     activity_type = factor(activity_type, levels = lvls_activity_type),
                     time_band = factor(time_band, levels = lvls_time_band)
        ) %>% 
          filter(str_detect(activity_type, "CBS") &
                   str_detect(activity_group, "Neighbourhood")
        ), 
        .progress = F  
      ),
      # nt = furrr::future_map(
      #   .x = other,
      #   .f = ~filter(.x,  
      #                str_detect(activity_type, "CBS") &
      #                  str_detect(activity_group, "Neighbourhood")
      #   ),
      #   .progress = F  
      # ),
      nt_n = furrr::future_map_int(
        .x = nt,
        .f = ~nrow(.x)
      ),
      nt_los = furrr::future_map(
        .x = nt,
        .f = ~filter(.x,
                     str_detect(activity_type, "CBS") &
                       str_detect(activity_group, "Neighbourhood")
        ) %>%
          group_by(
            post_prior, .drop = F
          ) %>% 
          summarise(
            nt_los = as.integer(max(attendance_date)-min(attendance_date))
          ) %>% 
          pivot_wider(., names_from = post_prior, values_from = nt_los) %>% 
          mutate(
            total = post + prior
          )
      ),
      post_nt1 = furrr::future_map(
        .x = nt,
        .f = ~filter(.x, days_from_ac_discharge >= 0) %>% 
          slice_min(., order_by = days_from_ac_discharge, with_ties = F) %>%
          select(activity_type:time_band, -length_of_stay),
        .progress = F  
      ),
      prior_nt1 = furrr::future_map(
        .x = nt,
        .f = ~filter(.x, days_from_ac_discharge < 0) %>%
          slice_max(., order_by = days_from_ac_discharge, with_ties = F) %>%
          select(activity_type:time_band, -length_of_stay),
        .progress = F
      ),
      post_nt_n = furrr::future_map(
        .x = nt,
        .f = ~filter(.x, days_from_ac_discharge >= 0) %>%
          group_by(., activity_type, .drop = T) %>%
          summarise(
            n = n()
          ) %>% 
          pivot_wider(., names_from = "activity_type", values_from = "n"),
        .progress = F  
      ),
      prior_nt_n = furrr::future_map(
        .x = nt,
        .f = ~filter(.x, days_from_ac_discharge < 0) %>%
          group_by(., activity_type, .drop = T) %>%
          summarise(
            n = n()
          ) %>% 
          pivot_wider(., names_from = "activity_type", values_from = "n"),
        .progress = F  
      ),
      tb_nt = furrr::future_map(.x = nt,
                                .f = ~group_by(.x, time_band, activity_group, .drop = F) %>%
                                  summarise(
                                    n = n(), 
                                    .groups = "keep"
                                  ),
                                .progress = F  
      ),
      ic_nt = furrr::future_map(.x = nt,
                                .f = ~group_by(.x, time_band, activity_type, .drop = F) %>%
                                  summarise(
                                    n = n(), 
                                    .groups = "keep"
                                  ) %>%
                                  filter(
                                    (time_band == "prior_0_4wks" & activity_type == "CBS") |
                                      (time_band == "post_0_4wks" & activity_type == "CBS")
                                  ) %>%
                                  ungroup() %>% 
                                  pivot_wider(., names_from = c("time_band", "activity_type"), 
                                              values_from = "n") %>% 
                                  mutate(
                                    continuity_nt = if_else(
                                      prior_0_4wks_CBS > 0 & post_0_4wks_CBS > 0,
                                      TRUE,
                                      FALSE
                                    )
                                  ), 
                                .progress = T
      )
    ) %>% 
    select(-nt) %>% 
    saveRDS(., 
            file = paste0("local/interim/ac_nt_df5", 
                          file_suffix, 
                          ".rds")
    )
  gc()
}  

# chunk -------------------------------------------------------------------
s <- 
  tibble(
    d0 = c(
      # ymd("2022-04-01")
      # ymd("2022-05-01")
      # ymd("2022-06-01")
      # ymd("2022-07-01"),
      # ymd("2022-08-01"),
      # ymd("2022-09-01")
      # ymd("2022-10-01")
      # ymd("2022-11-01")
      # ymd("2022-12-01")
      # ymd("2023-01-01")
      # ymd("2023-02-01")
      # ymd("2023-03-01")
      # ymd("2023-04-01")
      # ymd("2023-05-01")
      ymd("2023-06-01")
      # ymd("2023-07-01")
      # ymd("2023-08-01")
      # ymd("2023-09-01")
    ),
    d1 = c(
      # ymd("2022-05-31")
      # ymd("2022-06-30")
      # ymd("2022-07-31")
      # ymd("2022-08-31"),
      # ymd("2022-09-30"),
      # ymd("2022-10-31")
      # ymd("2022-11-30")
      # ymd("2022-12-31")
      # ymd("2023-01-31")
      # ymd("2023-02-28")
      # ymd("2023-03-31")
      # ymd("2023-04-30")
      # ymd("2023-05-31")
      ymd("2023-06-30")
      # ymd("2023-07-31")
      # ymd("2023-08-31")
      # ymd("2023-09-30")
    ),
    file_suffix = letters[20]
    # 17 
  ) 

# run ---------------------------------------------------------------------
plan(multisession, workers = 4)

# pmap(.l = s, .f = ac_ad_out) 
pmap(.l = s, .f = ac_nt_out)

rm(list = ls())
gc(verbose = F, full = T)  
future::plan(sequential)

# end ---------------------------------------------------------------------





