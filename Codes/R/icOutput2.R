library(Amelia)
library(tidyverse)
library(scales)
library(ggplot2)
library(ggmosaic)


library(ggridges)


library(rmapshaper)
library(sf)
#library(maptools)
library(classInt)
library(RColorBrewer)
library(tmap)
library(arsenal) 
library(gtsummary)

library(readr)
library(networkD3)
library(mice)

# setup -------------------------------------------------------------------
source("icSetup.R")
source("Hospital_IC_Continuity_AB_src.R")


all <- 
  final1 %>% 
  # filter(
  #  SDIC == 1 &
  #   (Attendance_Date <= IC_Attendance_Date)
  #) %>%
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
           hosp_next_date,hosp_next_pod,hosp_time_gap) %>%summarise(IC_Discharge_Date= max(IC_Discharge_Date),
                                                                    IC_Attendance_Date=min(IC_Attendance_Date), 
                                                                    IC_Contact_Nr=sum(IC_Contact_Nr),
                                                                    Type=paste(sort(unique(Type)), collapse = "-"),
                                                                    Group=paste(unique(Group), collapse = "-"),
                                                                    IC_Continuity=sum(IC_continuity),
                                                                    SDIC=max(SDIC)) %>%
  mutate(HLOS = as.numeric((difftime(Discharge_Date, Attendance_Date, units='day') )),
         IC_LOS  = as.numeric((difftime(IC_Discharge_Date , IC_Attendance_Date, units='day') )),
         IC_TimeToFirstContact =as.numeric((difftime( IC_Attendance_Date, Discharge_Date, units='day'))),
         IC_Hosp_readmission_time=as.numeric((difftime(hosp_next_date, IC_Discharge_Date , units='day') )),
         Type =ifelse(grepl("CCB", Type), "CCB", Type)) %>%ungroup()%>%
  
  select(
    Patient_ID,
    GP_Count, 
    UC111_Count, 
    UC999_Count,
    UCOOH_Count,
    ASCCOM_Count,
    ComServ_Count,
    IC_Count, 
    AE_Count,
    Last_GP_Event,
    Last_UC111_Event,
    Last_UC999_Event,
    Last_UCOOH_Event,
    Last_ASCCOM_Event,
    Last_ComServ_Event,
    Last_IC_Event,
    Last_AE_Event,
    Age,
    Sex = Sex_Desc,
    Age_group = Age_Band,
    Ethnic_group = Ethnic_Group_Desc,
    IMD_quintile = IMD_Quintile_New,
    Comorbidities = LTC_count,
    Frailty_index = Frailty_Index,
    Type,
    PoD,
    HLOS,
    IC_LOS,
    IC_TimeToFirstContact,
    LTC_Dementia,
    IC_Hosp_readmission_time,
    SDIC
  ) %>% 
  mutate(
    Overall = "overall",
    IMD_quintile = fct_explicit_na(f = factor(x = IMD_quintile, ordered = T), 
                                   na_level = "Unknown"),
    Age_group = fct_explicit_na(f = factor(x = Age_group, ordered = T), 
                                na_level = "Unknown"),
    # Comorbidities = fct_explicit_na(f = factor(x = Comorbidities, ordered = T), 
    # na_level = "Unknown"),
    #Frailty_index = fct_explicit_na(f = factor(x = Frailty_index, ordered = T), 
    #  na_level = "Unknown"),
    LTC_Dementia= fct_explicit_na(f = factor(x = LTC_Dementia, ordered = T), 
                                  na_level = "Unknown")
  ) %>%
  mutate(.data = ., across(.cols = where(is.character),
                           .fns = ~fct_explicit_na(f = ., na_level = "Unknown"))
  ) %>% mutate(Type=case_when(
    grepl("ASCCOM-CBS-CBScon",Type)| grepl("ASCCOM-CBS",Type) | grepl("ASCCOM-CBS",Type) ~ "ASCCOM-CBS", 
    grepl("CBS-CBScon" ,Type)~ "CBS",
    grepl("ASCCOM-Other.CBS" ,Type) |grepl("ASCCOM-Other.CBS-Other.CBScon" ,Type) |grepl("ASCCOM-Other.CBScon" ,Type) ~ "ASCCOM-Other.CBS",
    grepl("CBS-Other.CBS" ,Type) |grepl("CBS-Other.CBS-Other.CBScon" ,Type) |grepl("CBS-Other.CBScon" ,Type)~ "CBS-Other.CBS",
    grepl("CBScon-Other.CBS" ,Type) |grepl("CBScon-Other.CBS-Other.CBScon" ,Type) |grepl("CBScon-Other.CBScon" ,Type)~ "CBScon-Other.CBS",
    .default = Type))



# test

summ_vars <- c("Patient_ID")
grp_vars <- c("Overall", "Sex", "Age_group", "Ethnic_group", "Type", 
              "IMD_quintile", "Comorbidities", "Frailty_index", "LTC_Dementia")


out <-
  tibble(
    Var1 = grp_vars
  ) %>% 
  rownames_to_column() %>% 
  mutate(nms = paste("T1", rowname, Var1, sep = "_"),
         out = map(.x = Var1, 
                   .f = ~summarise_demographics(
                     df = all, 
                     grp = .x, 
                     summ = summ_vars, 
                     yearly = F
                   ) %>% 
                     rename(
                       n = n_Patient_ID,
                       n_patients = nd_Patient_ID,
                       pct_patients = pct_nd_Patient_ID
                     ) 
         )
  )

o1_out <-
  out %>%
  pull(out) %>% 
  set_names(x = ., out$nms)

all_ic<-all%>%filter(SDIC==1)

o1_out_num <- 
  list(
    "T1_8_Numeric_ic" = summarise_numeric(df = all_ic, grp = "SDIC")
  )

all_nic<-all%>%filter(SDIC==0)

o1_out_num <- 
  list(
    "T1_8_Numeric_ic" = summarise_numeric(df = all_nic, grp = "SDIC")
  )



as.data.frame(o1_out_num)


comp<-all%>% select(GP_Count, UC111_Count,  UC999_Count,
                  UCOOH_Count, ASCCOM_Count,  ComServ_Count,IC_Count, 
                  AE_Count, Age,Frailty_index, Comorbidities, LTC_Dementia, SDIC)  %>%tbl_summary(
                    by = SDIC,
                    statistic = list(
                      all_continuous() ~ "{mean} ({sd})",
                      all_categorical() ~ "{n} ({p}%)"
                    ),
                    digits = all_continuous() ~ 2,
                    missing_text = "(Missing)")

cols_to_transform <- paste0("stat_", 1:2)


for (col in cols_to_transform) {
  comp$table_body <- comp$table_body %>%
    mutate(extra = !!sym(col)) %>%
    separate(extra, c("number", "percentage"), sep = '\\ ', extra = 'drop') %>%
    mutate(number = as.numeric(gsub(",", "",number))) %>%
    mutate(!!col := ifelse(number <= 5 & var_type == "categorical", "< 5", !!sym(col))) %>%
    select(-c(number, percentage))
}

library(writexl)
write_xlsx(as_tibble(comp), "outpu2_IC.xlsx")

comp %>% as_flex_table(align = "left")
#----------------------------------------------------

comp<-all %>% filter(SDIC==1)%>%select(GP_Count, UC111_Count,  UC999_Count,
                                    UCOOH_Count, ASCCOM_Count,  ComServ_Count,IC_Count, 
                                    AE_Count, Age,Frailty_index, Comorbidities, LTC_Dementia, Type)  %>%tbl_summary(
                                      by =Type ,
                                      statistic = list(
                                        all_continuous() ~ "{mean} ({sd})",
                                        all_categorical() ~ "{n} ({p}%)"
                                      ),
                                      digits = all_continuous() ~ 2,
                                      #label = grade ~ "Tumor Grade",
                                      missing_text = "(Missing)")

cols_to_transform <- paste0("stat_", 1:8)


for (col in cols_to_transform) {
  comp$table_body <- comp$table_body %>%
    mutate(extra = !!sym(col)) %>%
    separate(extra, c("number", "percentage"), sep = '\\ ', extra = 'drop') %>%
    mutate(number = as.numeric(gsub(",", "",number))) %>%
    mutate(!!col := ifelse(number <= 5 & var_type == "categorical","< 5 ", !!sym(col))) %>%
    select(-c(number, percentage))
}

library(writexl)
write_xlsx(as_tibble(comp), "outpu2_IC_Other.xlsx")

comp %>% as_flex_table(align = "left")

#________________________________________________________________
f_30<-all
summary(f$IC_Hosp_readmission_time)
plt30 <- ggbetweenstats(
  data = as.data.frame(f_30),
  x = SDIC,
  y = GP_Count,
  palette = "Pastel2",
  title = "Comparison of Time to readmission of IC (up 30 days)",
)
plt30

variables<-c("GP_Count", "UC111_Count","UC999_Count", "UCOOH_Count","ASCCOM_Count", "ComServ_Count" ,"IC_Count","AE_Count","Last_GP_Event","Last_UC111_Event",
             "Last_UC999_Event","Last_UCOOH_Event","Last_ASCCOM_Event","Last_ComServ_Event","Last_IC_Event", "Last_AE_Event" )

for(variable in variables) {
  cat("Variable:", variable, "\n")
  cat("T-test results:\n")
  t_test_result <- t.test(all[all$SDIC==1,variables], all[all$SDIC==0,variables])
  print(t_test_result)
  
  # Visualization
  plot_title <- paste(variable, " for IC")
  ic_plot <- ggplot(all[all$SDIC==1,], aes_string(x = variable, fill = "SDIC")) +
    geom_histogram(position = "identity", alpha = 0.5, bins = 20) +
    labs(title = plot_title, x = variable, y = "Frequency") +
    theme_minimal()
  
  plot_title <- paste(variable, " for non IC")
  non_ic_plot <- ggplot(all[all$SDIC==0,], aes_string(x = variable, fill = "SDIC")) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 20) +
    labs(title = plot_title, x = variable, y = "Frequency") +
   theme_minimal()
  

  
 # print(ic_plot)
  #print(non_ic_plot)
 
}


variables<-c("Last_GP_Event","Last_UC111_Event",
             "Last_UC999_Event","Last_UCOOH_Event","Last_ASCCOM_Event","Last_ComServ_Event","Last_IC_Event", "Last_AE_Event" )

last_contact <- all_ic %>%
  select(variables) %>%
  mutate(Last_Contact = apply(., 1, function(row) {
    if (all(is.na(row))) {
      return(NA)
    } else {
      return(min(row, na.rm = TRUE))
    }
  }))

table(last_contact$Last_Contact, useNA = 'always')/nrow(last_contact)

last_contact <- all_nic %>%
  select(variables) %>%
  mutate(Last_Contact = apply(., 1, function(row) {
    if (all(is.na(row))) {
      return(NA)
    } else {
      return(min(row, na.rm = TRUE))
    }
  }))

table(last_contact$Last_Contact, useNA = 'always')/nrow(last_contact)


#---------------------------- Last contact before the hospital  IC submission --------------------------

variables<-c("Last_GP_Event","Last_UC111_Event",
             "Last_UC999_Event","Last_UCOOH_Event","Last_ASCCOM_Event","Last_ComServ_Event","Last_IC_Event", "Last_AE_Event" )

last_contact <- all_ic %>%
  select(variables) %>%
  mutate(Last_Contact = apply(., 1, function(row) {
    if (all(is.na(row))) {
      return(NA)
    } else {
      return(min(row, na.rm = TRUE))
    }
  }))
last_contact <- last_contact %>%
  mutate(First_NA_Column = ifelse(all(is.na(.)), names(.)[1], NA))

# Count occurrences of each event variable for the last contact
last_contact$Last_Event_Column <- apply(last_contact[variables], 1, function(x) {
  ifelse(all(is.na(x)), NA, colnames(last_contact)[which.min(x)])
})

event_counts <- table(last_contact$Last_Event_Column)

# Calculate percentages
event_percentages <- prop.table(event_counts) * 100

# Convert to data frame
event_data <- data.frame(
  Event_Column = names(event_percentages),
  Percentage = as.numeric(event_percentages)
)

# Sort data by percentage
event_data <- event_data[order(-event_data$Percentage), ]

# Create the bar plot
plot <- ggplot(event_data, aes(x = Event_Column, y = Percentage)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Last Health Care Contact Before Hospital Admission for IC",
       x = "Event Column",
       y = "Percentage") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size=20,angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Display the plot
print(plot)

# plot time --------------

melted_data <- all_ic %>%
  pivot_longer(cols = all_of(variables), names_to = "Variable", values_to = "Days")

# Create a grid of plots for each variable
plots <- lapply(unique(melted_data$Variable), function(var) {
  data <- melted_data %>%
    filter(Variable == var)
  
  plot <- ggplot(data, aes(x = Days)) +
    geom_density(fill = "skyblue", color = "black") +
    labs(title = paste(var, " Time Distribution"),
         x = "Days to Last Appointment",
         y = "Density") +
    theme_minimal()
  
  return(plot)
})

# Arrange the plots in a grid layout
gridExtra::grid.arrange(grobs = plots, ncol = 2) 
#------------------------------------------------------------
  
#----------------- The most frequent contact within 6 month  for IC 

variables<-c("GP_Count", "UC111_Count","UC999_Count", "UCOOH_Count","ASCCOM_Count", "ComServ_Count" ,"IC_Count","AE_Count")

most_used_contact <- all_ic %>%
  select(variables) %>%
  mutate(Most_Contact = apply(., 1, function(row) {
    if (all(is.na(row))) {
      return(NA)
    } else {
      return(max(row, na.rm = TRUE))
    }
  }))

# Count occurrences of each event variable for the last contact
most_used_contact$Most_Used_Service <- apply(most_used_contact[variables], 1, function(x) {
  ifelse(all(is.na(x)), NA, colnames(most_used_contact)[which.max(x)])
})

event_counts <- table(most_used_contact$Most_Used_Service )

# Calculate percentages
event_percentages <- prop.table(event_counts) * 100

# Convert to data frame
event_data <- data.frame(
  Event_Column = names(event_percentages),
  Percentage = as.numeric(event_percentages)
)

# Sort data by percentage
event_data <- event_data[order(-event_data$Percentage), ]

# Create the bar plot
plot <- ggplot(event_data, aes(x = Event_Column, y = Percentage)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Most Used Service within 6 Month prior Hospital Admission for IC",
       x = "Event Column",
       y = "Percentage") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size=20,angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Display the plot
print(plot)

#________________________________________ NON - IC------------------------------------------------------------

variables<-c("Last_GP_Event","Last_UC111_Event",
             "Last_UC999_Event","Last_UCOOH_Event","Last_ASCCOM_Event","Last_ComServ_Event","Last_IC_Event", "Last_AE_Event" )

last_contact <- all_nic %>%
  select(variables) %>%
  mutate(Last_Contact = apply(., 1, function(row) {
    if (all(is.na(row))) {
      return(NA)
    } else {
      return(min(row, na.rm = TRUE))
    }
  }))
last_contact <- last_contact %>%
  mutate(First_NA_Column = ifelse(all(is.na(.)), names(.)[1], NA))

# Count occurrences of each event variable for the last contact
last_contact$Last_Event_Column <- apply(last_contact[variables], 1, function(x) {
  ifelse(all(is.na(x)), NA, colnames(last_contact)[which.min(x)])
})

event_counts <- table(last_contact$Last_Event_Column)

# Calculate percentages
event_percentages <- prop.table(event_counts) * 100

# Convert to data frame
event_data <- data.frame(
  Event_Column = names(event_percentages),
  Percentage = as.numeric(event_percentages)
)

# Sort data by percentage
event_data <- event_data[order(-event_data$Percentage), ]

# Create the bar plot
plot <- ggplot(event_data, aes(x = Event_Column, y = Percentage)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Last Health Care Contact Before Hospital Admission for non IC",
       x = "Event Column",
       y = "Percentage") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size=20, angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Display the plot
print(plot)

# plot time --------------

melted_data <- all_nic %>%
  pivot_longer(cols = all_of(variables), names_to = "Variable", values_to = "Days")

# Create a grid of plots for each variable
plots <- lapply(unique(melted_data$Variable), function(var) {
  data <- melted_data %>%
    filter(Variable == var)
  
  plot <- ggplot(data, aes(x = Days)) +
    geom_density(fill = "skyblue", color = "black") +
    labs(title = paste(var, " TimevDistribution for non IC", var),
         x = "Days to Last Appointment",
         y = "Density") +
    theme_minimal()
  
  return(plot)
})

# Arrange the plots in a grid layout
gridExtra::grid.arrange(grobs = plots, ncol = 2) 

#----------------

variables<-c("GP_Count", "UC111_Count","UC999_Count", "UCOOH_Count","ASCCOM_Count", "ComServ_Count" ,"IC_Count","AE_Count")

most_used_contact <- all_nic %>%
  select(variables) %>%
  mutate(Most_Contact = apply(., 1, function(row) {
    if (all(is.na(row))) {
      return(NA)
    } else {
      return(max(row, na.rm = TRUE))
    }
  }))

# Count occurrences of each event variable for the last contact
most_used_contact$Most_Used_Service <- apply(most_used_contact[variables], 1, function(x) {
  ifelse(all(is.na(x)), NA, colnames(most_used_contact)[which.max(x)])
})

event_counts <- table(most_used_contact$Most_Used_Service )

# Calculate percentages
event_percentages <- prop.table(event_counts) * 100

# Convert to data frame
event_data <- data.frame(
  Event_Column = names(event_percentages),
  Percentage = as.numeric(event_percentages)
)

# Sort data by percentage
event_data <- event_data[order(-event_data$Percentage), ]

# Create the bar plot
plot <- ggplot(event_data, aes(x = Event_Column, y = Percentage)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Most Used Service within 6 Month prior Hospital Admission For non IC",
       x = "Event Column",
       y = "Percentage") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size=20,angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Display the plot
print(plot)

