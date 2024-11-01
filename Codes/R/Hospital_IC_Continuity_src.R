# library(Amelia)
library(tidyverse)
# library(scales)
# library(ggplot2)
# library(ggmosaic)
# library(rmapshaper)
# library(sf)
#library(maptools)
# library(classInt)
# library(RColorBrewer)
# library(tmap)
# library(arsenal) 
# library(gtsummary)
# library(readr)
# library(mice)
library(zoo)

################################# Death 
death<-get_query(read_file(file = "sql/Death.sql")))
str(death)
death<-death %>% filter(REG_DATE_OF_DEATH>"2021-08-31" & REG_DATE_OF_DEATH<"2023-10-01")

death<-death %>% select(PSEUDO_DEC_NHS_NUMBER, REG_DATE_OF_DEATH)

################################# Linked Hospital

data <- 
  get_query(read_file(file = "sql/HOSP_SD_IC_Activ_contact.sql"))

 str(data)
 summary(data$Attendance_Date)

###############################Re-code other services  ############################################

other.com.row<-data$Group  %in% c("Childrens Services", "Adult Dietetics", 
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


data$Type[other.com.row]<-"Other.CBS"


# data was linked by 3 left joins so the NA Type for spell groups needs to be removed
data <- data %>%
  group_by(Patient_ID, Attendance_Date, Discharge_Date) %>%mutate(n=n()) %>%  ungroup()  %>%
  filter((!(is.na(Type) & n > 1) | n == 1)) 

data%>%group_by(Patient_ID, Attendance_Date, PoD) %>% summarize(n=n()) %>%ungroup()%>%select(PoD) %>%table()
################################Analysis with out DC ######################################################
final<-data%>% filter(PoD!='DC')
final%>%group_by(Patient_ID, Attendance_Date, PoD) %>% summarize(n=n()) %>%ungroup()%>%select(PoD) %>%table()

######################################Next hospital Appointment ##########################################
final<- final %>%
  arrange(Patient_ID, Attendance_Date)

a<-final %>% select(Patient_ID, Attendance_Date, PoD) %>%arrange(Attendance_Date)
a<-unique(a)
a<-a%>% group_by(Patient_ID)%>%arrange((Attendance_Date)) %>% mutate(hosp_next_date=lead(Attendance_Date),
                                                                     hosp_next_pod=lead(PoD)) %>%ungroup()

final1<- final %>% left_join(a, join_by(Patient_ID==Patient_ID, Attendance_Date==Attendance_Date, PoD==PoD))
final1<- final1 %>% mutate(hosp_time_gap=as.numeric((difftime(hosp_next_date, Discharge_Date, units='day'))))

final1<- final1 %>%
  arrange(Patient_ID, Attendance_Date)

summary(final1)



############################# Clear records that IC attendence date is outside the range#########################
final1<-final1%>%  mutate(next_attendance=!is.na(IC_Attendance_Date) & !is.na(hosp_next_date) &
                                             IC_Attendance_Date>hosp_next_date, ## (IC_Attendance_Date < Discharge_Date+days(7) & IC_Referral_Date>=Attendance_Date & IC_Referral_Date<=Discharge_Date+days(7)),
  IC_Referral_Date= if_else(next_attendance, NA,IC_Referral_Date),
  IC_Attendance_Date= if_else(next_attendance, NA, IC_Attendance_Date),
  IC_Discharge_Date=if_else(next_attendance,NA, IC_Discharge_Date),
  Type = if_else(next_attendance,NA, Type),
  Group = if_else(next_attendance, NA,Group),
  IC_Discharge_Outcome=if_else(next_attendance,IC_Discharge_Outcome,NA),
  IC_Contact_Nr=if_else(next_attendance,NA, IC_Contact_Nr))%>%
  ungroup() %>%
  # Drop the helper column next_attendance
  select(-next_attendance)

final1 <- final1 %>%
  group_by(Patient_ID, Attendance_Date, Discharge_Date) %>%mutate(n=n()) %>%  ungroup()  %>%
  filter((!(is.na(Type) & n > 1) | n == 1)) 

#as.data.frame(final1%>% filter(Patient_ID=='269324295309330330304340358330') %>% 
              #  select(PoD, Patient_ID, Attendance_Date, Discharge_Date, IC_Referral_Date, IC_Attendance_Date,
                            #   IC_Discharge_Date, Type, IC_Contact_Nr,hosp_next_date))

#final1 %>%
 # group_by(Patient_ID, Attendance_Date, Discharge_Date) %>% summarise(n=n())
######################################Roll Services ##########################################################
#the Reablemnt and Beds were coded as spells in the system.  CN were coded as contact type.To be able adjust the length of stay between spells we linked contacts but now 
#we need to roll them into spells
#
#final1%>% filter(is.na(hosp_next_date) & !is.na(IC_Discharge_Date)) %>% select (Patient_ID)
#as.data.frame(final1%>% filter(Patient_ID=="269324297299316298303346360334") %>% select(PoD, Patient_ID, Attendance_Date, Discharge_Date, IC_Referral_Date, IC_Attendance_Date,
                           #  IC_Discharge_Date, Type, IC_Contact_Nr,hosp_next_date))

final1<-final1 %>% mutate(IC_Discharge_Date=case_when(
                  IC_Discharge_Date>hosp_next_date ~ hosp_next_date,
                  .default = IC_Discharge_Date
))

final1<-final1%>%
  group_by(GP_Count,Last_GP_Event,UC111_Count,Last_UC111_Event,UC999_Count,Last_UC999_Event,UCOOH_Count,
           Last_UCOOH_Event,ASCCOM_Count,Last_ASCCOM_Event,ComServ_Count,Last_ComServ_Event,IC_Count,Last_IC_Event,
           AE_Count,Last_AE_Event,Patient_ID,Age,Age_Band_FW,Sex,Practice_Code,LSOA,PCN,IMD_Decile,Population_Segment,
           Smoke_status_y01,Frailty_Index,LTC_count,LTC_Dementia,Care_Home,Carer,Date_of_Death,IMD_Decile_New,LSOA_new,Ethnic_Group,
          Decision_to_Refer_to_Service_Date,Attendance_Date,Discharge_Date,PoD,Dimention_2,Dimention_3,
          Discharge_Location,HRG4,Diagnostic,Operat_Procedure,Provider_Code,Record_Classification,
          Discharge_Method_Code,Patient_Classification_Code,Carer_Support_Indicator,
          AE_Date_After_Spell,IC_Referral_Date,Type,Group,IC_Discharge_Outcome,
          AC_LOS,Age_Band,Sex_Desc,IMD_Quintile_New,Ethnic_Category_Desc,Ethnic_Group_Desc,Discharge_Location_Desc,
          Discharge_Method_Desc,Diagnostic_Desc,Diagnostic_Chpt,Treatment_Function_Desc,Admission_Method_Desc,HRG4_Desc,
          Carer_Support_Desc,OPCS4_Desc,Patient_Classification_Desc,PoD_Desc,Provider_Desc,
          hosp_next_date,hosp_next_pod,hosp_time_gap) %>%summarise(IC_Discharge_Date= max(IC_Discharge_Date),
                                                             IC_Attendance_Date=min(IC_Attendance_Date), 
                                                             IC_Contact_Nr=sum(IC_Contact_Nr))

final1%>%group_by(Patient_ID, Attendance_Date, PoD) %>% summarize(n=n()) %>%ungroup()%>%select(PoD) %>%table()
###############################################Calculate length of stay #############################################################################

final1<-final1 %>%
  mutate(HLOS = as.numeric((difftime(Discharge_Date, Attendance_Date, units='day') )),
          IC_LOS  = as.numeric((difftime(IC_Discharge_Date , IC_Attendance_Date, units='day') )),
          IC_TimeToFirstContact =as.numeric((difftime( IC_Attendance_Date, Discharge_Date, units='day'))),
          AE_time_gap=as.numeric(difftime(AE_Date_After_Spell, Discharge_Date, units = 'days')),
          IC_continuity=ifelse(!(Type %in% c('ASCCOM', 'CCB')) & !is.na(IC_Referral_Date) & Attendance_Date > IC_Referral_Date, 1, 0)) %>%
         # mutate(sdic_serv_flag= ifelse((!is.na(IC_Referral_Date) & (IC_Referral_Date>=Attendance_Date))|IC_TimeToFirstContact<=7, 1, 0))%>%
          ungroup()

c<-as.data.frame(final1%>% group_by(Patient_ID, Attendance_Date, PoD) %>% summarise(n=n()))
c%>%select(PoD)%>%table()
 
#final1%>% filter(IC_LOS>90)%>%
 #select(Patient_ID, Attendance_Date, Discharge_Date, IC_Referral_Date, IC_Attendance_Date, IC_Discharge_Date, Type, IC_LOS)

#some issues with dates remove those that starts within hospital spells - should not have happened
final1<-final1%>%filter(is.na(IC_TimeToFirstContact) | IC_TimeToFirstContact>=0)
final1<-final1%>%filter(HLOS<200)

t<-final1
############### filter spells 01 April 22 and 31 March 23

final1<- final1 %>% filter(Discharge_Date>=as.Date("2022-04-01") & Discharge_Date< as.Date("2023-04-01"))

summary(final1$Attendance_Date)
summary(final1$Discharge_Date)


#final1<-
 # final1 %>% 
#  mutate(
 #   Acute_Date=(ifelse(is.na(hosp_next_date) & is.na(AE_Date_After_Spell), NA,
#                              ifelse(!is.na(hosp_next_date) & is.na(AE_Date_After_Spell), hosp_next_date,
 #                                    ifelse(is.na(hosp_next_date) & !is.na(AE_Date_After_Spell), AE_Date_After_Spell, 
  #                                          ifelse(hosp_next_date <= AE_Date_After_Spell, hosp_next_date, AE_Date_After_Spell))))),
   #                       Acute_Time = ifelse(is.na(hosp_next_date) & is.na(AE_Date_After_Spell),NA,
    #                                          ifelse(!is.na(hosp_next_date) & is.na(AE_Date_After_Spell), hosp_time_gap,
     #                                                ifelse(is.na(hosp_next_date) & !is.na(AE_Date_After_Spell), AE_time_gap, 
      #                                                      ifelse(hosp_next_date <= AE_Date_After_Spell,hosp_time_gap,AE_time_gap)))), 
       #                   Acute_PoD = ifelse(is.na(hosp_next_date) & is.na(AE_Date_After_Spell),NA,
        #                                     ifelse(!is.na(hosp_next_date) & is.na(AE_Date_After_Spell), hosp_next_pod,
         #                                           ifelse(is.na(hosp_next_date) & !is.na(AE_Date_After_Spell), 'AE', 
          #                                                 ifelse(hosp_next_date <= AE_Date_After_Spell,hosp_next_pod,'AE')))))


#final1$Acute_PoD[is.na(final1$Acute_PoD)]<-"NA"
#final1$Acute_Time[is.na(final1$Acute_Time)]<- -10


##########################Readmission _flags for both AE and Spells

#final1$readmission90<-ifelse(is.na(final1$Acute_Date) | final1$Acute_Time>90, 0, 1)
#final1$readmission60<-ifelse(is.na(final1$Acute_Date) | final1$Acute_Time>60, 0, 1)
#final1$readmission30<-ifelse(is.na(final1$Acute_Date) | final1$Acute_Time>30, 0, 1)

final1$IC_Hosp_readmission_time<-as.numeric((difftime(final1$hosp_next_date, final1$IC_Discharge_Date , units='day') ))
#summary(final1$IC_Hosp_readmission_time)
#final1$IC_Hosp_readmission<-ifelse(is.na(final1$IC_Hosp_readmission_time), 0, 1)


######### Add SDIC flag ###############################################################

final1<- final1 %>% mutate( SDIC = case_when(
   is.na(Type)  ~ 0,
    Group  %in% c("Childrens Services", "Adult Dietetics", 
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
                                                              "TB Service") ~ 0,
      Type %in% c('CBS') & IC_continuity == 1 & IC_TimeToFirstContact > 7~ 0,
      Type %in% c('CBS') & IC_continuity == 0 & IC_Referral_Date>Discharge_Date+days(7)~0,
    .default = 1
    ))
                  
                    
#as.data.frame(final1%>% filter(Patient_ID=='311343285330349302298362313308') %>% 
 #              select (PoD, Attendance_Date, Discharge_Date, IC_Referral_Date, IC_Attendance_Date, 
  #                     IC_Discharge_Date, SDIC, IC_continuity,IC_Contact_Nr, Type, Group))

 
table(final1$Type, useNA = 'always')

#number of unique hospital spells
#final1%>%group_by(Patient_ID, Attendance_Date, Discharge_Date)%>% summarise(n=n())  %>%n_distinct()
#number of uniqu patients id
#final1%>%group_by(Patient_ID)%>% summarise(n=n())  %>%n_distinct()
#continuity vs single
#table(final1$Type, useNA = 'always')

 final1 %>% 
   group_by(Type) %>% 
   summarise(
     n = n(),
     nd.pid = n_distinct(Patient_ID)
   )

#number of unique patients in sdic
# final1%>%filter(SDIC==1) %>% group_by(Patient_ID) %>% summarise(n=n()) %>%n_distinct()
#number of spells in sdic
#number of unique patients in sdic
# final1%>%filter(SDIC==1) %>% group_by(Patient_ID, Attendance_Date) %>% summarise(n=n()) %>%n_distinct()

# table(final1$IC_continuity[final1$SDIC==1], useNA = 'always')
# final1%>%filter(SDIC==1) %>% select(Type) %>% table()

 final1 %>% filter(SDIC==1)%>%
   group_by(Type) %>% 
   summarise(
     n = n(),
     nd.pid = n_distinct(Patient_ID)
   )
final1%>%summary()
############################### Visualise pathways

# table(final1$Discharge_Location, final1$SDIC)
final1$Pathway<-ifelse(final1$SDIC==0, 0,
                       ifelse(final1$Type=='CCB', 2, 1))
# 
# table(final1$Pathway)/nrow(final1)
# 
# 
# final1%>%filter(IC_continuity!=1) %>% select(Pathway, Discharge_Location) %>% table
# final1%>%filter(IC_continuity!=1) %>% select(Type, Discharge_Location) %>% table

# ###########################################Add continuity #############################

final1$Type <- ifelse(final1$IC_continuity == 1, paste(final1$Type, "con", sep = ""), final1$Type)

# table(final1$Type)
final1%>%filter(IC_Hosp_readmission_time<0)%>% select(IC_Hosp_readmission_time) %>% table()






# t_sdic<-final1%>%filter(SDIC==1) %>%group_by(Patient_ID, Attendance_Date, Discharge_Date)%>% mutate(Plot_Target=paste(Type, collapse = "-") ,
#                                                                             Continuity=sum(IC_continuity), 
#                                                                             
#                                                                             sdic=sum(SDIC)) %>%ungroup()
# t_sdic$Plot_Source='Hospital'                                            
# t_sdic %>% select(Patient_ID, Attendance_Date, Discharge_Date, Plot_Source, Plot_Target) %>%filter(!is.na(Plot_Target))
# 
# library(networkD3)
# tt<-t_sdic%>%group_by(Plot_Source, Plot_Target) %>%summarise(value=n())
# nodes <- data.frame(name = unique(c(tt$Plot_Source, tt$Plot_Target)))
# tt$IDsource <- match(tt$Plot_Source, nodes$name)-1 
# tt$IDtarget <- match(tt$Plot_Target, nodes$name)-1
# sankey<- sankeyNetwork(Links = as.data.frame(tt), Nodes = nodes, Source = "IDsource", Target = "IDtarget",
#                        Value = 'value', NodeID = 'name' )
# sankey

####################################Plots##########################################################################


#final1%>% group_by(Patient_ID, Attendance_Date, Discharge_Date, IC_Discharge_Date, IC_Attendance_Date, Type, Group) %>% summarize(n=n()) %>%
# filter(n>1)
#as.data.frame(final1%>%filter(Patient_ID=='279365316338325329305363315296'& Attendance_Date=='2023-02-21') )


########################### remove stays that resulted with death 
final1<-final1 %>% filter(Discharge_Method_Code!=4 | Discharge_Location!=79)
########################Link death 
final1<-final1%>%group_by(Patient_ID)%>% left_join(death, join_by(Patient_ID == PSEUDO_DEC_NHS_NUMBER)) %>% ungroup()
final1$REG_DATE_OF_DEATH<-as.POSIXct(final1$REG_DATE_OF_DEATH)
str(final1)

################################## unify the patient caracteristics among the records for teh same patient
calculate_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
#final2<-final1
final1<-final1%>% group_by(Patient_ID)%>% mutate(ind=which.min(Age), 
                                                 Age=Age[ind], 
                                                 Age_Band=Age_Band[ind], 
                                                 Sex=calculate_mode(na.omit(Sex)),
                                                 Sex_Desc = calculate_mode(na.omit(Sex_Desc)), 
                                                 Practice_Code=calculate_mode(na.omit(Practice_Code)),
                                                 LSOA=calculate_mode(na.omit(LSOA)),
                                                 IMD_Decile=max(IMD_Decile), 
                                                 Frailty_Index=max(Frailty_Index),
                                                 LTC_count=max(LTC_count),
                                                 LTC_Dementia=max(LTC_Dementia),
                                                 IMD_Quintile_New=max(IMD_Quintile_New),
                                                 Ethnic_Group_Desc=calculate_mode(na.omit(Ethnic_Group_Desc))
                                                 ) %>%ungroup() %>%select(-ind)
#final1 %>% filter(IC_Discharge_Date > REG_DATE_OF_DEATH)%>% select(Patient_ID, IC_Attendance_Date, IC_Discharge_Date, hosp_next_date, REG_DATE_OF_DEATH)
#final1<-final1 %>%mutate(IC_Discharge_Date=ifelse(!is.na(IC_Discharge_Date) & !is.na(IC_Discharge_Date)
                                            #      &IC_Discharge_Date>REG_DATE_OF_DEATH, REG_DATE_OF_DEATH, IC_Discharge_Date))

final1<-final1 %>% mutate(IC_Discharge_Date=case_when(
  IC_Discharge_Date>REG_DATE_OF_DEATH ~ REG_DATE_OF_DEATH,
  .default = IC_Discharge_Date
))

