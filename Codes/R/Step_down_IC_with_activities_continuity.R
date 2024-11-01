
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
library(mice)

source(icOutput1)


final1 %>%  filter(Population_Segment!='Maternity')%>%
  ggplot(aes(x=PoD, y=HLOS, fill=as.factor(SDIC)))+
  geom_boxplot()+
  ggtitle("Hospital Length of Stay in Days") +
  #facet_wrap(~Population_Segment)+
  coord_flip()+
  guides(color= guide_legend("Step down IC"), fill=guide_legend("Step down IC"))#+
  #scale_color_manual(values = c("0" = "orange", "1" = "purple"))

final1 %>% filter(Population_Segment!='Maternity')%>% filter(PoD=="NEL")%>% group_by(SDIC, readmission90, Population_Segment) %>% summarise(n=n())  %>% 
  mutate(Fraq=n/sum(n)) %>%
  ggplot(aes(y=Fraq, x=as.factor(readmission90), fill=as.factor(SDIC))) +
  geom_bar(stat="identity", position=position_dodge())+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(title="Frequency", x="Hospital Readmission", y="Fraction")  +
  guides(color= guide_legend("Step down IC"), fill=guide_legend("Step down IC"))+
  ggtitle("Readmission after hospital non elective stay") +
  facet_wrap(~Population_Segment)


final1 %>% filter(Population_Segment!='Maternity')%>% filter(PoD=="EL")%>% group_by(SDIC, readmission90, Population_Segment) %>% summarise(n=n())  %>% 
  mutate(Fraq=n/sum(n)) %>%
  ggplot(aes(y=Fraq, x=as.factor(readmission90), fill=as.factor(SDIC))) +
  geom_bar(stat="identity", position=position_dodge())+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(title="Frequency", x="Hospital Readmission", y="Fraction")  +
  guides(color= guide_legend("Step down IC"), fill=guide_legend("Step down IC"))+
  ggtitle("Readmission after electives hospital stay") +
  facet_wrap(~Population_Segment)

final1 %>% filter(Population_Segment!='Maternity')%>% filter(PoD=="EL")%>% group_by(SDIC, Acute_PoD, Population_Segment) %>% summarise(n=n())  %>% 
  mutate(Fraq=n/sum(n)) %>%
  ggplot(aes(y=Fraq, x=as.factor(Acute_PoD), fill=as.factor(SDIC))) +
  geom_bar(stat="identity", position=position_dodge())+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(title="Frequency", x="Hospital Readmission", y="Fraction")  +
  guides(color= guide_legend("Step down IC"), fill=guide_legend("Step down IC"))+
  ggtitle("Readmission after electives hospital stay") +
  facet_wrap(~Population_Segment)


final1 %>% filter(Population_Segment!='Maternity')%>% filter(readmission90==1)%>%
  ggplot(aes(x=Acute_Time, y= PoD, fill=as.factor(SDIC)))+
  geom_density_ridges(alpha=0.3) +
  theme_ridges() + 
  ggtitle("Time to Readmission") +
  facet_wrap(~Population_Segment)+
  guides(color= guide_legend("Step down IC"), fill=guide_legend("Step down IC"))


final1 %>% filter(Population_Segment!='Maternity')%>% filter(!is.na(hosp_next_date) & hosp_time_gap<90) %>%
  ggplot(aes(x=hosp_time_gap, y= hosp_next_pod, fill=as.factor(SDIC)))+
  geom_density_ridges(alpha=0.3) +
  theme_ridges() + 
  ggtitle("Time to Readmission") +
  facet_wrap(~Population_Segment)+
  guides(color= guide_legend("Step down IC"), fill=guide_legend("Step down IC"))


final1 %>% filter(Population_Segment!='Maternity')%>% filter(!is.na(hosp_next_date) & hosp_time_gap<90) %>%
  ggplot(aes(x=Population_Segment, fill=as.factor(SDIC)))+
  geom_density_ridges(alpha=0.3) +
  theme_ridges() + 
  ggtitle("Time to Readmission") +
  facet_wrap(~Population_Segment)+
  guides(color= guide_legend("Step down IC"), fill=guide_legend("Step down IC"))

final1 %>% filter(Population_Segment!='Maternity')%>% mutate(HospReadm=ifelse(!is.na(hosp_next_date) & hosp_time_gap<90, 1, 0)) %>%
      group_by(Population_Segment, HospReadm, SDIC, .groups = "keep" )%>% summarise(n=n()) %>%
  group_by(Population_Segment) %>% mutate(Freq = n / sum(n)) %>%
  ggplot(aes(x=as.factor(HospReadm), y=Freq, fill=as.factor(SDIC)))+
  geom_bar(stat = 'identity', position = 'dodge')+
  ggtitle("Time to Hospital Readmission") +
  facet_wrap(~Population_Segment)+
  guides(color= guide_legend("Step down IC"), fill=guide_legend("Step down IC"))+
  labs(title = "Hospital readmission fraction within each population group", x="Hospital readmission")

final1 %>% filter(Population_Segment!='Maternity')%>% mutate(AEReadm=ifelse(!is.na(AE_Date_After_Spell) & AE_time_gap<90, 1, 0)) %>%
  group_by(Population_Segment, AEReadm, SDIC, .groups = "keep" )%>% summarise(n=n()) %>%
  group_by(Population_Segment) %>% mutate(Freq = n / sum(n)) %>%
  ggplot(aes(x=as.factor(AEReadm), y=Freq, fill=as.factor(SDIC)))+
  geom_bar(stat = 'identity', position = 'dodge')+
  ggtitle("Time to Hospital Readmission") +
  facet_wrap(~Population_Segment)+
  guides(color= guide_legend("Step down IC"), fill=guide_legend("Step down IC"))+
  labs(title = "AE contact -  fraction within each population group", x="AE readmission")


final1 %>% filter(Population_Segment!='Maternity')%>% filter(!is.na(AE_Date_After_Spell) & AE_time_gap<90) %>%
  ggplot(aes(x=AE_time_gap, y= Population_Segment, fill=as.factor(SDIC)))+
  geom_density_ridges(alpha=0.3) +
  theme_ridges() + 
  ggtitle("Time to Readmission to AE") +
  guides(color= guide_legend("Step down IC"), fill=guide_legend("Step down IC"))


final1$Frailty_Index<-as.numeric(final1$Frailty_Index)
final1$LTC_count<-as.numeric(final1$LTC_count)

table(final1$PHM_Segment)
final1$Pathway<-'0'



final1 %>% filter(Population_Segment!='Maternity') %>% group_by(Pathway, Population_Segment) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))%>%
  ggplot(aes(x=as.factor(Pathway), y=perc, fill=Population_Segment)) +
  geom_bar(stat="identity", width = 0.7)+
  geom_text(aes( label = round(perc,3), group=Population_Segment), position = position_stack(vjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(title="Population Segments in each Discharge Pathways", x='Discharge Pathways', y="Fraction")

final1 %>% filter(Population_Segment!='Maternity') %>%filter(readmission90==1) %>% group_by(Pathway, Population_Segment) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))%>%
  ggplot(aes(x=as.factor(Pathway), y=perc, fill=Population_Segment)) +
  geom_bar(stat="identity", width = 0.7)+
  geom_text(aes( label = round(perc,3), group=Population_Segment), position = position_stack(vjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(title="Population Segments in each Discharge Pathways", x='Discharge Pathways', y="Fraction")

final1 %>% filter(Population_Segment!='Maternity')%>% group_by(Provider_Code,Pathway) %>% summarise(n=n())  %>% 
  mutate(Fraq=n/sum(n)) %>%
  ggplot(aes(y=Fraq, fill=as.factor(Pathway), x=as.factor(Provider_Code))) +
  geom_bar(stat="identity", position=position_dodge())+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs( x="StepDown IC", y="Frequancy")  +
  guides(color= guide_legend("Discharge Pathway"), fill=guide_legend("Discharge Pathway"))+
  ggtitle("Discharge pathway counts") +
  facet_wrap(~Provider_Code)



final1 %>% filter(Population_Segment!='Maternity')%>% group_by(SDIC, Acute_PoD, Pathway, readmission90) %>% summarise(n=n())  %>% 
  mutate(Fraq=n/sum(n)) %>%
  ggplot(aes(y=Fraq, x=as.factor(Acute_PoD), fill=as.factor(readmission90))) +
  geom_bar(stat="identity", position=position_dodge())+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(title="Frequency", x="Hospital Readmission Type", y="Fraction")  +
  ggtitle("Readmission Types") +
  facet_wrap(~Pathway)

final1 %>%filter(Population_Segment=='Adult Cancer')%>% select(GP_Count, UC111_Count,  UC999_Count,
                                                          UCOOH_Count, ASCCOM_Count,  ComServ_Count,IC_Count, 
                                                          AE_Count, Age,Frailty_Index, LTC_count, LTC_Dementia, SDIC)  %>%tbl_summary(
                                                            by = SDIC,
                                                            statistic = list(
                                                              all_continuous() ~ "{mean} ({sd})",
                                                              all_categorical() ~ "{n} / {N} ({p}%)"
                                                            ),
                                                            digits = all_continuous() ~ 2,
                                                            label = SDIC ~ "Tumor Grade",
                                                            missing_text = "(Missing)")




final1 %>%filter(Population_Segment=='Frailty')%>% select(GP_Count, UC111_Count,  UC999_Count,
 UCOOH_Count, ASCCOM_Count,  ComServ_Count,IC_Count, 
AE_Count, Age,Frailty_Index, LTC_count, LTC_Dementia, SDIC)  %>%tbl_summary(
  by = SDIC,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} / {N} ({p}%)"
  ),
  digits = all_continuous() ~ 2,
  label = SDIC ~ "Tumor Grade",
  missing_text = "(Missing)")
  

final1 %>%filter(Population_Segment=='EoL')%>% select(GP_Count, UC111_Count,  UC999_Count,
                                                          UCOOH_Count, ASCCOM_Count,  ComServ_Count,IC_Count, 
                                                          AE_Count, Age,Frailty_Index, LTC_count, LTC_Dementia, SDIC)  %>%tbl_summary(
                                                            by = SDIC,
                                                            statistic = list(
                                                              all_continuous() ~ "{mean} ({sd})",
                                                              all_categorical() ~ "{n} / {N} ({p}%)"
                                                            ),
                                                            digits = all_continuous() ~ 2,
                                                            #label = grade ~ "Tumor Grade",
                                                            missing_text = "(Missing)")

final1 %>%filter(Population_Segment=='Long Term Condition')%>% select(GP_Count, UC111_Count,  UC999_Count,
                                                      UCOOH_Count, ASCCOM_Count,  ComServ_Count,IC_Count, 
                                                      AE_Count, Age,  Sex,Frailty_Index, LTC_count, LTC_Dementia, SDIC)  %>%tbl_summary(
                                                        by = SDIC,
                                                        statistic = list(
                                                          all_continuous() ~ "{mean} ({sd})",
                                                          all_categorical() ~ "{n} / {N} ({p}%)"
                                                        ),
                                                        digits = all_continuous() ~ 2,
                                                        #label = grade ~ "Tumor Grade",
                                                        missing_text = "(Missing)")

library(ggstatsplot)
average_frailty <- as.data.frame(aggregate(Frailty_Index ~ Pathway, data = final1, FUN = function(x) c(mean = mean(x), sd = sd(x))))
final1%>%ggplot(aes(x = Pathway, y = Frailty_Index)) +
  stat_summary(fun = mean, geom = "col", fill = "lightblue") +
  stat_summary(fun = mean, geom = "point", size = 5) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.5) + # mean_cl_boot
  theme_ggstatsplot()

unique(final1$Provider_Code)
final1 %>% group_by(Pathway, Provider_Code) %>% summarise(n=n()) %>% filter(n>100) %>%
  ggplot(aes(x = Provider_Code, y = n, fill=Pathway))+
  geom_bar(stat = "identity")+
  facet_wrap(~Pathway)+
  coord_flip()

final1%>% filter(Population_Segment!='Maternity')%>%
  ggplot(aes( x=as.factor(readmission90), fill=as.factor(SDIC))) +
  geom_bar(stat="identity", position=position_dodge())+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(title="Frequency", x="Hospital Readmission Type", y="Fraction")  +
  guides(color= guide_legend("Step down IC"), fill=guide_legend("Step down IC"))+
  ggtitle("Readmission Types") +
  facet_wrap(~Pathway)

final1%>% filter(Population_Segment!='Maternity')%>%
  ggplot(aes(x = Pathway, y = Age)) +
  stat_summary(fun = mean, geom = "col", fill = "salmon2") +
  stat_summary(fun = mean, geom = "point", size = 5) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.5) + # mean_cl_boot
  theme_ggstatsplot()

  final1%>% filter(Population_Segment!='Maternity')%>%
    group_by(Population_Segment, readmission90) %>%
   summarise(n=n())  %>%  mutate(Fraq=n/sum(n))%>% ungroup()%>%
    ggplot(aes( x=Population_Segment,y=Fraq, fill=as.factor(readmission90)))+
    geom_bar(stat="identity", position=position_dodge())+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    coord_flip()+
    #facet_wrap(~SDIC)
    guides(color= guide_legend("Hosp Contact in 90 days"), fill=guide_legend("Hosp contact in 90 days"))+
    labs(title="Hospital contact in each population group")
  
  
final1%>% filter(!(is.na(hosp_next_date) & is.na(AE_Date_After_Spell))) %>%
  select(Patient_ID,Attendance_Date, Acute_Date, Acute_PoD) %>% filter(Patient_ID=='269324295309330330304340358330')

############################################################ SDIC only ##################################################################

final1 %>%filter(SDIC==1)%>% filter(Population_Segment!= 'Maternity')%>% select(GP_Count, UC111_Count,  UC999_Count, Population_Segment,
                                                                      UCOOH_Count, ASCCOM_Count,  ComServ_Count,IC_Count, 
                                                                      AE_Count, Age,  Sex,Frailty_Index, LTC_count, LTC_Dementia)  %>%tbl_summary(
                                                                        by = Population_Segment,
                                                                        statistic = list(
                                                                          all_continuous() ~ "{mean} ({sd})",
                                                                          all_categorical() ~ "{n} / {N} ({p}%)"
                                                                        ),
                                                                        digits = all_continuous() ~ 2,
                                                                        #label = grade ~ "Tumor Grade",
                                                                        missing_text = "(Missing)")







final_SDIC<-final_SDIC%>%filter(Population_Segment!='Maternity')
final_SDIC$Population_Segment<-droplevels(final_SDIC$Population_Segment)
final$Group<-droplevels()
table(final_SDIC$Population_Segment)

final1 %>% filter(SDIC==1) %>%
  ggplot(aes(x=Type, y=HLOS, fill=PoD))+
  geom_boxplot()+
  ggtitle(" Hospital Length of Stay") + labs(x="Type of Intermidiate Care")+
  facet_wrap(~Population_Segment) +
  coord_flip()

final1 %>% filter(SDIC==1) %>% group_by(Type, Population_Segment)%>%summarise(n=n()) %>% mutate(Fraq=n/sum(n)) %>%
  ggplot(aes(x=Type, y=Fraq, fill=Type))+
  geom_bar(stat="identity", position='dodge')+
  ggtitle("Fraction of different population group within each IC service type") + labs(x="Type of Intermidiate Care")+
  facet_wrap(~Population_Segment) 

final1 %>% filter(SDIC==1 & Population_Segment!='Maternity') %>% mutate(PoD_N=ifelse(PoD %in% c('DC', 'EL'), 'EL', 'NEL')) %>%
  ggplot(aes(x=PoD_N,fill=Type))+
  geom_bar(position = 'dodge')+
  ggtitle("Counts of SD IC service type after the EL and NEL hospital stay") + labs(x="Type of Hospital Admissiom")+
  facet_wrap(~Population_Segment) 


final1 %>% filter(SDIC==1) %>%
  ggplot(aes(x=Type, y=IC_LOS, fill=PoD))+
  geom_boxplot()+
  ggtitle(" Intermediate Care Service Length of Stay") +
  #facet_wrap(~PHM_Segment) +
  coord_flip()


final1 %>% filter(SDIC==1) %>%  filter(Type=='CBS') %>%
  ggplot(aes(x=Group, y=IC_LOS, fill=PoD))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  coord_flip()+
  ggtitle("IC Length of Stay for CBS") +
  #facet_wrap(~PHM_Segment)
xlab("")

final1 %>%filter(SDIC==1 & Population_Segment!='Maternity') %>% filter(as.numeric(IC_Contact_Nr)<=200) %>%
  ggplot(aes(x=Type, y=as.numeric(IC_Contact_Nr), fill=PoD))+
  geom_boxplot()+
  ggtitle("Number of Contacts within Spell (<200)") +
  #facet_wrap(~PHM_Segment)
xlab(" Type of IC") +ylab("Number of contacts")+
  coord_flip()


final1 %>%   filter(SDIC==1 & Population_Segment!='Maternity') %>% filter(Type=='CBS') %>%  filter(as.numeric(IC_Contact_Nr)<=200) %>%
  ggplot(aes(x=Group, y=as.numeric(IC_Contact_Nr), fill=PoD))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  coord_flip()+
  ggtitle("Number of Contacts within Spell CBS") +
  #facet_wrap(~PHM_Segment)
  xlab("Group of CBS") +ylab("Number of contacts")


final1 %>%  filter(SDIC==1 &  Population_Segment!='Maternity') %>%
  ggplot(aes(x=Type, y=IC_TimeToFirstContact, fill=PoD))+
  geom_boxplot()+
  ggtitle("Time to first contact") +
  #facet_wrap(~PHM_Segment)
xlab("")+
  coord_flip()


final1 %>%filter(SDIC==1 & Population_Segment!='Maternity') %>%
  ggplot(aes(y=Type, x=IC_TimeToFirstContact, fill=Type))+
  geom_density_ridges(alpha=0.3) +
  theme_ridges() + 
  ggtitle("Time to first contact") +
  facet_wrap(~Population_Segment)
xlab("")


final1 %>% filter(Type=='CBS') %>% filter(SDIC==1 & Population_Segment!='Maternity') %>%
  ggplot(aes(x=Group, y=IC_TimeToFirstContact, fill=Type))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  coord_flip()+
  ggtitle("Time to First Contact for CBS") +
xlab("")



  f<- final1%>%filter(SDIC==1 & Population_Segment!='Maternity') %>% select(Age, Sex, Ethnic_Group, IMD_Decile, Frailty_Index, LTC_count, Population_Segment,Type, Group,  IC_LOS,
                        IC_TimeToFirstContact, IC_Contact_Nr, PoD)
f$Frailty_Index<-as.numeric(f$Frailty_Index)
f$LTC_count<-as.numeric(f$LTC_count)
str(f)

f$IMD_Decile<-as.factor(f$IMD_Decile)
f$IC_TimeToFirstContact<-as.numeric(f$IC_TimeToFirstContact)
f$IC_LOS<-as.numeric(f$IC_LOS)
f$IC_Contact_Nr<-as.numeric(f$IC_Contact_Nr)
#community care beds

str(f)

  f%>% select(!c(Type, IMD_Decile, Ethnic_Group,Group, PoD)) %>%tbl_summary(
by = Population_Segment,
statistic = list(
  all_continuous() ~ "{mean} ({sd})",
  all_categorical() ~ "{n} / {N} ({p}%)"
),
digits = all_continuous() ~ 2,
#label = grade ~ "Tumor Grade",
missing_text = "(Missing)")


f%>% select(!c(IMD_Decile, Ethnic_Group,Group, Population_Segment)) %>%tbl_summary(
  by = Type,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} / {N} ({p}%)"
  ),
  digits = all_continuous() ~ 2,
  #label = grade ~ "Tumor Grade",
  missing_text = "(Missing)")


f%>% select(!c(IMD_Decile, Ethnic_Group_ONS,Type, Population_Segment)) %>%tbl_summary(
  by = Group,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} / {N} ({p}%)"
  ),
  digits = all_continuous() ~ 2,
  #label = grade ~ "Tumor Grade",
  missing_text = "(Missing)")


#comunity care beds
tmp<-f%>% filter(Type=='CCB') 
tmp$IC_TimeToFirstContact<-as.factor(tmp$IC_TimeToFirstContact)
tmp$IC_Contact_Nr<-as.factor(tmp$IC_Contact_Nr)

tmp%>% select(!c(Type, IMD_Decile, Ethnic_Group_ONS,Group, PoD)) %>%tbl_summary(
  by = Population_Segment,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} / {N} ({p}%)"
  ),
  digits = all_continuous() ~ 2,
  #label = grade ~ "Tumor Grade",
  missing_text = "(Missing)")


  tmp%>%select(!c(Type, IMD_Decile, Ethnic_Group_ONS,Group, Population_Segment))%>%tbl_summary(
  by = HospitalAdmission,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} / {N} ({p}%)"
  ),
  digits = all_continuous() ~ 2,
  #label = grade ~ "Tumor Grade",
  missing_text = "(Missing)")

#reablement

tmp<-f%>% filter(Type=='ASCCOM') 


tmp%>% select(!c(Type, IMD_Decile, Ethnic_Group_ONS,Group, PoD)) %>%tbl_summary(
  by = Population_Segment,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} / {N} ({p}%)"
  ),
  digits = all_continuous() ~ 2,
  #label = grade ~ "Tumor Grade",
  missing_text = "(Missing)")


tmp%>%select(!c(Type, IMD_Decile, Ethnic_Group_ONS,Group, Population_Segment))%>%tbl_summary(
  by = PoD,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} / {N} ({p}%)"
  ),
  digits = all_continuous() ~ 2,
  #label = grade ~ "Tumor Grade",
  missing_text = "(Missing)")

#CBS

tmp<-f%>% filter(Type=='CBS') 
tmp$TimeToFirstContact<-as.numeric(tmp$TimeToFirstContact)
tmp$NumberofContacts<-as.factor(tmp$NumberofContacts)

tmp%>% select(!c(Type, IMD_Decile, Ethnic_Group_ONS,Group, HospitalAdmission)) %>%tbl_summary(
  by = PHM_Segment,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} / {N} ({p}%)"
  ),
  digits = all_continuous() ~ 2,
  #label = grade ~ "Tumor Grade",
  missing_text = "(Missing)")


tmp%>%select(!c(Type, IMD_Decile, Ethnic_Group_ONS,Group, PHM_Segment))%>%tbl_summary(
  by = HospitalAdmission,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} / {N} ({p}%)"
  ),
  digits = all_continuous() ~ 2,
  #label = grade ~ "Tumor Grade",
  missing_text = "(Missing)")

tmp$Group<-droplevels(tmp$Group)
tmp%>%select(!c(Type, IMD_Decile, Ethnic_Group_ONS, HospitalAdmission, PHM_Segment))%>%tbl_summary(
  by = Group,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} / {N} ({p}%)"
  ),
  digits = all_continuous() ~ 2,
  #label = grade ~ "Tumor Grade",
  missing_text = "(Missing)")


library(ggstatsplot)

f<-final1%>% filter(SDIC==1 & Population_Segment!='Maternity')
str(f)


plt <- ggbetweenstats(
  data = as.data.frame(f),
  x = Type,
  y = IC_LOS
)
plt


plt <- ggbetweenstats(
  data = as.data.frame(f),
  x = Type,
  y = readmission90
)
plt

summary(final$HospAttendenceDate)

final1 %>% filter(SDIC==1 & Population_Segment!='Maternity') %>%
  ggplot(aes(x=Type, y=IC_LOS, fill=PoD))+
  geom_boxplot()+
  ggtitle("Length of Stay") +
  facet_wrap(~Population_Segment)
xlab("")


final1 %>% filter(SDIC==1 & Population_Segment!='Maternity')%>%
  select(Age, Group, Population_Segment, IC_LOS, IC_TimeToFirstContact,
                     IC_Contact_Nr) %>% tbl_summary(by=Population_Segment) 
  

###################### Readmission



final1 %>% filter(SDIC==1 & Population_Segment!='Maternity')%>% mutate(AEReadm=ifelse(!is.na(AE_Date_After_Spell) & AE_time_gap<90, 1, 0)) %>%
  group_by(Population_Segment, AEReadm, .groups = "keep" )%>% summarise(n=n()) %>%
  group_by(Population_Segment) %>% mutate(Freq = n / sum(n)) %>%
  ggplot(aes(x=as.factor(AEReadm), y=Freq, fill=as.factor(AEReadm)))+
  geom_bar(stat = 'identity', position = 'dodge')+
  ggtitle("Time to Hospital Readmission") +
  facet_wrap(~Population_Segment)+
  guides(color= guide_legend("AE readmission"), fill=guide_legend("AE readmission"))+
  labs(title = "AE contact -  fraction within each population group", x="AE readmission")

final1 %>% filter(SDIC==1 & Population_Segment!='Maternity')%>% mutate(PoD_N=ifelse(PoD %in% c("EL","DC"), 'EL', 'NEL'))%>%
  mutate(HospReadm=ifelse(!is.na(hosp_next_date) & hosp_time_gap<90, 1, 0)) %>%
  group_by(Population_Segment, HospReadm, PoD_N, .groups = "keep" )%>% summarise(n=n()) %>%
  group_by(Population_Segment) %>% mutate(Freq = n / sum(n)) %>%
  ggplot(aes(x=as.factor(HospReadm), y=Freq, fill=PoD_N))+
  geom_bar(stat = 'identity', position = 'dodge')+
  ggtitle("Time to Hospital Readmission") +
  facet_wrap(~Population_Segment)+
  guides(color= guide_legend("Hospital readmission"), fill=guide_legend("AE readmission"))+
  labs(title = "Hospital Readmission - fraction within each population group", x="Hospitall readmission")

final1%>%  filter(SDIC==1 & Population_Segment!='Maternity') %>% select(Provider_Code) %>%table()

final1 %>% filter(SDIC==1 & Population_Segment!='Maternity')%>% filter(!is.na(AE_Date_After_Spell) & AE_time_gap<90) %>%
  ggplot(aes(x=AE_time_gap, y= Population_Segment, fill=Type))+
  geom_density_ridges(alpha=0.3) +
  theme_ridges() + 
  ggtitle("Time to Readmission to AE")
 # guides(color= guide_legend("Step down IC"), fill=guide_legend("Step down IC"))

final1 %>% filter(SDIC==1 & Population_Segment!='Maternity')%>% filter(!is.na(hosp_next_date) & hosp_time_gap<90) %>%
  ggplot(aes(x=hosp_time_gap, y= Population_Segment, fill=Type))+
  geom_density_ridges(alpha=0.3) +
  theme_ridges() + 
  ggtitle("Time to Hospital Readmission")

f<-final1 %>% filter(SDIC==1 & Population_Segment!='Maternity')%>% filter(!is.na(hosp_next_date) & hosp_time_gap<90)

plt <- ggbetweenstats(
  data = as.data.frame(f),
  x = Type,
  y = hosp_time_gap
)
plt


f<-final1 %>% filter(SDIC==1 & Population_Segment!='Maternity')%>% filter(!is.na(AE_Date_After_Spell) & AE_time_gap<90)

plt <- ggbetweenstats(
  data = as.data.frame(f),
  x = Type,
  y = AE_time_gap
)
plt

final1$Frailty_Index<-as.numeric(final1$Frailty_Index)
final1$LTC_count<-as.numeric(final1$LTC_count)


####################################### Whos in


final_ethnic<-final1 %>%filter(SDIC==1 & Population_Segment!='Maternity')%>%
  filter(Ethnic_Group_ONS != 'Unknown' | !is.na(Ethnic_Group_ONS))%>% 
  group_by(Type, Ethnic_Group_ONS) %>% summarise(n=n())%>%
  inner_join(rate_ethnicity, by=join_by(Ethnic_Group_ONS==Ethnic_Group_ONS))%>% 
  mutate(Freq=n/count *10000)

final_sex<-final1%>%filter(SDIC==1 & Population_Segment!='Maternity') %>%  filter(!is.na(Sex)) %>% mutate(Sex=as.character(Sex))%>%
  group_by(Type, Sex) %>% summarise(n=n())%>%
  inner_join(rate_sex, by=join_by(Sex==Sex))%>% 
  mutate(Freq=n/count *10000)

final_imd<-final1 %>%filter(SDIC==1 & Population_Segment!='Maternity') %>% filter(!is.na(IMD_Decile)) %>% 
  group_by(Type, IMD_Decile) %>% summarise(n=n())%>%
  inner_join(rate_IMD, by=join_by(IMD_Decile==IMD))%>% 
  mutate(Freq=n/count *10000)

rate<-data.frame(Service_Type=c(final_sex$Type, final_ethnic$Type,final_imd$Type), 
                 Group=c(final_sex$Sex, final_ethnic$Ethnic_Group_ONS, final_imd$IMD_Decile), 
                 Rate=c(final_sex$Freq, final_ethnic$Freq, final_imd$Freq))

unique(rate$Group)
rate$Group <- factor(rate$Group, levels = unique(rate$Group))


rate %>%mutate(Rate=round(Rate, 1))%>% 
  ggplot(aes(y=Service_Type, x=Group, fill=Rate))+
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1)+
  coord_fixed()+
  geom_text(aes(label = Rate), color = "white", size = 3) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(title='Step Down Intermediate Care Rate per 10000 for 22-23 hospital Admission',fill='Rate per 10000')


#################### rates per group

final_ethnic<-final1 %>%filter(SDIC==1 & Population_Segment!='Maternity')%>%
  filter(Ethnic_Group_ONS != 'Unknown' | !is.na(Ethnic_Group_ONS))%>% 
  group_by(Group, Ethnic_Group_ONS) %>% summarise(n=n())%>%
  inner_join(rate_ethnicity, by=join_by(Ethnic_Group_ONS==Ethnic_Group_ONS))%>% 
  mutate(Freq=n/count *10000)

final_sex<-final1%>%filter(SDIC==1 & Population_Segment!='Maternity') %>%  filter(!is.na(Sex)) %>% mutate(Sex=as.character(Sex))%>%
  group_by(Group, Sex) %>% summarise(n=n())%>%
  inner_join(rate_sex, by=join_by(Sex==Sex))%>% 
  mutate(Freq=n/count *10000)

final_imd<-final1 %>%filter(SDIC==1 & Population_Segment!='Maternity') %>% filter(!is.na(IMD_Decile)) %>% 
  group_by(Group, IMD_Decile) %>% summarise(n=n())%>%
  inner_join(rate_IMD, by=join_by(IMD_Decile==IMD))%>% 
  mutate(Freq=n/count *10000)

rate<-data.frame(Service_Type=c(final_sex$Group, final_ethnic$Group,final_imd$Group), 
                 Group=c(final_sex$Sex, final_ethnic$Ethnic_Group_ONS, final_imd$IMD_Decile), 
                 Rate=c(final_sex$Freq, final_ethnic$Freq, final_imd$Freq))

unique(rate$Group)
rate$Group <- factor(rate$Group, levels = unique(rate$Group))


rate %>%mutate(Rate=round(Rate, 1))%>% 
  ggplot(aes(y=Service_Type, x=Group, fill=Rate))+
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1)+
  coord_fixed()+
  geom_text(aes(label = Rate), color = "white", size = 3) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(title='Step Down Intermediate Care Rate per 10000 for 22-23 hospital Admission',fill='Rate per 10000')


library(survival)



# Create the Surv object for time-to-event data
surv_obj <- Surv(final1$hosp_time_gap, final1$readmission30)

# Fit Cox proportional hazards regression model
cox_model <- coxph(surv_obj ~ Age + IMD_Decile+ Sex + Frailty_Index + HLOS +
                     SDIC + IC_TimeToFirstContact + Type, data = final1)

# Summarize the model
summary(cox_model)
install.packages("survminer")

library(survminer)

# Assuming 'cox_model' is the fitted Cox proportional hazards regression model

# Plot survival curves
ggsurvplot(survfit(cox_model), data = final1, risk.table = TRUE)

# Plot hazard ratio (forest plot)
ggforest(cox_model, data = final1)

library(survivalAnalysis)
forest_plot(cox_model)
autoplot(cox_model)
