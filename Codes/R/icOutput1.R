# Output1

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
library(ggstatsplot)
# setup -------------------------------------------------------------------
source("icSetup.R")



source("Hospital_IC_Continuity_src.R")




#----------plot sankey for pathways fo whole dataset Fig2 ----------------------------------------------------------

final1<-final1%>%mutate(Pathway=ifelse(Pathway==0 & Discharge_Location%in% c(54, 65, 85, 87, 88), 3, Pathway))
t_sdic<-final1%>% 
 group_by(Patient_ID, Attendance_Date, Discharge_Date)%>% summarise(Plot_Target=paste('Pathway',max(Pathway), sep = "") ,
                                                                             Continuity=sum(IC_continuity),
                                                                             sdic=sum(SDIC)) %>%ungroup()

t_sdic$Plot_Source='Hospital'                                            
tt<-t_sdic%>%group_by(Plot_Source, Plot_Target) %>%summarise(value=n())
# tt %>% filter(Plot_Target=='CBS-CBS-CBS-CBS')
nodes <- data.frame(name = unique(c(tt$Plot_Source, tt$Plot_Target)))
tt$IDsource <- match(tt$Plot_Source, nodes$name)-1 
tt$IDtarget <- match(tt$Plot_Target, nodes$name)-1
sankey<- sankeyNetwork(Links = as.data.frame(tt), Nodes = nodes, Source = "IDsource", Target = "IDtarget",
                        Value = 'value', NodeID = 'name' , fontSize = 12)

sankey

table(final1$Pathway)/nrow(final1)


p <- htmlwidgets::onRender(sankey,JS)
p

tt %>%
  mutate(
    perc=value/sum(value) *100)

#---------------------------------------------------------------------------------------
table(final1$Discharge_Location)
h.nd<-final1 %>%
  group_by(Patient_ID, Attendance_Date) %>%
  summarise(
    n = n()) %>%n_distinct()

p.nd<-final1 %>%
  group_by(Patient_ID) %>%
  summarise(
    n = n()) %>%n_distinct()

#number of spells in whole cohort

final1 %>%
  group_by(Patient_ID, Attendance_Date, PoD) %>% 
  summarise(
    n = n()) %>%ungroup() %>%
  group_by(PoD) %>%
  summarise(
    n = n()) %>% mutate(n/sum(n)) 

#unique Patient number per POD
final1 %>%
  group_by(Patient_ID, Attendance_Date, PoD) %>% 
  summarise(
    n = n()) %>%ungroup() %>%
  group_by(PoD) %>%
  summarise(
    n = n(), 
    p.u =n_distinct(Patient_ID)) %>% mutate(persc=n/sum(n))


final1 %>%
  group_by(Patient_ID) %>%
  summarise(
    n = n()) %>%n_distinct()

# ------Summary numbers for IC---------------------------------------------------------------------


final1 %>%
  group_by(SDIC==1) %>%
  summarise(
    n = n(),
    p.id=n_distinct(Patient_ID),
  ) %>% mutate(perc=p.id/sum(p.id))


#   
final1 %>% filter(SDIC==1) %>%
  group_by(Patient_ID, Attendance_Date) %>%
  summarise(
    n = n()) %>%n_distinct() 

final1 %>% filter(SDIC==1) %>%
  group_by(Patient_ID, Attendance_Date, PoD) %>% 
  summarise(
    n = n()) %>%ungroup() %>%
  group_by(PoD) %>%
  summarise(
    n = n()) %>% mutate(n/sum(n)) 


# nd.pid
#n_distinct(df$Patient_ID)

#########################Start from final #####################
df <- final1 
df<-df %>% arrange(Patient_ID, Attendance_Date)

#---------------------------Pathway for IC Fig 3 --------------------------------------------------------
t_sdic<-final1%>%filter(SDIC==1) %>% arrange(Type) %>%
 group_by(Patient_ID, Attendance_Date, Discharge_Date)%>% summarise(Plot_Target=paste(unique(Type), collapse = "-") ,
                                                                             Continuity=sum(IC_continuity),
                                                                             sdic=sum(SDIC)) %>%ungroup()

t_sdic<-t_sdic %>% mutate(Plot_Target =ifelse(grepl("CCB", Plot_Target), "CCB", Plot_Target))
-------------------------------t_sdic<-t_sdic %>%mutate(Plot_Target=case_when(
  grepl("ASCCOM-CBS-CBScon",Plot_Target)| grepl("ASCCOM-CBS",Plot_Target) | grepl("ASCCOM-CBS",Plot_Target) ~ "ASCCOM-CBS", 
  grepl("CBS-CBScon" ,Plot_Target)~ "CBS",
  .default = Plot_Target))

t_sdic$Plot_Source='Hospital'                                            
t_sdic %>% select(Patient_ID, Attendance_Date, Discharge_Date, Plot_Source, Plot_Target) %>%filter(!is.na(Plot_Target))
 
table(t_sdic$Plot_Target)/nrow(t_sdic) *100
tt<-t_sdic%>%group_by(Plot_Source, Plot_Target) %>%summarise(value=n())
#tt %>% filter(Plot_Target=='CBS-CBS-CBS-CBS')
nodes <- data.frame(name = unique(c(tt$Plot_Source, tt$Plot_Target)))
tt$IDsource <- match(tt$Plot_Source, nodes$name)-1 
tt$IDtarget <- match(tt$Plot_Target, nodes$name)-1
sankey<- sankeyNetwork(Links = as.data.frame(tt), Nodes = nodes, Source = "IDsource", Target = "IDtarget",
                        Value = 'value', NodeID = 'name', fontSize = 12 )
sankey
 


# age
range(df$Age, na.rm = T)



# date
range(df$Attendance_Date, na.rm = T)

df %>% 
  group_by(
    ym = zoo::as.yearmon(Attendance_Date)
  ) %>% 
  summarise(
    n = n(),
    nd.pid = n_distinct(Patient_ID)
  ) 



df %>% 
  group_by(
    ym = zoo::as.yearmon(Discharge_Date)
  ) %>% 
  summarise(
    n = n(),
    nd.pid = n_distinct(Patient_ID)
  ) 

# skim
df %>% 
  skimr::skim_without_charts()

# format ----------------------------------------------------------------------
df1 <-
  df %>% 
  # slice_head(n = 1000) %>%
  mutate(
    across(.cols = contains("Date"), .fns = ~as.Date(.))
  ) %>% 
  filter(Attendance_Date >= as.Date("2021-09-01") & 
           Attendance_Date <= as.Date("2023-09-01") &
           # (Attendance_Date <= IC_Attendance_Date) &
           (Age >= 18 | is.na(Age)) &
           PoD != "DC"
         )


df1 %>%
  group_by(SDIC) %>%
  summarise(
    n = n(),
    nd.pid = n_distinct(Patient_ID)
  )


# ic ----------------------------------------------------------------------
glimpse(df1)

ic <- 
  df1 %>% 
  filter(
    SDIC == 1 &
      (Attendance_Date <= IC_Attendance_Date)
    ) %>%
  group_by(GP_Count,Last_GP_Event,UC111_Count,Last_UC111_Event,UC999_Count,Last_UC999_Event,UCOOH_Count,
           Last_UCOOH_Event,ASCCOM_Count,Last_ASCCOM_Event,ComServ_Count,Last_ComServ_Event,IC_Count,Last_IC_Event,
           AE_Count,Last_AE_Event,Patient_ID,Age,Age_Band_FW,Sex,Practice_Code,LSOA,PCN,Date_of_Death,IMD_Decile,Population_Segment,
           Smoke_status_y01,Frailty_Index,LTC_count,LTC_Dementia,Care_Home,Carer,
           Decision_to_Refer_to_Service_Date,Attendance_Date,Discharge_Date,PoD,Dimention_2,Dimention_3,
           Discharge_Location,HRG4,Diagnostic,Operat_Procedure,Provider_Code,Record_Classification,
           Discharge_Method_Code,Patient_Classification_Code,Carer_Support_Indicator,
           AE_Date_After_Spell,
           AC_LOS,Age_Band,Sex_Desc,IMD_Quintile_New,Ethnic_Category_Desc,Ethnic_Group_Desc,Discharge_Location_Desc,
           Discharge_Method_Desc,Diagnostic_Desc,Diagnostic_Chpt,Treatment_Function_Desc,Admission_Method_Desc,HRG4_Desc,
           Carer_Support_Desc,OPCS4_Desc,Patient_Classification_Desc,PoD_Desc,Provider_Desc,
           hosp_next_date,hosp_next_pod,hosp_time_gap, REG_DATE_OF_DEATH) %>%summarise(IC_Discharge_Date= max(IC_Discharge_Date),
                                                                    IC_Attendance_Date=min(IC_Attendance_Date), 
                                                                    IC_Contact_Nr=sum(IC_Contact_Nr),
                                                                    Type=paste(sort(unique(Type)), collapse = "-"),
                                                                    Group=paste(unique(Group), collapse = "-"),
                                                                    IC_Continuity=sum(IC_continuity)) %>%
           mutate(HLOS = as.numeric((difftime(Discharge_Date, Attendance_Date, units='day') )),
                  IC_LOS  = as.numeric((difftime(IC_Discharge_Date , IC_Attendance_Date, units='day') )),
                  IC_TimeToFirstContact =as.numeric((difftime( IC_Attendance_Date, Discharge_Date, units='day'))),
                  IC_Hosp_readmission_time=as.numeric((difftime(hosp_next_date, IC_Discharge_Date , units='day') )),
                  Type =ifelse(grepl("CCB", Type), "CCB", Type)) %>%ungroup()%>%
                                                                  
  select(
    Patient_ID,
    Age,
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
    Discharge_Date,
    IC_Attendance_Date,
    IC_Discharge_Date,
    Discharge_Location,
    Diagnostic_Chpt,
    IC_TimeToFirstContact,
    LTC_Dementia,
    IC_Hosp_readmission_time, 
    IC_Continuity,
    REG_DATE_OF_DEATH
    ) %>% 
  mutate(
    Overall = "overall",
    IMD_quintile = fct_explicit_na(f = factor(x = IMD_quintile, ordered = T), 
                                   na_level = "Unknown"),
    Age_group = fct_explicit_na( f= factor(x = Age_group, ordered = T), 
                                na_level = "Unknown"),
    Comorbidities = fct_explicit_na(f = factor(x = Comorbidities, ordered = T), 
                                   na_level = "Unknown"),
    #Frailty_index = fct_explicit_na(f = factor(x = Frailty_index, ordered = T), 
                                  #  na_level = "Unknown"),
   # LTC_Dementia= fct_explicit_na(f = factor(x = LTC_Dementia, ordered = T), 
                                 #  na_level = "Unknown")
  ) %>%
  mutate(.data = ., across(.cols = where(is.character),
                          .fns = ~fct_explicit_na(f = ., na_level = "Unknown"))
  ) %>% mutate(Type=case_when(
              grepl("ASCCOM-CBS-CBScon",Type)| grepl("ASCCOM-CBS",Type) | grepl("ASCCOM-CBS",Type) ~ "ASCCOM-CBS", 
              grepl("CBS-CBScon" ,Type)~ "CBS",
              .default = Type))

############################## Missingness #################################
missingness<-function(data)
{
  missing_vector<-apply(data, 2, function(x) sum(is.na(x)))
  missing_prop<-missing_vector/nrow(data)
  
  
  miss_prop<-data.frame(prop=missing_prop)
  
  miss_prop$names<-rownames(miss_prop)
  miss_prop_sel<-miss_prop[miss_prop$prop>0,]
  return(miss_prop_sel)
}


miss_prop_sel<-missingness(ic)
ggplot(miss_prop_sel, aes(x=names, y=prop, fill=names))+geom_bar(stat="identity") +coord_flip()
#missmap(final)


n_distinct(df$Patient_ID)
n_distinct(df1$Patient_ID)
n_distinct(ic$Patient_ID)
dim(ic)


summ_vars <- c("Patient_ID")
grp_vars <- c("Overall", "Sex", "Age_group", "Ethnic_group", 'Type',
              "IMD_quintile", "Comorbidities", "Frailty_index","LTC_Dementia")

out <-
  tibble(
    Var1 = grp_vars
  ) %>% 
  rownames_to_column() %>% 
  mutate(nms = paste("T1", rowname, Var1, sep = "_"),
         out = map(.x = Var1, 
                    .f = ~summarise_demographics(
                      df = ic, 
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

# check
o1_out$T1_1_Overall %>%
  pull(n_patients) %>%
  sum(.)

#check for frailty and dementia
ic %>% group_by(Patient_ID)%>%summarise(dementia=max(LTC_Dementia)) %>% 
  group_by(dementia) %>%summarise(n=n()) %>% mutate (perc=n/sum(n))

mean(as.numeric(ic$Frailty_index), na.rm = T)
sd(as.numeric(ic$Frailty_index), na.rm = T)

median(as.numeric(ic$Frailty_index), na.rm = T)
IQR(as.numeric(ic$Frailty_index), na.rm = T)

## o1 plot --------------------------------------------------------------------

o1_out_plots <- 
  o1_out %>% 
  map(.x = ., 
      .f = ~pivot_longer(data = ., cols = where(is.numeric), 
                         names_to = "metric", values_to = "values") %>% 
        # dplyr::filter(metric %in% c("nd_patient_id", "nd_ac_spell_id", "std_number", "pop_n")) %>% 
        ggplot(aes(x = .data[[names(.x)[1]]], y = values)) +
        geom_col(position = "dodge") +
        geom_text(aes(label = round(values, 2)), 
                  position = position_dodge(),
                  vjust = 1.5,
                  color="white", 
                  size=2
        ) +
        facet_wrap(~metric, scales = "free_y", nrow = 3) +  
        theme_minimal() +
        theme(legend.position = "top")
  ) %>% 
  set_names(x = ., nm = names(o1_out))



## o1 write tables ---------------------------------------------------------
# modify WriteTables.R with correct headers
write_my_tables(out = o1_out, filename = "o1_out_cat_tables")
saveRDS(o1_out, file = "output-1/o1_out_cat.rds")


## o1 write plots----------------------------------------------------------------
write_plots(o1_out_plots)
saveRDS(o1_out_plots, file = "output-1/o1_out_cat_plots.rds")


# o1 num ------------------------------------------------------------------
o1_out_num <- 
  list(
    "T1_9_Numeric" = summarise_numeric(df = ic, grp = "Overall")
  )

     
     

o1_out <- 
  append(o1_out, o1_out_num)

write_my_tables(out = o1_out, filename = "o1_out")
saveRDS(o1_out, file = "output-1/o1_out.rds")

# save --------------------------------------------------------------------
save(list = ls(), file = ".RData")

# end ---------------------------------------------------------------------

##################### Plots
final1 %>%filter( Population_Segment!='Maternity')%>%
  ggplot(aes(x=PoD, y=HLOS, fill=as.factor(SDIC)))+
  geom_boxplot()+
  scale_y_log10() +
  ggtitle("Hospital Length of Stay in Days") +
  #facet_wrap(~Population_Segment)+
  coord_flip()+
  guides(color= guide_legend("Step down IC"), fill=guide_legend("Step down IC"))#+
#scale_color_manual(values = c("0" = "orange", "1" = "purple"))

ic%>%
  ggplot(aes(x=PoD, y=HLOS, fill=as.factor(Type)))+
  geom_boxplot()+
  scale_y_log10() +
  ggtitle("Hospital Length of Stay in Days") +
  #facet_wrap(~Population_Segment)+
  coord_flip()+
  guides(color= guide_legend("Step down IC"), fill=guide_legend("Step down IC"))#+
#scale_color_manual(values = c("0" = "orange", "1" = "purple"))

ic%>%
  group_by(PoD)%>%
  summarise(
    n=n())%>% mutate(perc=n/sum(n))

ic%>% filter(PoD=='EL') %>%
  group_by(Patient_ID)%>%
  summarise(
    n=n())%>% mutate(perc=n/sum(n))

ic%>%
  group_by(Type,PoD)%>%
  summarise(
    n=mean(HLOS), sd=sd(HLOS))

ic%>%group_by(Type, IC_Continuity) %>%summarise(n=n()) %>%ungroup()%>%
   filter(IC_Continuity>=1) %>% select(n)%>%sum()/9740# summarise(n=n_distinct(Patient_ID) )



  ic%>% filter(IC_Hosp_readmission_time <90) %>%
  ggplot(aes(x=IC_Hosp_readmission_time, y= PoD, fill=PoD))+
  geom_density_ridges(alpha=0.3) +
  theme_ridges() + 
  ggtitle("Time to Readmission") 
  
####################### Fig 5

library(ggstatsplot)
# Fi by pathway ------------------------------------------------------------------
overall_mean_fi <- mean(final1$Frailty_Index)
sdic_mean_fi <- mean(as.numeric(as.character(ic$Frailty_index)))

# Create dataframe for legend
legend_data <- data.frame(label = c("Overall Mean", "IC Mean"),
                          yintercept = c(overall_mean_fi, sdic_mean_fi),
                          color = c("green", "red"))

final1 %>%group_by(Patient_ID, Attendance_Date, Frailty_Index) %>%summarise(Pathway=max(Pathway)) %>%
  ggplot(aes(x = Pathway, y = Frailty_Index)) +
  stat_summary(fun = mean, geom = "col", fill = "lightblue") +
  stat_summary(fun = mean, geom = "point", size = 5) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.5) +
  geom_hline(data = legend_data, aes(yintercept = yintercept, color = label, linetype = label), size = 1) +
  #geom_text(data = legend_data, aes(x = Inf, y = yintercept, label = label, color = label), hjust = 0, vjust = -0.5) +
  labs(title = "",
       x = "Pathway",
       y = "Frailty Index") +
 # scale_color_manual(values = c("green" = "green", "red" = "red")) +
  scale_linetype_manual(values = c("Overall Mean" = "dashed", "IC Mean" = "dashed")) +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size=20, hjust = 1))


# Age by Pathway ---------------------------------------------
  overall_mean_age <- mean(final1$Age)
  sdic_mean_age <- mean(final1$Age[final1$SDIC == 1])
  
  # Create dataframe for legend
  legend_data_age <- data.frame(label = c("Overall Mean", "IC Mean"),
                                yintercept = c(overall_mean_age, sdic_mean_age),
                                color = c("green", "red"))
  
  # Plot the data
  final1 %>% group_by(Patient_ID, Attendance_Date, Age) %>%summarise(Pathway=max(Pathway)) %>%
    ggplot(aes(x = Pathway, y = Age)) +
    stat_summary(fun = mean, geom = "col", fill = "lightblue") +
    stat_summary(fun = mean, geom = "point", size = 5) +
    stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.5) +
    geom_hline(data = legend_data_age, aes(yintercept = yintercept, color=label, linetype = label), size = 1) +
    geom_text(data = legend_data_age, aes(x = Inf, y = yintercept, label = label), hjust = 0, vjust = -0.5) +
    labs(title = "",
         x = "Pathway",
         y = "Age") +
    scale_linetype_manual(values = c("Overall Mean" = "dashed", "IC Mean" = "dashed")) +
    theme_minimal()+
    theme(
      plot.title = element_text(size = 20, face = "bold"),
      axis.title.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      axis.text.x = element_text(size=20, hjust = 1))
  
  
  ic$Frailty_index<-as.numeric(ic$Frailty_index)
  ic$Comorbidities<-as.numeric(ic$Comorbidities)
  
  #----------Compare stay between types  for number of hospital stays 
  
  ic_comp_type<-ic %>% select(Age, Frailty_index,  LTC_Dementia, Comorbidities, Ethnic_group, IMD_quintile, Type, IC_LOS, IC_TimeToFirstContact)  %>%tbl_summary(
                      by = Type,
                      statistic = list(
                        all_continuous() ~ "{mean} ({sd})",
                        all_categorical() ~ "{n} ({p}%)"
                      ),
                      digits = all_continuous() ~ 2,
                      #label = grade ~ "Tumor Grade",
                      missing_text = "(Missing)") %>%  bold_labels()
  
  cols_to_transform <- paste0("stat_", 1:5)
  
  
  for (col in cols_to_transform) {
    ic_comp_type$table_body <- ic_comp_type$table_body %>%
      mutate(extra = !!sym(col)) %>%
      separate(extra, c("number", "percentage"), sep = '\\ ', extra = 'drop') %>%
      mutate(number = as.numeric(gsub(",", "",number))) %>%
      mutate(!!col := ifelse(number < 5 & var_type == "categorical", "< 5", !!sym(col))) %>%
      select(-c(number, percentage))
  }
   
    library(writexl)
    write_xlsx(as_tibble(ic_comp_type), "outpu1_Type.xlsx")
    
    ic_comp_type %>% as_flex_table(align = "left")
#-----------------------------------------------------------------------------------
  ic %>%#filter(PoD=='NEL')%>%
    ggplot(aes(x=Type, y=IC_LOS, fill=Type))+
    geom_boxplot()+
  ggtitle(" IC Length of Stay vs IC Type") +
    scale_fill_brewer(palette = "Set2")+
    theme(text = element_text(size = 20))+
    theme(panel.background = element_rect(fill = "white"))
  
 # --------------------------IC-LOS -------------------------------------------- 
  f<-ic #%>%#filter(PoD=='NEL')
  summary(f$IC_Hosp_readmission_time)
  plt <- ggbetweenstats(
    data = as.data.frame(f),
    x = Type,
    y = IC_LOS, 
    palette = "Set2",
  ggsignif.args = list(textsize = 5, tip_length = 0.01),
      ggtheme =   theme(text = element_text(size = 18),panel.background = element_rect(fill = "white") )
    )
  plt
  
table(ic$Type)
  
  add<-ic %>% select(Type, IC_TimeToFirstContact, IC_LOS, HLOS)%>% group_by(Type) %>% summarise(
    min_T2F = min(IC_TimeToFirstContact),
    Q1_T2F = quantile(IC_TimeToFirstContact, 0.25),
    median_T2F = median(IC_TimeToFirstContact),
    mean_T2F= mean(IC_TimeToFirstContact), 
    Q3_T2F = quantile(IC_TimeToFirstContact, 0.75),
    max_T2F = max(IC_TimeToFirstContact),
    sd_T2F = sd(IC_TimeToFirstContact),
    iqr_T2F = iqr(IC_TimeToFirstContact),
    min_H_LOS = min(HLOS),
    Q1_H_LOS = quantile(HLOS, 0.25),
    median_H_LOS = median(HLOS), 
    mean_H_LOS = mean(HLOS), 
    Q3_H_LOS = quantile (HLOS, 0.75),
    max_H_LOS = max(HLOS),
    sd_H_LOS = sd(HLOS),
    iqr_H_LOS = iqr(HLOS),
    min_IC_LOS = min(IC_LOS),
    Q1_IC_LOS = quantile(IC_LOS, 0.25),
    median_IC_LOS = median(IC_LOS),
    mean_IC_LOS= mean(IC_LOS), 
    Q3_IC_LOS =quantile (IC_LOS, 0.75),
    max_IC_LOS = max(IC_LOS),
    sd_IC_LOS = sd(IC_LOS),
    IQR_IC_LOS =iqr(IC_LOS)
  )
  
  cbs <- ic%>% filter(Type=='CBS') %>% group_by(Group) %>% summarize(n())
  write(as.character(cbs$Group), file ="CBS_groups.csv")
  
  


    
write.csv(add, file='MedianAnd_IQR_by_IC_Type.csv', row.names = F)
  
  
  f<-ic %>% filter(IC_LOS<100)
  plt_n.out <- ggbetweenstats(
    data = as.data.frame(f),
    x = Type,
    y = IC_LOS, 
    palette = "Set2",
    ggsignif.args = list(textsize = 5, tip_length = 0.01),
    ggtheme =   theme(text = element_text(size = 18),panel.background = element_rect(fill = "white") )
  )
  plt_n.out
  
  
  
  ic %>%
    ggplot(aes(x=Type, y=IC_TimeToFirstContact, fill=Type))+
    geom_boxplot()+
    ggtitle("Time to first contact") +
    #facet_wrap(~PHM_Segment)
    scale_fill_brewer(palette = "Set2")+
      theme(text = element_text(size = 20))+
      theme(panel.background = element_rect(fill = "white"))
    
    f<-ic #%>  %#filter(PoD=='NEL')
    summary(f$IC_Hosp_readmission_time)
    plt <- ggbetweenstats(
      data = as.data.frame(f),
      x = Type,
      y = IC_TimeToFirstContact, 
      palette = "Set2",
      ggtheme =   theme(text = element_text(size = 18),panel.background = element_rect(fill = "white") )
    )
    plt
    
    
    f<-ic  %>% filter(Type=='CBS' & Group %in% c('Community Intravenous Antibiotic Service ', 'Community Stroke Team', 'LTCs Management - Respiratory',
                                                 'Virtual Respiratory Ward','Integrated Neighbourhood Team - Nursing')) 
    plt <- ggbetweenstats(
      data = as.data.frame(f),
      x = Group,
      y = IC_TimeToFirstContact, 
      palette = "Dark2",
      ggtheme =   theme(text = element_text(size = 20),panel.background = element_rect(fill = "white") )
    )
    plt
    
    ic %>% filter(Type=='CBS' & Group %in%c('Integrated Neighbourhood Team - Nursing'))  %>% group_by(PoD) %>% summarise(n(), median(IC_TimeToFirstContact))
    
    
    
    ic %>% group_by(Group) %>%summarize(n=n_distinct(Patient_ID)/7320 *100)  %>% filter (n>1) %>%
      ggplot(aes(y=n, x="", fill=Group))+geom_bar(stat="identity", width=1) +coord_polar("y", start=0)
      coord_polar("y", start=0)
      
      ic %>% filter(Type ==c('CBS', 'CBScon')) %>% group_by(Group) %>%summarize(n=n()/9740 *100) %>% filter (n>1) %>%
        ggplot(aes(y=n, x="", fill=Group))+geom_bar(stat="identity", width=1) +coord_polar("y", start=0)
      coord_polar("y", start=0)
 
############################## Comparison with NE Fig 4 #######################################
#-------------------------- age      --------------------------------------------------------------------------------
 
          
  p_age<-ic%>%
    ggplot( aes(x = Age, fill = Type, color = Type)) +
    geom_density(alpha = 0.5) +
    labs(title = "Age Density vs IC Type",
         x = "Age",
         y = "Density") +
    scale_fill_brewer(palette = "Set2") +
    scale_color_brewer(palette = "Set2")+
    theme(panel.background = element_rect(fill = "white"))+
        theme(
          plot.title = element_text(size = 20, face = "bold"),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          axis.text.x = element_text(size=20, hjust = 1),
          axis.text.y = element_text(size=20, hjust = 1), 
          legend.title=element_text(size=20), 
          legend.text=element_text(size=17))
#------------------------  Gender
  
  total_males <- o1_out$T1_2_Sex$n_patients[o1_out$T1_2_Sex$Sex=='Male']
  total_females <- o1_out$T1_2_Sex$n_patients[o1_out$T1_2_Sex$Sex=='Female']
  perc_male <- o1_out$T1_2_Sex$pct_patients[o1_out$T1_2_Sex$Sex=='Male']
  perc_female <- o1_out$T1_2_Sex$pct_patients[o1_out$T1_2_Sex$Sex=='Female']
  cohort_data <- data.frame(Sex_Desc = c("Male", "Female"),
                            perc = c(perc_male, perc_female))
  
  
  # Calculate percentages for each grouping
  percentage_data <-ic %>%
    group_by(Type, Sex) %>%
    summarise(n = n()) %>%
    mutate(perc = n / sum(n) *100)
  
  # Plot the data
  p_sex<-percentage_data %>%
    ggplot(aes(x = Sex, y = perc, fill=Type)) +
    geom_bar(stat = "identity", position = position_dodge()) +
  #  geom_errorbar(aes(ymin = perc - 1.96 * sqrt(perc * (1 - perc) / n),
   #                   ymax = perc + 1.96 * sqrt(perc * (1 - perc) / n)),
    #              position = position_dodge(width = 0.9), width = 0.2) +  # Add error bars
    labs(title = "Gender of IC Users",
         x = "Gender",
         y = "% of IC patients") +
    scale_fill_brewer(palette = "Set2") +
    #geom_line(data = cohort_data, aes(group = 1), color = "blue", size = 1.5) +
    #geom_line(data = cohort_data, aes(x = Sex_Desc, y = perc), color = "blue", size = 1.5) +
    #geom_point(data = cohort_data, aes(x = Sex_Desc, y = perc), color = "blue", size = 4) +
   # geom_text(data = cohort_data, aes(label = paste0(perc, "%"), y = perc), color = "blue", vjust = -0.5, hjust = -0.5) +
    theme(
      plot.title = element_text(size = 20, face = "bold"),
      axis.title.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      axis.text.x = element_text(size=20, hjust = 1),
      axis.text.y = element_text(size=20, hjust = 1), 
      legend.title=element_text(size=20), 
      legend.text=element_text(size=17))+
    theme(panel.background = element_rect(fill = "white"))
  
  
  library(gridExtra)
  grid.arrange(p_age, p_sex, ncol = 2)
  
  # per etnicity-------------------------------
  
  
  percentage_data <- ic %>%
    group_by(Type, Ethnic_group) %>%
    summarise(n = n()) %>%
    mutate(perc = n / sum(n) *100)
  
  # Plot the data
  p_ethn<-percentage_data %>%
    ggplot(aes(x = Ethnic_group, y = perc, fill=Type)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    #geom_errorbar(aes(ymin = perc - 1.96 * sqrt(perc * (1 - perc) / n),
      #                ymax = perc + 1.96 * sqrt(perc * (1 - perc) / n)),
       #           position = position_dodge(width = 0.9), width = 0.2) +  # Add error bars
    labs(title = "Ethnicity of IC Users",
         x = "Ethnicity",
         y = "% of IC Patients") +
    scale_fill_brewer(palette = "Set2") +
    #geom_line(data = cohort_data, aes(x = Ethnic_Group_Desc, y = perc), color = "blue", size = 1.5) +
    #geom_point(data = cohort_data, aes(x = Ethnic_Group_Desc, y = perc), color = "blue", size = 4) +
    #geom_text(data = cohort_data, aes(label = paste0(round(perc, 1), "%"), y = perc), color = "blue", vjust = -0.5, hjust = -0.5) +
    theme(
      plot.title = element_text(size = 20, face = "bold"),
      axis.title.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      axis.text.x = element_text(size=20,angle = 19, vjust = 0.5, hjust=1),
      axis.text.y = element_text(size=20, hjust = 1),
      legend.title=element_text(size=20), 
      legend.text=element_text(size=17)
    )+
    theme(panel.background = element_rect(fill = "white"))
  

  
  final1 %>%filter(SDIC==1 & Population_Segment!='Maternity') %>% 
    group_by(Ethnic_Group_Desc)%>% summarise(n=n())%>%mutate(perc=n/sum(n))
  
  #over 80
  final1 %>%filter(SDIC==1 & Population_Segment!='Maternity') %>%  filter(Age>80) %>%
    group_by(Ethnic_Group_Desc)%>% summarise(n=n())%>%mutate(perc=n/sum(n))
  

  
  #-------------------------- Frailty of IC -----------------
  
  p_fi<-ic%>%
    ggplot( aes(x = as.numeric(as.character(Frailty_index)), fill = Type, color = Type)) +
    geom_density(alpha = 0.5) +
    labs(title = "Frailty",
         x = "Frailty",
         y = "Density") +
    scale_fill_brewer(palette = "Set2") +
    scale_color_brewer(palette = "Set2")+
    theme(panel.background = element_rect(fill = "white"))+
    theme(
      plot.title = element_text(size = 20, face = "bold"),
      axis.title.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      axis.text.x = element_text(size=20),
      axis.text.y = element_text(size=20, hjust = 1)
    )
    
  
 
  
  
#--------------------------IMD --------------------------------  
    p_imd<- ic %>%
      group_by(Type, IMD_quintile) %>% summarise(n=n())%>%
      mutate(percentage = n / sum(n) * 100) %>%
      ggplot(aes(x = IMD_quintile, y = percentage, fill = Type)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "IMD Deprivation Quintile",
           x = "IMD Deprivation Quintile",
           y = "% of IC Patients") +
      scale_fill_brewer(palette = "Set2") +
    theme(
      plot.title = element_text(size = 20, face = "bold"),
      axis.title.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      axis.text.x = element_text(size=20),
      axis.text.y = element_text(size=20, hjust = 1),
      legend.title=element_text(size=20), 
      legend.text=element_text(size=17)
    )+
    theme(panel.background = element_rect(fill = "white"))

  # -------------------dementia
    
    percentage_data <- ic%>%  
     distinct(Patient_ID, Type, LTC_Dementia)%>%
      group_by(Type, LTC_Dementia) %>% summarise(n=n_distinct(Patient_ID))%>% filter(LTC_Dementia==1)%>%ungroup() %>%
     mutate(percentage = n / sum(n) * 100)
     #mutate(percentage = n / 7230 * 100)
    
    # Plot the data
    p_dem<-percentage_data %>% filter(LTC_Dementia==1) %>%
      ggplot(aes(x = Type, y = percentage, fill = Type)) +
      geom_bar(stat = "identity") +
      labs(title = "Dementia Flag for IC Type",
           x = "Type",
           y = "% of IC Patients") +
      scale_fill_brewer(palette = "Set2") +
      theme(
        plot.title = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size=20, angle = 20, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size=20, hjust = 1),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=17)
      )+
      theme(panel.background = element_rect(fill = "white"))
      
   
    grid.arrange(p_imd, p_ethn, ncol = 2)
    grid.arrange(p_dem, p_fi, ncol = 2)
    
    
    write.csv(percentage_data, file="dimentia_IC_type_plot.csv", row.names = F)
    
   write.csv(ic%>%select(Frailty_index, Type), file="Frailty_IC_Type_Plot.csv", row.names = F)
    
 #______________________________________________________________________________________   
    final1 %>% filter(SDIC == 1 & Population_Segment != 'Maternity') %>% 
      group_by(Type,PoD) %>% summarise(n=n())%>%mutate(perc=n/sum(n))
    
    final1 %>% group_by(PoD, SDIC) %>%
      summarise(n=n(), perc=n()/nrow(final1))
    
    final1 %>% group_by( PoD, SDIC) %>%
      summarise(n=n())%>% mutate(perc=n/sum(n))
    
    final1 %>% filter(SDIC==1) %>% group_by(Type) %>% summarise(n=n()) %>%mutate(perc=n/sum(n))
    final1 %>% group_by(SDIC) %>% summarize(hlos_m=mean(HLOS), sd=sd(HLOS))
  
  # hospital readmission ------------------------------------------------------------------------------
    #how many patient were readmited 
    
  #group by hospital spells 

    summary(ic$IC_Hosp_readmission_time)
    
    ic$readmission30<-ifelse(!is.na(ic$IC_Hosp_readmission_time) & ic$IC_Hosp_readmission_time<=30, 1, 0)
    
    final1$readmission30 <-ifelse(!is.na(final1$IC_Hosp_readmission_time) & final1$IC_Hosp_readmission_time<=30, 1, 0)
    final1%>% filter(is.na(hosp_next_date) & IC_Hosp_readmission_time==0)%>%select(hosp_next_date, readmission30) %>%table()
    
    ic%>% group_by(readmission30) %>%summarise(n=n()) %>%mutate(n/sum(n))

    ic%>% filter(readmission30==1) %>%group_by(Patient_ID) %>% summarise(n())
    
    ic%>% filter(readmission30==1) %>%group_by(Patient_ID) %>% summarise(n()) %>%nrow()/7320
    
    
    ic$readmission60<-ifelse(!is.na(ic$IC_Hosp_readmission_time) & ic$IC_Hosp_readmission_time<=60, 1, 0)
    ic%>% filter(readmission60==1) %>%group_by(Patient_ID) %>% summarise(n())
    ic%>% filter(readmission60==1) %>%group_by(Patient_ID) %>% summarise(n()) %>%nrow()/7320
    
    
    ic$readmission90<-ifelse(!is.na(ic$IC_Hosp_readmission_time) & ic$IC_Hosp_readmission_time<=90, 1, 0)
    ic%>% filter(readmission90==1) %>%group_by(Patient_ID) %>% summarise(n())
    ic%>% filter(readmission90==1) %>%group_by(Patient_ID) %>% summarise(n()) %>%nrow()/7320
    
    
    ic%>% filter(readmission30==1) %>%group_by(Patient_ID) %>%summarise(n=n()) %>% ungroup()%>%
      filter(n>1)
    
    
    test_d<-ic %>%filter(is.na(IC_Hosp_readmission_time) & !is.na(REG_DATE_OF_DEATH)) %>% 
      select (Patient_ID,  IC_Attendance_Date, IC_Discharge_Date,REG_DATE_OF_DEATH,IC_LOS,Type) %>%
     mutate(TimeToD=round(as.numeric(difftime(REG_DATE_OF_DEATH,IC_Discharge_Date, units = 'days'))), 
            death=ifelse (!is.na(TimeToD) & TimeToD<=30,1,0))
     test_d%>%filter(TimeToD>-3 & TimeToD<=30) %>%ggplot(aes(x=TimeToD, fill=Type, color=Type))+geom_density(fill=NA, alpha=0.5) + ggtitle("Time to Death in days")
     
     test_d%>% filter(TimeToD>=0& TimeToD<=30) %>%group_by(Patient_ID) %>% summarise(n())
     test_d%>% filter(TimeToD>=0& TimeToD<=60) %>%group_by(Patient_ID) %>% summarise(n())
     test_d%>% filter(TimeToD>=0& TimeToD<=90) %>%group_by(Patient_ID) %>% summarise(n())
    
    ic<-ic%>% mutate(TimeToD=round(as.numeric(difftime(REG_DATE_OF_DEATH,IC_Discharge_Date, units = 'days'))), 
                     death=ifelse (!is.na(TimeToD) & TimeToD<=30,1,0))
    
    as.data.frame(head(ic))
    
    ic%>%filter(readmission30==1)%>% group_by(death)%>%summarise(n=n(), p.n=n_distinct(Patient_ID))
    
    ic%>%filter(readmission30==0)%>% group_by(death)%>%summarise(n=n(), p.n=n_distinct(Patient_ID))
    
    summ_vars <- c("Patient_ID")
    grp_vars <- c("Sex", "Age_group", "Ethnic_group", 
                             "IMD_quintile", "Comorbidities", "Frailty_index","LTC_Dementia", "IC_LOS", "IC_Hosp_readmission_time")
    
    
  
   ic %>%filter(death!=1 &(is.na(IC_Hosp_readmission_time) | IC_Hosp_readmission_time<=30)) %>% select(readmission30, Age,  Frailty_index, Comorbidities, LTC_Dementia, IC_LOS,  IC_TimeToFirstContact) %>%
     tbl_summary(
      by = readmission30,
      statistic = list(
        all_continuous() ~ "{mean} ({sd})",
        all_categorical() ~ "{n} ({p}%)"
      ),
      digits = all_continuous() ~ 2,
      missing_text = "(Missing)")
   
   ic$readmission30<-as.factor(ic$readmission30)
   ic %>% filter(IC_Hosp_readmission_time<=30) %>%
     ggplot(aes(x=IC_Hosp_readmission_time,  y=Type, fill=Type))+
     geom_density_ridges(alpha=0.3) +
     theme_ridges() + 
     ggtitle("Time to hosp readmission (30 days)")
   # guides(color= guide_legend("Step down IC"), fill=guide_legend("Step downType# guides(color= guide_legend("Step down IC"), fill=guide_legend("Step down IC"))
    
   ic%>%filter(death==0 & IC_Hosp_readmission_time<=30)%>%mutate (h.readm_week=case_when(
                                                       IC_Hosp_readmission_time<=1~'0',
                                                       IC_Hosp_readmission_time>1 &IC_Hosp_readmission_time<=7~'1',
                                                       IC_Hosp_readmission_time>7 &IC_Hosp_readmission_time<=14~'2',
                                                       IC_Hosp_readmission_time>14 &IC_Hosp_readmission_time<=21 ~'3',
                                                       .default = '4+'
                                                       )) %>%group_by(h.readm_week) %>% summarise(n=n()/nrow(ic_r) *100)%>% 
     ggplot(aes(x=h.readm_week, y=n, fill=h.readm_week))+geom_bar(stat="identity")+
     ggtitle("Weeks to hosp readmision")
   
   ic%>%filter(death==0  & IC_Hosp_readmission_time<=30)%>%mutate (h.readm_week=case_when(
     IC_Hosp_readmission_time<=1~'0',
     IC_Hosp_readmission_time>1 &IC_Hosp_readmission_time<=7~'1',
     IC_Hosp_readmission_time>7 &IC_Hosp_readmission_time<=14~'2',
     IC_Hosp_readmission_time>14 &IC_Hosp_readmission_time<=21 ~'3',
     .default = '4+'
   )) %>% filter(h.readm_week=='0') %>%group_by(Type) %>%summarise(n=n()) %>%mutate(n/sum(n))
   
   
   ic_r <- 
     ic%>% 
     filter(
      readmission30 == 1 
     ) %>%
     select(
       Patient_ID,
       Sex = Sex,
       Age_group=Age_group,
       Ethnic_group = Ethnic_group,
       IMD_quintile = IMD_quintile,
       Comorbidities = Comorbidities,
       Frailty_index = Frailty_index,
       IC_LOS,
       IC_TimeToFirstContact,
       LTC_Dementia,
       IC_Hosp_readmission_time,
       Type
     ) 
     
  
   summ_vars <- c("Patient_ID")
   grp_vars <- c("Sex", "Age_group", "Ethnic_group", 
                 "IMD_quintile", "Comorbidities", "Frailty_index","LTC_Dementia",  "IC_LOS","IC_TimeToFirstContact",
                 "IC_Hosp_readmission_time")
   
   out_r<-
     tibble(
       Var1 = grp_vars
     ) %>% 
     rownames_to_column() %>% 
     mutate(nms = paste("T1", rowname, Var1, sep = "_"),
            out = map(.x = Var1, 
                      .f = ~summarise_demographics(
                        df = ic_r, 
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
     out_r %>%
     pull(out) %>% 
     set_names(x = ., out_r$nms)
   
    mean(ic_r$IC_LOS)
    sd(ic_r$IC_LOS)
    median(ic_r$IC_LOS)
    IQR(ic_r$IC_LOS)
    
    ic %>%group_by(Type, readmission30) %>%summarise(n=n_distinct(Patient_ID)) %>% mutate(perc=n/7320) %>% filter(readmission30==1) %>%
      ggplot(aes(x=Type, y=perc, fill=Type))+
    geom_bar(stat="identity")+
      labs(title = "Readmission vs IC Type",
           x = "Type",
           y = "% of IC Patient") +
      scale_fill_brewer(palette = "Set2") +
      theme(text = element_text(size = 20))+
      theme(axis.text.x = element_text(angle = 20, vjust = 0.5, hjust=1))+
      theme(panel.background = element_rect(fill = "white"))
   
    
    
    f_30<-ic%>%filter(death==0 & readmission30==1)
    summary(f$IC_Hosp_readmission_time)
    plt30 <- ggbetweenstats(
      data = as.data.frame(f_30),
      x = Type,
      y = IC_Hosp_readmission_time,
      palette = "Set2",
      title = "Comparison of Time to readmission of IC (up 30 days)",
    )
    plt30
    
    f_60<-ic %>%filter(IC_Hosp_readmission_time<60)
    summary(f$IC_Hosp_readmission_time)
    plt60 <- ggbetweenstats(
      data = as.data.frame(f_60),
      x = Type,
      y = IC_Hosp_readmission_time, 
      palette = "Set2",
      title = "Comparison of Time to readmission of IC (60 days)",
    )
    plt60
    
    f_90<-ic %>%filter(IC_Hosp_readmission_time<90)
    summary(f$IC_Hosp_readmission_time)
    plt90 <- ggbetweenstats(
      data = as.data.frame(f_90),
      x = Type,
      y = IC_Hosp_readmission_time, 
      palette = "Set2",
      title = "Comparison of Time to readmission of IC (up 90 days)",
    )
    plt90
    
    f_all<-ic 
    summary(f$IC_Hosp_readmission_time)
    plt <- ggbetweenstats(
      data = as.data.frame(f_all),
      x = Type,
      y = IC_Hosp_readmission_time, 
      palette = "Set2",
      title = "Comparison of Time to readmission of IC (up 90 days)",
    )
    plt
    
    ic_r %>%filter(is.na(IC_Hosp_readmission_time) | IC_Hosp_readmission_time<=30) %>% select(Type, Age_group,  Frailty_index,Sex, IMD_quintile, LTC_Dementia, IC_LOS,  IC_TimeToFirstContact) %>%
      tbl_summary(
        by = Type,
        statistic = list(
          all_continuous() ~ "{mean} ({sd})",
          all_categorical() ~ "{n} ({p}%)"
        ),
        digits = all_continuous() ~ 2,
        missing_text = "(Missing)")


   

  ############################## Death and readm for NON-IC ##################################################################################################
  
  ic_ids<-ic%>%group_by(Patient_ID)%>%distinct(Patient_ID)
   nic<-final1%>%filter(SDIC==0 & !Patient_ID %in% ic_ids)%>%
    group_by(Patient_ID, REG_DATE_OF_DEATH) %>% 
    summarise(max_hosp_disc=max(Discharge_Date), max_hos_next =max(hosp_next_date), 
              readmission=paste(as.numeric(difftime(hosp_next_date,Discharge_Date, units = 'days')), collapse = "-")) %>%
    mutate(TimeToD=round(as.numeric(difftime(REG_DATE_OF_DEATH,max_hosp_disc, units = 'days'))))

 readmission_nic=data.frame(r30 = nic %>%mutate(read30 = if_else(any(str_split(readmission, "-")[[1]] %>% as.integer() < 30), 1, 0))%>%filter(read30==1) %>%nrow(),
  r60=nic %>%mutate(read60 = if_else(any(str_split(readmission, "-")[[1]] %>% as.integer() < 60), 1, 0))%>%filter(read60==1) %>%nrow(), 
  r90=nic %>%mutate(read90 = if_else(any(str_split(readmission, "-")[[1]] %>% as.integer() < 90), 1, 0))%>%filter(read90==1) %>% nrow())
 
 death_nic<-data.frame(d30=nic %>%filter(TimeToD>=0 & TimeToD<=30) %>%nrow(), d60=nic %>%filter(TimeToD>=0 & TimeToD<=60) %>%nrow(),
                      d90=nic %>%filter(TimeToD>=0 & TimeToD<=90) %>%nrow())
  
 #########################
 #readmission_nic/nrow(nic)
# death_nic/nrow(nic)
 
 #-------------------------------Admission  Reason 
 ic%>%filter(PoD=='EL') %>% select(Diagnostic_Chpt) %>% group_by(Diagnostic_Chpt) %>%summarise(n=n()) %>% mutate(n/sum(n))
 ic%>%filter(PoD=='NEL') %>% select(Diagnostic_Chpt) %>% group_by(Diagnostic_Chpt) %>%summarise(n=n()) %>% mutate(n/sum(n))
 
 
 #==============================Hospital Readmission  and Death per Pathway
 
deaths<- final1%>%group_by(Patient_ID, Discharge_Date, IC_Hosp_readmission_time, REG_DATE_OF_DEATH, Pathway) %>%
  summarize(n()) %>% ungroup()

deaths<- deaths%>%mutate(Readmission=case_when(
  IC_Hosp_readmission_time>0 & IC_Hosp_readmission_time<=30 ~30,
  IC_Hosp_readmission_time>30 & IC_Hosp_readmission_time<=60 ~60,
  IC_Hosp_readmission_time>60 & IC_Hosp_readmission_time<=90 ~90,
  IC_Hosp_readmission_time>90 & IC_Hosp_readmission_time<=180 ~180,
  IC_Hosp_readmission_time>180 ~360)) 

deaths<- deaths%>%mutate(TimeToDeath= round(as.numeric(difftime(REG_DATE_OF_DEATH,Discharge_Date, units = 'days')))) %>%
  mutate(Death = case_when(
    TimeToDeath>0 & TimeToDeath<=30 ~30,
    TimeToDeath>30 & TimeToDeath<=60 ~60,
    TimeToDeath>60 & TimeToDeath<=90 ~90,
    TimeToDeath>90 & TimeToDeath<=180 ~180,
    TimeToDeath>180 ~360))

r_tmp<-deaths%>%group_by(Pathway, Readmission) %>%summarise(c=n()) %>% arrange(Pathway, Readmission) %>%
  mutate(Cum_readm=cumsum(c)) %>%mutate(perc=Cum_readm/sum(Cum_readm) *100)

  
as.data.frame(r_tmp)

d_tmp<-deaths%>%group_by(Pathway, Death) %>%summarise(c=n()) %>% arrange(Pathway, Death) %>%
  mutate(Cum_death=cumsum(c)) %>%mutate(perc=Cum_death/sum(Cum_death) *100)


as.data.frame(d_tmp)



 