
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

# etl ----------------------------------------------------------------
# df <- 
#   get_query(read_file(file = "sql/HOSP_SD_IC_Activ_contact_AB.sql")) %>% 
#   as_tibble() %>% 
#   janitor::clean_names()

source("Hospital_IC_Continuity_AB_src.R")

death<-get_query("select * from  Warehouse_LDM.crd.Deaths")
str(death)
death<-death %>% filter(REG_DATE_OF_DEATH>"2022-03-31" & REG_DATE_OF_DEATH<"2023-10-01")

final2<-final1 
temp<-death %>% select(PSEUDO_DEC_NHS_NUMBER, REG_DATE_OF_DEATH)

final2<-final2%>%left_join(temp, join_by(Patient_ID == PSEUDO_DEC_NHS_NUMBER))

table(final2$REG_DATE_OF_DEATH, final2$Type)
                           