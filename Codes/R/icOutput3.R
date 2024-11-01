library(Amelia)
library(ggplot2)
library(tidyverse)
library(mice)
library(furrr)

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

# etl ----------------------------------------------------------------
# df <- 
#   get_query(read_file(file = "sql/HOSP_SD_IC_Activ_contact_AB.sql")) %>% 
#   as_tibble() %>% 
#   janitor::clean_names()

source("Hospital_IC_Continuity_AB_src.R")
#saveRDS(final1, file="../Data/final1.Rds")
#final1<-readRDS("../Data/final1.Rds")

missingness<-function(data)
{
  missing_vector<-apply(data, 2, function(x) sum(is.na(x)))
  missing_prop<-missing_vector/nrow(data)
  

  miss_prop<-data.frame(prop=missing_prop)
  
  miss_prop$names<-rownames(miss_prop)
  miss_prop_sel<-miss_prop[miss_prop$prop>0,]
  return(miss_prop_sel)
}

#plot data distribution
plot_hist<-function(x)
{
  nc <- max(5, ceiling(sqrt(ncol(x))))
  nr <- ceiling(ncol(x) / nc)
  par(mfrow = c(nr, nc), mgp = c(2, 0.6, 0), mar = c(2, 3, 3, 0.5))
  for (i in 1:ncol(x)) {
    if (is.numeric(x[, i])) {
      hist(x[, i], nclass = 50, xlab = "",
           main = paste0(names(x[i]), " (",
                         round(mean(is.na(x[, i])) * 100, 2), "% NA)")
      )
    } else {
      barplot(table(x[, i]), ylab = "Frequency",
              main = paste0(names(x[i]), " (",
                            round(mean(is.na(x[, i])) * 100, 2), "% NA)"))
    }
  }
}

##------------------------------------roll ic comunity

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
    GP_Count, 
    IC_Count,
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
    Age_group = fct_explicit_na(f = factor(x = Age_group, ordered = T), 
                                na_level = "Unknown"),
    #Comorbidities = fct_explicit_na(f = factor(x = Comorbidities, ordered = T), 
    #   na_level = "Unknown"),
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
    .default = Type))
                                                                                                                                  
ic$readmission30<-ifelse(!is.na(ic$IC_Hosp_readmission_time) & ic$IC_Hosp_readmission_time<=30, 1, 0)

ic<-ic%>% mutate(TimeToD=round(as.numeric(difftime(REG_DATE_OF_DEATH,IC_Discharge_Date, units = 'days'))), 
                 death=ifelse (!is.na(TimeToD) & TimeToD<=30,1,0))

#---------------------select variables for log reg

final2<-ic %>% filter(death==0) %>% select(GP_Count,IC_Count, Age, Sex, 
                          Frailty_index, Comorbidities, LTC_Dementia, Ethnic_group, HLOS, PoD , 
                          Type,  IC_LOS, IC_TimeToFirstContact, IC_Hosp_readmission_time, readmission30)


numerical_variables <- final2 %>% select_if(is.numeric) 
numerical_variables<-na.omit(numerical_variables)
apply(numerical_variables, 2, sd)
library(corrplot)
correl<-cor(numerical_variables)
corrplot(correl)



final_sdic<-final2

table(final_sdic$Type)

numerical_variables <- final_sdic %>% select_if(is.numeric)
correl<-cor(numerical_variables)
library(corrplot)
corrplot(correl)

#lsoa - too many categories



#get missingness
miss_prop_sel<-missingness(final_sdic)
ggplot(miss_prop_sel, aes(x=names, y=prop, fill=names))+geom_bar(stat="identity") +coord_flip()
#missmap(final)



final_sdic[sapply(final_sdic, is.character)] <- lapply(final_sdic[sapply(final_sdic, is.character)], as.factor)
final_sdic$readmission30<-as.factor(final_sdic$readmission30)


###imputation

#plot data distribution for the final dataset
#plot_hist(final_sdic)


 #############################################################
#impute



final_sdic<-final_sdic %>%select(-IC_Hosp_readmission_time)


library(mice)
imp2 <- mice(final_sdic, maxit = 1, method = 'rf')
#imp2 <- futuremice(final,  n.core = 5, n.imp.core = 5)
imp2$method

#check covergence
plot(imp2, layout = c(6, 6))
densityplot(imp2)



#save 10 models for each iteration if imputation
#save_best_models=10

#save 100 models for each iteration if no or maxit =1 imputation
save_best_models=100

var_list<-as.list(rep(0, ncol(final_sdic)))
names(var_list)<-names(final_sdic)
#store formulas
formulas<-NULL
#BIC for top models
top<-NULL

#iterate over the imputation

library(caret)
set.seed(123)
ids<-createDataPartition(final_sdic$readmission30, p=0.8, list=F)
#
#for(i in 1:imp2$m)
i=1
{
  completed <-complete(imp2, i)
  train <-completed[ids,]
  test<-completed[-ids,]


################### GLUMULTI ################################
  library(glmulti)
  glmulti.logistic.out <- glmulti(readmission30 ~ ., data = train, 
          level=1,                 # main effect, change to 2 if you want a pairwise interactions
          method = "h",            # h Exhaustive approach #g genetic algorithm
          crit = "bic",            # BIC as criteria
          confsetsize = save_best_models,       # Keep n best models
          plotty = F, report = T,  # No plot or interim reports
          fitfunction = "glm",     # glm function
          family = binomial)       # binomial family for logistic regression

  glm_bic<-glmulti.logistic.out 
  ## Show 10 best models 
  glm_bic@formulas
  
  #print top models based on BIC 
  if(is.null(top)){
    top <- weightable(glm_bic)
  } else { 
    top_n <- weightable(glm_bic)
    top<-rbind(top,top_n)
  }
 

  for (l in 1:length(glm_bic@formulas))
  {
    #extract variables from formula fro best model 
    var<-all.vars((glm_bic@formulas[[l]])[[3]])
    formulas<-c(formulas,  glm_bic@formulas[[l]])
    for(k in var)
    {
      var_list[[k]]<-var_list[[k]]+1
    }
  }

}  
  #After iteration over imputations:
  #Plot the variable importance
  var_usage<-data.frame(count = sort(unlist(var_list)/100, decreasing = T))
  var_usage$name<-rownames(var_usage)
  var_usage$name <- factor(var_usage$name, levels = var_usage$name)
  
  ggplot(var_usage, aes(x=name, y=count))+geom_bar(stat="identity") +coord_flip()
  
  #which treshold to use to select the frequent variables: 0.6
  frequent_var<-rownames(var_usage[which(var_usage$count>=0.6),])
  
  #get formulas that have frequent variables
  final_formulas<-list()
  for( i in 1:length(formulas))
  {
    if(all(frequent_var %in% all.vars(formulas[[i]])))
    {
      final_formulas<-c(final_formulas, formulas[[i]])
    }
  }
  



plot(glm_bic)
#The horizontal red line differentiates between models whose AICc value is less versus more than 2 
#units away from that of the "best" model (i.e., the model with the lowest AICc). The output above shows 
#that there are 3 such models. 

#print top models based on BIC 

top <- weightable(glm_bic)
top <- top[top$bic <= min(top$bic) + 5,]
top

summary(glm_bic@objects[[1]])
exp(glm_bic@objects[[1]]$coefficients[-1])
library(coefplot)
coefplot(glm_bic@objects[[1]], innerCI=2, outerCI=0, intercept=F)
coefplot(glm_bic@objects[[1]])
print(glm_bic@objects[[1]])


#The values under weights are the model weights (also called "Akaike weights"). 
#From an information-theoretic perspective, the Akaike weight for a particular model can be regarded as 
#the probability that the model is the best model (in a Kullback-Leibler sense of minimizing the loss of 
#information when approximating full reality by a fitted model) out of all of the models considered/fitted.

#The importance value for a particular predictor is equal to the sum of the weights/probabilities for 
#the models in which the variable appears. 
#a variable that shows up in lots of models with large weights will receive a high importance value.
plot(glm_bic, type="s")
plot(glm_bic, type="r")
plot(glm_bic, type="w")
#get aic from the list of models: use aic() function
AIC <- rep(0, nrow(top))
model <- rep(NA, nrow(top))
AUC <- rep(0, nrow(top))
RSQUARED <- rep(0, nrow(top))
residuals_fit <- list()
residuals_plot<-list()


plot <- list()
for(i in 1:nrow(top)){
  fit <- glm(paste(as.character(glm_bic@formulas[i])), data = train, family = binomial)
  residuals_fit[[i]]<-residuals(fit)
  model[i] <- paste(as.character(glm_bic@formulas[i]))
  AIC[i] <- fit$aic
  predictpr <- predict(fit, type = "response")
  plot[[i]]<-ggplot(data.frame(x=train$readmission30, y=predictpr), aes(x=x,y=y))+geom_point()
  residuals_plot[[i]]<-ggplot(data.frame(x=predictpr, y=residuals_fit[[i]]), aes(x=x,y=y))+geom_point()+title("Fitted vs residuals")
  ROC <- pROC::roc(train$readmission30 ~ predictpr)
  AUC[i] <- pROC::auc(ROC)
  #The McFadden definition of pseudo-Rsquared
  RSQUARED[i] <- 1 - (fit$deviance/fit$null.deviance)
}

#plot predicted vs observed
library(gridExtra)
n <- length(plot)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(plot, ncol=nCol))
#plot fitted vs resid
library(gridExtra)
n <- length(residuals_plot)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(residuals_plot, ncol=nCol))


INDEX <- seq(1:nrow(top))
glm_bic.fits <- data.frame(INDEX, model, AIC, RSQUARED, AUC)
glm_bic.fits

combine_train<-data.frame()
combine_test<-data.frame()

for(i in 1:nrow(top)){
  fit <- glm(paste(as.character(glm_bic@formulas[1])), data = train, family = binomial)
  predictpr <- predict(fit, type = "response")
  any(abs(predictpr)>1)
  train1 <- cbind(train,predictpr)
  train1$PredClass<-ifelse(as.numeric(train1$predictpr)>0.5, 1, 0)
  train1$PredClass<-as.factor(train1$PredClass)
  train1$Class<-as.factor(train1$readmission30)
  cm_train<-confusionMatrix(train1$Class, train1$PredClass, pos='1')
  combine_train<-rbind(combine_train, cm_train$byClass)
  #names(combine_train)<-names(cm_train$byClass)
  #combine_train<- combine_train[,c("Balanced Accuracy", "Sensitivity", 
  #                     "Specificity", "Precision", "Recall", "F1")] 
  
  #plot(pROC::roc(train1$PercentageAttendance ~ train1$predictpr), main = "ROC for best model")
  
  #test the performance on the test dataset
  predictpr <- predict(fit, newdata=test, type = "response")
  any(abs(predictpr)>1)
  test1 <- cbind(test,predictpr)
  test1$PredClass<-ifelse(as.numeric(test1$predictpr)>0.5, 1, 0)
  test1$PredClass<-as.factor(test1$PredClass)
  test1$Class<-as.factor(test1$readmission30)
  cm_test<-confusionMatrix(test1$Class, test1$PredClass, pos='1')
  combine_test<-rbind(combine_test, cm_test$byClass)
  #names(combine_test)<-names(cm_test$byClass)
  #combine_test<- combine_test[,c("Balanced Accuracy", "Sensitivity", 
  #                     "Specificity", "Precision", "Recall", "F1")]  
}
names(combine_test)<-names(cm_test$byClass)
combine_test<- combine_test[,c("Balanced Accuracy", "Sensitivity", 
                               "Specificity", "Precision", "Recall", "F1")]  


str(final_sdic)

library(ggstatsplot)
f<-ic%>%filter(IC_Hosp_readmission_time<30)
plt <- ggbetweenstats(
  data = as.data.frame(f),
  x = Sex,
  y = IC_Hosp_readmission_time
)
plt


f<-ic%>%filter(IC_Hosp_readmission_time<90)
plt <- ggbetweenstats(
  data = as.data.frame(f),
  x = Type,
  y = IC_Hosp_readmission_time
)
plt

f<-ic%>%filter(IC_Hosp_readmission_time<30)
plt <- ggbetweenstats(
  data = as.data.frame(f),
  x = PoD,
  y = IC_Hosp_readmission_time
)
plt

plot(f$Frailty_index,f$IC_Hosp_readmission_time)
plot(f$IC_LOS,f$IC_Hosp_readmission_time)
plot(f$IC_TimeToFirstContact,f$IC_Hosp_readmission_time)

ic %>% group_by(readmission30, PoD) %>% summarise(n=n()) %>% mutate(Fraq=n/sum(n))%>%
  ggplot(aes(y=Fraq, x=PoD, fill=readmission30))+
  geom_bar(stat="identity", position=position_dodge())+
  ggtitle("Hospital Readmission") 
  #facet_wrap(~PHM_Segment) +



ic %>% group_by(readmission30, Sex) %>% summarise(n=n()) %>% mutate(Fraq=n/sum(n))%>%
  ggplot(aes(y=Fraq, x=Sex, fill=as.factor(readmission30)))+
  geom_bar(stat="identity", position=position_dodge())+
  ggtitle("Hospital Readmission")

library(ggstatsplot)
plt <- ggbetweenstats(
  data = as.data.frame(ic),
  x = readmission30,
  y = IC_Count
)
plt

ic%>%ggplot(aes(x=IC_Count, y= readmission30, fill=readmission30))+
  geom_density_ridges(alpha=0.3) +
  theme_ridges() + 
  ggtitle("IC_Coun  vs t Readmission") 

ic%>%mutate(ICC=case_when(
  IC_Count<100  ~ "-100",
  IC_Count>=100 & IC_Count<200  ~ "100-200",
  IC_Count>=200 & IC_Count<300  ~ "200-300",
  IC_Count>=300 & IC_Count<400  ~ "300-400",
  IC_Count>=400~ "400+" ))%>% group_by(ICC) %>% summarise(n=n(),
                readmission_rate=mean(as.numeric(as.character(readmission30))))%>%
  ggplot(aes(x=ICC, y= readmission_rate, fill=ICC))+
  geom_bar(stat="identity")+
  ggtitle("IC_Count  vs  Readmission") 

ic%>%ggplot(aes(x=Frailty_index, y= readmission30, fill=readmission30))+
  geom_density_ridges(alpha=0.3) +
  theme_ridges() + 
  ggtitle("Frailty Readmission") 

plt <- ggbetweenstats(
  data = as.data.frame(ic),
  x = readmission30,
  y = Frailty_index
)
plt


ic%>%ggplot(aes(y=Frailty_index, x= as.factor(readmission30), fill=as.factor(readmission30)))+
  geom_boxplot()
ggtitle("HLOS Readmission") 


plt <- ggbetweenstats(
  data = as.data.frame(ic),
  x = readmission30,
  y = HLOS
)
plt


final_sdic%>%mutate(HLOSG=case_when(
  HLOS<50  ~ "1-50",
  HLOS>=50 & HLOS<100  ~ "50-100",
  HLOS>=100 & HLOS<200  ~ "100-200",
  HLOS>=200~ "200+" ))%>% group_by(HLOSG) %>% summarise(n=n(),
                        readmission_rate=mean(as.numeric(as.character(readmission30))))%>%
  mutate(HLOSG=fct_relevel(HLOSG, "1-50",  "50-100","100-200", "200+" )) %>%
  ggplot(aes(x=HLOSG, y= readmission_rate, fill=HLOSG))+
  geom_bar(stat="identity")+
  ggtitle("HLOS Readmission") 

#check discharge place
  



final1 %>% select(IC_Hosp_readmission,GP_Count, UC111_Count, UC999_Count, UC111_Count,UCOOH_Count, AE_Count, IC_Count, ASCCOM_Count, ComServ_Count) %>%tbl_summary(
  by = IC_Hosp_readmission,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} / {N} ({p}%)"
  ),
  digits = all_continuous() ~ 2,
  #label = grade ~ "Tumor Grade",
  missing_text = "(Missing)")


              