library(dplyr)
library(ggplot2)
library(corrplot)
library(RColorBrewer)
library(caTools)
library(vtree)
library(ggfortify)

#CONVERTING SPSS FILE TO CSV
#df <- read.spss("CCHS_Annual_2017_2018.sav", use.value.label=TRUE, to.data.frame=TRUE, na.rm = TRUE)
#write.csv(df, "CCHS_Annual17.csv")
#data<- read.csv("CCHS_Annual17.csv")

#READING FROM CSV FILE
data <- read.csv(file = "CCHS_Annual.csv")

# sub-seting the  necessary columns to be used from the dataset
data<- subset(data,select = c(DHH_SEX,DHHGAGE,VERDATE, HWTDGISW,GEN_015,GEN_020, INCDGHH, WDM_005, WDM_010, WDM_015, WDM_030, PAADVACV, EHG2DVR3, ALCDVTTM,DHHDGHSZ, DHHGMS,LBFDVWSS, SMKDVSTY,SDCDVIMM,PAY_045, GEO_PRV, GEODGHR4, ADM_045, CCC_140, CCC_095, CCC_065, CIH_030H, DOFSC, CCC_085, CCC_130, DOCP2, UPE_010, SDCDGCB, FVCDVFRU, PHC_040,GEN_005))
data



str(data)


#Creating valid levels for labels

data <- data  %>% 
mutate(HWTDGISW = case_when(data$HWTDGISW >=  'Normal weight' ~ 1,
                              data$HWTDGISW >=  'Obese - Class I, II, III'~ 2,
                              data$HWTDGISW >=  'Overweight '~ 3,
                              data$HWTDGISW >= 'Underweight'~ 4))

str(data)

data <- data  %>% 
  mutate(SMKDVSTY = case_when(data$SMKDVSTY >=  'Current daily smoker' ~ 1,
                              data$SMKDVSTY >=  'Current occasional smoker'~ 2,
                              data$SMKDVSTY >=  'Experimental smoker (at least 1 cig, non-smoker now)'~ 3,
                              data$SMKDVSTY >= 'Former daily smoker (non-smoker now)l'~ 4,
                              data$SMKDVSTY >= 'Former occasional smoker (non-smoker now)'~ 5,
                              data$SMKDVSTY <= 'Lifetime abstainer (never smoked a whole cigarette)'~ 6))
str(data)

data <- data  %>% 
  mutate(GEN_020 = case_when(data$GEN_020 >=  'A bit stressful' ~ 1,
                             data$GEN_020 >=  'Extremely stressful'~ 2,
                             data$GEN_020 >=  'Not at all stressful '~ 3,
                             data$GEN_020 >= 'Not very stressful'~ 4,
                             data$GEN_020 >= 'Not very stressful'~ 5,
                             data$GEN_020 <= 'Quite a bit stressful'~ 6))

str(data)

data <- data  %>% 
  mutate(ALCDVTTM = case_when(data$ALCDVTTM >=  'Did not drink in the last 12 months' ~ 1,
                              data$ALCDVTTM >=  'Occasional drinker'~ 2,
                              data$ALCDVTTM >=  'Regular drinker'~ 3))

str(data)

data <- data  %>% 
  mutate(INCDGHH = case_when(data$INCDGHH >=  '$20,000 to $39,999' ~ 1,
                             data$INCDGHH >=  '$40,000 to $59,999'~ 2,
                             data$INCDGHH >=  '$60,000 to $79,999 '~ 3,
                             data$INCDGHH >= '$80,000 or more'~ 4,
                             data$INCDGHH >= 'No income or less than $20,000'~ 5))

str(data)

unique(data$GEN_015)
data <- data  %>% 
  mutate(GEN_015 = case_when(data$GEN_015 >=  '0' ~ 0,
                             data$GEN_015 >=  'Poor'~ 1,
                             data$GEN_015 >=  'Fair'~ 2,
                             data$GEN_015 >=  'Good'~ 3,
                             data$GEN_015 >=  'Very good'~ 4,
                             data$GEN_015 >=  'Excellent'~ 5))

str(data)

data <- data  %>% 
  mutate(WDM_005 = case_when(data$WDM_005 >=  'A lot of difficulty' ~ 1,
                             data$WDM_005 >=  'Cannot do at all / Unable to do' ~2,
                             data$WDM_005 >=  'No difficulty '~ 3,
                             data$WDM_005 >= 'Some difficulty'~ 4))

data <- data  %>% 
  mutate(WDM_010 = case_when(data$WDM_010 >=  'A lot of difficulty' ~ 1,
                             data$WDM_010 >=  'Cannot do at all / Unable to do' ~2,
                             data$WDM_010 >=  'No difficulty '~ 3,
                             data$WDM_010 >= 'Some difficulty'~ 4))

data <- data  %>% 
  mutate(WDM_015 = case_when(data$WDM_015 >=  'A lot of difficulty' ~ 1,
                             data$WDM_015 >=  'Cannot do at all / Unable to do' ~2,
                             data$WDM_015 >=  'No difficulty '~ 3,
                             data$WDM_015 >= 'Some difficulty'~ 4))

data <- data  %>% 
  mutate(WDM_030  = case_when(data$WDM_030  >=  'A lot of difficulty' ~ 1,
                              data$WDM_030  >=  'Cannot do at all / Unable to do' ~2,
                              data$WDM_030  >=  'No difficulty '~ 3,
                              data$WDM_030  >= 'Some difficulty'~ 4))

str(data)

data <- data  %>% 
  mutate(PAADVACV = case_when(data$PAADVACV >=  'No physical activity minutes reported' ~ 1,
                              data$PAADVACV >=  'Physically active at / above recommended level from CPAG'~ 2,
                              data$PAADVACV >=  'Physically active below recommended level from CPAG'~ 3))




str(data)

data <- data  %>% 
  mutate(EHG2DVR3 = case_when(data$EHG2DVR3 >=  'Less than secondary school graduation' ~ 1,
                              data$EHG2DVR3 >=  'Post-secondary certificate diploma or univ degree'~ 2,
                              data$EHG2DVR3 >=  'Secondary school graduation, no post-secondary education'~ 3))

str(data)

data <- data  %>% 
  mutate(DHHGMS = case_when(data$DHHGMS >=  'Common-law' ~ 1,
                            data$DHHGMS >=  'Married'~ 2,
                            data$DHHGMS >=  'Single'~ 3,
                            data$DHHGMS >=  'Widowed/Divorced/Separated'~ 4))

str(data)


data <- data  %>% 
  mutate(LBFDVWSS = case_when(data$LBFDVWSS >=  '0' ~ 1,
                              data$LBFDVWSS >=  'Did not have a job - last week'~ 2,
                              data$LBFDVWSS >=  'Worked at a job / business - last week'~ 3))

str(data)


data <- data  %>% 
  mutate(SDCDVIMM = case_when(data$SDCDVIMM >=  'Landed immigrant / non-permanent resident' ~ 1,
                              data$SDCDVIMM >=  'Non-immigrant (Canadian born)'~ 2))

str(data)

data <- data  %>% 
  mutate(CCC_095  = case_when(data$CCC_095  >=  'Yes' ~ 1,
                              data$CCC_095  >=  'No'~ 2))
data <- data  %>% 
  mutate(CCC_065   = case_when(data$CCC_065   >=  'Yes' ~ 1,
                               data$CCC_065   >=  'No'~ 2))

data <- data  %>% 
  mutate(DOFSC  = case_when(data$DOFSC  >=  'Yes' ~ 1,
                            data$DOFSC  >=  'No'~ 2))
data <- data  %>% 
  mutate(CCC_085   = case_when(data$CCC_085   >=  'Yes' ~ 1,
                               data$CCC_085   >=  'No'~ 2))
data <- data  %>% 
  mutate(CCC_130   = case_when(data$CCC_130   >=  'Yes' ~ 1,
                               data$CCC_130   >=  'No'~ 2))
data <- data  %>% 
  mutate(DOCP2     = case_when(data$DOCP2     >=  'Yes' ~ 1,
                               data$DOCP2     >=  'No'~ 2))

data <- data  %>% 
  mutate(SDCDGCB  = case_when(data$SDCDGCB   >=  'Canada' ~ 1,
                              data$SDCDGCB >=  'Other'~ 2))
str(data)


data <- data  %>% 
  mutate(PHC_040 = case_when(data$PHC_040 >=  'Another language' ~ 1,
                             data$PHC_040 >=  'English'~ 2,
                             data$PHC_040 >=  'English and another language)'~ 3,
                             data$PHC_040 >= 'English and French'~ 4,
                             data$PHC_040 >= 'French'~ 5,
                             data$PHC_040 <= 'French and another language)'~ 6))
str(data)

is.na(data)<-sapply(data, is.infinite)
data[is.na(data)]<-0
sum(is.na(data))
mean(is.na(data))


data <- na.omit(data)
sum(is.na(data))


province_age <- table(data$GEO_PRV,data$DHHGAGE)
province_age


age<- table(data$DHHGAGE)
age

province <- table(data$GEO_PRV)
province

region<- c(data$GEODGHR4)
region



status <- table(data$SDCDVIMM,data$DHH_SEX)
status
prop.table(status)


vtree(data, c("SDCDVIMM", "DHH_SEX"))


vtree(data, c("SDCDVIMM", "ALCDVTTM"))

library(vtree)
vtree(data, c("GEN_005","SMKDVSTY"))

# 1. Mental Health of Immigrants in Canada:

#ANOVA TEST FOR BMI 
anov <- aov(HWTDGISW ~ (GEN_020 + SMKDVSTY + ALCDVTTM), data = data, na.action=na.exclude)
summary(anov)

#LR FOR BMI
simple.fit = lm(HWTDGISW ~ (GEN_020 + SMKDVSTY + ALCDVTTM), data=data)
summary(simple.fit)

str(data$SDCDVIMM)

#ANOVA FOR IMMIGRANTS BY VARIABLE SMOKING, DRINKING, AND PERCEIVED LIFE STRESS
anov1 <- aov(SDCDVIMM ~ (GEN_020 + SMKDVSTY + ALCDVTTM), data = data, na.action=na.exclude)
summary(anov1)


#LR FOR IMMIGRANTS BY VARIABLE SMOKING, DRINKING, AND PERCEIVED LIFE STRESS
simple.fit1 = lm(SDCDVIMM ~ (GEN_020 + SMKDVSTY + ALCDVTTM), data=data)
summary(simple.fit1)

anova(simple.fit1)
#PLOTTING LR ON BASIS OF GENDER
ggplot(data,aes(y=SMKDVSTY+GEN_020+ALCDVTTM, x=SDCDVIMM ,color=factor(DHH_SEX)))+geom_point()+stat_smooth(method="lm",se=FALSE)+ ylab("SMOKING,DRINKING,LIFESTRESS")+
  xlab("IMMIGRANTS")

#PLOTTING LR ON BASIS OF AGE
ggplot(data,aes(y=SMKDVSTY+GEN_020+ALCDVTTM, x=SDCDVIMM ,color=factor(DHHGAGE)))+geom_point()+stat_smooth(method="lm",se=FALSE)+ ylab("SMOKING,DRINKING,LIFESTRESS")+
  xlab("IMMIGRANTS")

#PLOTTING LR ON BASIS OF perceived health
ggplot(data,aes(y=SMKDVSTY+GEN_020+ALCDVTTM, x=SDCDVIMM,color=factor(GEN_005)))+geom_point()+stat_smooth(method="lm",se=FALSE)+ ylab("SMOKING,DRINKING,LIFESTRESS")+
  xlab("IMMIGRANTS")


# 2. Diseases Analyzing the ratio of selected health conditions such as Diabetes,
#Health Diseases, Cancer, Blood Pressure among Immigrants vs Canadian Residents

data$age  <- data$DHHGAGE
data$age  <- ifelse(data$age  %in% c("Age between 12 and 14","Age between 18 and 19", "Age between 15 and 17"), 1, 2)

subset<- data[data$age == 2, ] 

#anova and linear regression test
disease.fit = lm(SDCDVIMM ~ (CCC_095 + CCC_085 + CCC_130+ CCC_065), data=data)
summary(disease.fit)
anova(disease.fit)

#analysis of diseases among immigrants
ggplot(subset,aes(y=CCC_095 + CCC_085 + CCC_130+ CCC_065, x=SDCDVIMM,color=factor(DHH_SEX )))+geom_point()+stat_smooth(method="lm",se=FALSE)+ ylab("Chronic Diseases")+
  xlab("IMMIGRANTS")


#VISIT TO HEALTH PROFESSIONAL
ggplot(data,aes(y=age + DOCP2+ INCDGHH ,x= CCC_095 + CCC_085 + CCC_130+ CCC_065 ,color=factor(DHHGAGE)))+geom_point()+stat_smooth(method="lm",se=FALSE)+xlab("Chronic Diseases")+
  ylab("visits to health proffesionals by people, income factor")


