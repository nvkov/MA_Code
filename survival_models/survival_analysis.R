#Survival analysis:

#Correct Leistung:

rm(list=ls())
library("data.table")
library("sets")
library("survival")
library("stargazer")

#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<-"/Pkw/MobileDaten/generatedData/final_dataset/"

wd<- paste0(project_directory, data_directory)
setwd(wd)

#Load dataset:
load("dataset_full.RData")

df$newTOM<- as.numeric(df$prices_lastDate-df$prices_firstDate) +1
df$status<- abs(df$right_censor - 1)
df$year_bought<-as.factor(format(df$prices_lastDate, "%Y")) 

# Survival models ---------------------------------------------------------
relCols<- c("valuePrice", "newTOM", "MS","DOP", "Quantile", "age", "Leather_seats", 
            "Full_service_history", "Xenon_lights", "color_cat", "year_bought", 
            "Leistung", "Typ","status", "vendor_ID", "car_ID")
df1<- df[df$DOP<=2,relCols, with=F]
df1$newDOP<- df1$DOP*10

# Generate some vendor profile variables ----------------------------------

df1<- df1[ ,size_vendor:=length(unique(car_ID)), by=vendor_ID]
df1$size_vendor<- df1$size_vendor/100

df1<- df1[,times_present_diff_price:=1:.N, by=car_ID]
df1<- d1[, total_price_reduction]
# Begin survivavl analysis ------------------------------------------------

fitCPH1 <- coxph(Surv(newTOM, status, type="right") ~ MS+ newDOP + Quantile + age + 
                   Leather_seats +Full_service_history + valuePrice+
                   Xenon_lights + color_cat + year_bought +size_vendor + times_present_diff_price +strata(Typ), data=df1)
summary(fitCPH1)
#(res.zph1 <- cox.zph(fitCPH1))

fitCPH_A150<-coxph(Surv(newTOM, status) ~ MS+ DOP + Quantile + age + 
                     Leather_seats +Full_service_history +
                     Xenon_lights + color_cat + year_bought, data=df1[df1$Typ=="A150"])
summary(fitCPH_A150) 

fitCPH_A180<-coxph(Surv(newTOM, status) ~ MS+ DOP + Quantile + age + 
                     Leather_seats +Full_service_history +
                     Xenon_lights + color_cat + year_bought, data=df1[df1$Typ=="A180"])
summary(fitCPH_A180) 


fitCPH_A200<-coxph(Surv(newTOM, status) ~ MS+ DOP + Quantile + age + 
                     Leather_seats +Full_service_history +
                     Xenon_lights + color_cat + year_bought, data=df1[df1$Typ=="A200"])
summary(fitCPH_A200) 

fitCPH_A140<-coxph(Surv(newTOM, status) ~ MS+ DOP + Quantile + age + 
                     Leather_seats +Full_service_history +
                     Xenon_lights + color_cat + year_bought, data=df1[df1$Typ=="A140"])
summary(fitCPH_A140) 

fitCPH_A160<-coxph(Surv(newTOM, status) ~ MS+ DOP + Quantile + age + 
                     Leather_seats +Full_service_history +
                     Xenon_lights + color_cat + year_bought, data=df1[df1$Typ=="A160"])
summary(fitCPH_A160)

fitCPH_A170<-coxph(Surv(newTOM, status) ~ MS+ DOP + Quantile + age + 
                     Leather_seats +Full_service_history +
                     Xenon_lights + color_cat + year_bought, data=df1[df1$Typ=="A170"])
summary(fitCPH_A170) 

fitCPH_A190<-coxph(Surv(newTOM, status) ~ MS+ DOP + Quantile + age + 
                     Leather_seats +Full_service_history +
                     Xenon_lights + color_cat + year_bought, data=df1[df1$Typ=="A190"])
summary(fitCPH_A190)

fitCPH_A210<-coxph(Surv(newTOM, status) ~ MS+ DOP + Quantile + age + 
                     Leather_seats +Full_service_history +
                     Xenon_lights + color_cat + year_bought, data=df1[df1$Typ=="A210"])
summary(fitCPH_A210) 


sink("C:/Users/Nk/Documents/Uni/MA/MA_Code/Results/tab_cphA.tex")
stargazer(fitCPH_A140,fitCPH_A150,fitCPH_A160,fitCPH_A170, fitCPH_A180,
          fitCPH_A190 ,fitCPH_A200,fitCPH_A210,
          apply.coef = exp,
          apply.se = exp, 
          label="tab:cphA",
          title="Cox proportional hazards. Comparing buyer preferences for the Mercedes-Benz A-Class", 
          covariate.labels = c("Market size", "DOP", "Quantile",
                               "Age", "Leather seats", "Full service history", 
                               "Xenon lights", "Color"),
          dep.var.caption  = "Dependent variable",
          dep.var.labels   = "Time on market (in days)",
          column.labels = c("A140","A150", "A160","A170", "A180", "A190", "A200", "A210"), 
          summary=F, 
          align=TRUE)
sink()

fitCPH_year<-coxph(Surv(newTOM, status) ~ MS+ DOP + Quantile + age + 
                     Leather_seats +Full_service_history +
                     Xenon_lights + color_cat + strata(year_bought), data=df1[df1$Typ=="A210"])
summary(fitCPH_year) 

fitCPH_year<-coxph(Surv(newTOM, status) ~ MS+ DOP + Quantile + age + 
                     Leather_seats +Full_service_history +
                     Xenon_lights + color_cat + year_bought , data=df1)
summary(fitCPH_year) 



# Some descriptive statistics ---------------------------------------------

mean(df1$newTOM[df1$year_bought==2008])
mean(df1$newTOM[df1$year_bought==2009])
mean(df1$newTOM[df1$year_bought==2010])
mean(df1$newTOM[df1$year_bought==2011])
mean(df1$newTOM[df1$year_bought==2012])

average_TOMs<- matrix(NA, 72,6)
for(i in unique(df1$Typ)){
  names<- unique(df1$Typ)
  average_TOMs[as.numeric(names[which(names==i)]), 1]<- i
  average_TOMs[as.numeric(names[which(names==i)]), 2]<- mean(df1$newTOM[df1$year_bought==2008 & df1$Typ==i])
  average_TOMs[as.numeric(names[which(names==i)]), 3]<- mean(df1$newTOM[df1$year_bought==2009& df1$Typ==i])
  average_TOMs[as.numeric(names[which(names==i)]), 4]<- mean(df1$newTOM[df1$year_bought==2010& df1$Typ==i])
  average_TOMs[as.numeric(names[which(names==i)]), 5]<- mean(df1$newTOM[df1$year_bought==2011& df1$Typ==i])
  average_TOMs[as.numeric(names[which(names==i)]), 6]<- mean(df1$newTOM[df1$year_bought==2012& df1$Typ==i])
  colnames(average_TOMs)<- c("Type", "y2008", "y2009", "y2010", "y2011", "y2012")
  }

rownames(average_TOMs)<- average_TOMs[,1]
average_TOMs<- average_TOMs[,-1]




