#Survival analysis:

#Correct Leistung:

rm(list=ls())
library("data.table")
library("sets")
library("survival")
library("stargazer")
library("beepr")

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
            "Leistung", "Typ","status", "vendor_ID", "car_ID", "prices_firstDate", "prices_lastDate") 
df1<- df[df$DOP<=2 & df$newTOM<400,relCols, with=F]
df1$newDOP<- df1$DOP*10
df1$age<- df1$age/30
# Generate some vendor profile variables ----------------------------------

df1<- df1[ ,size_vendor:=length(unique(car_ID)), by=vendor_ID]
df1$size_vendor<- df1$size_vendor/100

df1<- df1[,times_present_diff_price:=1:.N, by=car_ID]
df1<- d1[, total_price_reduction]
df1$valuePrice100<- df1$valuePrice/100
df1$Class<- gsub("[0-9]", "" ,df1$Typ)
df1$Hub_Cat<- gsub("[A-Z]", "", df1$Typ)
# Begin survivavl analysis ------------------------------------------------


# # Kaplan-Meier vs. Nelson-Aalen -------------------------------------------
# 
# fitKM <- survfit(Surv(newTOM, status)~ 1, type="kaplan-meier", data=df1)
# 
# fitNA<- survfit(coxph(Surv(df1$newTOM, df1$status)~1), type="aalen")
# 
# plot(fitKM)
# lines(fitNA, col="red")

# Cox proportional hazards ------------------------------------------------

regressors<-c("MS","DOP", "Quantile", "age", 
              "Leather_seats", "Full_service_history",
              "Xenon_lights", "color_cat", "year_bought")

regs<- "MS+ DOP + Quantile + age + 
Leather_seats +Full_service_history +
Xenon_lights + color_cat + year_bought"



# Survival models ---------------------------------------------------------
Smodels<- NULL
for (i in unique(factor(df1$Typ[grep("S", df1$Typ)]))){
  Smodels[[i]]<- coxph(Surv(newTOM, status) ~ MS+ DOP + Quantile + age + 
                         Leather_seats +Full_service_history +
                         Xenon_lights + color_cat + year_bought, data=df1[df1$Typ==i])
}



# Save results ------------------------------------------------------------


sink("C:/Users/Nk/Documents/Uni/MA/MA_Code/Results/tab_cphS.tex")
stargazer(Smodels[["S250"]], Smodels[["S320"]], Smodels[["S350"]], Smodels[["S400"]], 
          Smodels[["S420"]], Smodels[["S450"]], Smodels[["S500"]], Smodels[["S550"]], 
          Smodels[["S600"]],
          apply.coef = exp,
          apply.se = exp, 
          label="tab:cphA",
          title="Cox proportional hazards. Comparing buyer preferences for the Mercedes-Benz A-Class", 
          covariate.labels = c("Market size", "DOP", "Quantile",
                               "Age", "Leather seats", "Full service history", 
                               "Xenon lights", "Color"),
          dep.var.caption  = "Dependent variable",
          dep.var.labels   = "Time on market (in days)",
          column.labels = c("S250", "S320", "S350", "S400", "S420", "S450", "S500", "S550", "S600"), 
          summary=F, 
          align=TRUE)
sink()

sink("C:/Users/Nk/Documents/Uni/MA/MA_Code/Results/tab_cphSL.tex")
stargazer(Smodels[["SL280"]], Smodels[["SL300"]], Smodels[["SL320"]], Smodels[["SL350"]], 
          Smodels[["SL600"]], 
          apply.coef = exp,
          apply.se = exp, 
          label="tab:cphA",
          title="Cox proportional hazards. Comparing buyer preferences for the Mercedes-Benz A-Class", 
          covariate.labels = c("Market size", "DOP", "Quantile",
                               "Age", "Leather seats", "Full service history", 
                               "Xenon lights", "Color"),
          dep.var.caption  = "Dependent variable",
          dep.var.labels   = "Time on market (in days)",
          column.labels = c("SL280", "SL300", "SL320", "SL350", "SL600"), 
          summary=F, 
          align=TRUE)
sink()


sink("C:/Users/Nk/Documents/Uni/MA/MA_Code/Results/tab_cphSLK.tex")
stargazer(Smodels[["SLK200"]], Smodels[["SLK230"]], Smodels[["SLK250"]], 
          Smodels[["SLK280"]], Smodels[["SLK300"]], Smodels[["SLK320"]], Smodels[["SLK350"]], 
          apply.coef = exp,
          apply.se = exp, 
          label="tab:cphA",
          title="Cox proportional hazards. Comparing buyer preferences for the Mercedes-Benz A-Class", 
          covariate.labels = c("Market size", "DOP", "Quantile",
                               "Age", "Leather seats", "Full service history", 
                               "Xenon lights", "Color"),
          dep.var.caption  = "Dependent variable",
          dep.var.labels   = "Time on market (in days)",
          column.labels = c("SLK200", "SLK230", "SLK250", "SLK280", "SLK300", "SLK320", "SLK350"), 
          summary=F, 
          align=TRUE)
sink()


