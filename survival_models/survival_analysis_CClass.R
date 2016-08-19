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
Cmodels<- NULL
for (i in unique(factor(df1$Typ[grep("C", df1$Typ)]))){
  Cmodels[[i]]<- coxph(Surv(newTOM, status) ~ MS+ DOP + Quantile + age + 
                         Leather_seats +Full_service_history +
                         Xenon_lights + color_cat + year_bought, data=df1[df1$Typ==i])
}



# Save results ------------------------------------------------------------


sink("C:/Users/Nk/Documents/Uni/MA/MA_Code/Results/tab_cphC.tex")
stargazer(Cmodels[["C160"]], Cmodels[["C180"]], Cmodels[["C200"]], Cmodels[["C220"]], 
          Cmodels[["C230"]], Cmodels[["C240"]],Cmodels[["C250"]], Cmodels[["C270"]], Cmodels[["C280"]], 
          Cmodels[["C300"]], Cmodels[["C320"]], Cmodels[["C350"]],
          apply.coef = exp,
          apply.se = exp, 
          label="tab:cphA",
          title="Cox proportional hazards. Comparing buyer preferences for the Mercedes-Benz A-Class", 
          covariate.labels = c("Market size", "DOP", "Quantile",
                               "Age", "Leather seats", "Full service history", 
                               "Xenon lights", "Color"),
          dep.var.caption  = "Dependent variable",
          dep.var.labels   = "Time on market (in days)",
          column.labels = c("C160", "C180", "C200", "C220", "C230", "C240", 
                            "C250", "C270", "C280", "C300", "C320", "C350"), 
          summary=F, 
          align=TRUE)
sink()





