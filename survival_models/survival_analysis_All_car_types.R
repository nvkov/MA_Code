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
            "Leistung", "Typ","status", "vendor_ID", "car_ID", "prices_firstDate", "prices_lastDate", "Kategorie") 
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

# Cox proportional hazards ------------------------------------------------

regressors<-c("MS","DOP", "Quantile", "age", 
              "Leather_seats", "Full_service_history",
              "Xenon_lights", "color_cat", "year_bought")

regs<- "MS+ DOP + Quantile + age + 
Leather_seats +Full_service_history +
Xenon_lights + color_cat + year_bought"



# Survival models ---------------------------------------------------------
Models<- NULL
for (i in unique(factor(df1$Typ))){
  Models[[i]]<- coxph(Surv(newTOM, status) ~ MS+ DOP + Quantile + age + 
                         Leather_seats +Full_service_history +
                         Xenon_lights + color_cat, data=df1[df1$Typ==i])
}



# Save results ------------------------------------------------------------


sink("C:/Users/Nk/Documents/Uni/MA/MA_Code/Results/tab_cphS.tex")
stargazer(Models[["S250"]], Models[["S320"]], Models[["S350"]], Models[["S400"]], 
          Models[["S420"]], Models[["S450"]], Models[["S500"]], Models[["S550"]], 
          Models[["S600"]],
          apply.coef = exp,
          apply.se = exp, 
          label="tab:cphS",
          title="Cox proportional hazards. Comparing buyer preferences for the Mercedes-Benz S-Class", 
          covariate.labels = c("Market size", "DOP", "Quantile",
                               "Age", "Leather seats", "Full service history", 
                               "Xenon lights", "Color"),
          dep.var.caption  = "Dependent variable",
          dep.var.labels   = "Time on market (in days)",
          column.labels = c("S250", "S320", "S350", "S400", "S420", "S450", "S500", "S550", "S600"), 
          no.space = T,
          summary=F, 
          align=TRUE, 
          no.space = T, 
          column.sep.width = "2pt", 
          float.env="sidewaystable", 
          font.size = "tiny", 
          t.auto=F, 
          p.auto=F, 
          report = "vc*")
sink()

sink("C:/Users/Nk/Documents/Uni/MA/MA_Code/Results/tab_cphSL.tex")
stargazer(Models[["SL280"]], Models[["SL300"]], Models[["SL320"]], Models[["SL350"]], 
          Models[["SL600"]], 
          apply.coef = exp,
          apply.se = exp, 
          label="tab:cphSL",
          title="Cox proportional hazards. Comparing buyer preferences for the Mercedes-Benz SL-Class", 
          covariate.labels = c("Market size", "DOP", "Quantile",
                               "Age", "Leather seats", "Full service history", 
                               "Xenon lights", "Color"),
          dep.var.caption  = "Dependent variable",
          dep.var.labels   = "Time on market (in days)",
          column.labels = c("SL280", "SL300", "SL320", "SL350", "SL600"), 
          no.space = T,
          summary=F, 
          align=TRUE, 
          column.sep.width = "2pt", 
          float.env="sidewaystable", 
          font.size = "tiny", 
          t.auto=F, 
          p.auto=F, 
          report = "vc*")
sink()


sink("C:/Users/Nk/Documents/Uni/MA/MA_Code/Results/tab_cphSLK.tex")
stargazer(Models[["SLK200"]], Models[["SLK230"]], Models[["SLK250"]], 
          Models[["SLK280"]], Models[["SLK300"]], Models[["SLK320"]], Models[["SLK350"]], 
          apply.coef = exp,
          apply.se = exp, 
          label="tab:cphSLK",
          title="Cox proportional hazards. Comparing buyer preferences for the Mercedes-Benz SLK-Class", 
          covariate.labels = c("Market size", "DOP", "Quantile",
                               "Age", "Leather seats", "Full service history", 
                               "Xenon lights", "Color"),
          dep.var.caption  = "Dependent variable",
          dep.var.labels   = "Time on market (in days)",
          column.labels = c("SLK200", "SLK230", "SLK250", "SLK280", "SLK300", "SLK320", "SLK350"), 
          no.space = T,
          summary=F, 
          align=TRUE, 
          column.sep.width = "2pt", 
          float.env="sidewaystable", 
          font.size = "tiny", 
          t.auto=F, 
          p.auto=F, 
          report = "vc*")
sink()


# BClass ------------------------------------------------------------------

sink("C:/Users/Nk/Documents/Uni/MA/MA_Code/Results/tab_cphB.tex")
stargazer(Models[["B150"]], Models[["B160"]], Models[["B170"]], Models[["B180"]], 
          Models[["B200"]], 
          apply.coef = exp,
          apply.se = exp, 
          label="tab:cphB",
          title="Cox proportional hazards. Comparing buyer preferences for the Mercedes-Benz B-Class", 
          covariate.labels = c("Market size", "DOP", "Quantile",
                               "Age", "Leather seats", "Full service history", 
                               "Xenon lights", "Color"),
          dep.var.caption  = "Dependent variable",
          dep.var.labels   = "Time on market (in days)",
          column.labels = c("B150", "B160", "B170", "B180", "B200"), 
          no.space = T,
          summary=F, 
          align=TRUE, 
          column.sep.width = "2pt", 
          float.env="sidewaystable", 
          font.size = "tiny", 
          t.auto=F, 
          p.auto=F, 
          report = "vc*")
sink()



# CClass ------------------------------------------------------------------

sink("C:/Users/Nk/Documents/Uni/MA/MA_Code/Results/tab_cphC.tex")
stargazer(Models[["C160"]], Models[["C180"]], Models[["C200"]], Models[["C220"]], 
          Models[["C230"]], Models[["C240"]],Models[["C250"]], Models[["C270"]], Models[["C280"]], 
          Models[["C300"]], Models[["C320"]], Models[["C350"]],
          apply.coef = exp,
          apply.se = exp, 
          label="tab:cphC",
          title="Cox proportional hazards. Comparing buyer preferences for the Mercedes-Benz C-Class", 
          covariate.labels = c("Market size", "DOP", "Quantile",
                               "Age", "Leather seats", "Full service history", 
                               "Xenon lights", "Color"),
          dep.var.caption  = "Dependent variable",
          dep.var.labels   = "Time on market (in days)",
          column.labels = c("C160", "C180", "C200", "C220", "C230", "C240", 
                            "C250", "C270", "C280", "C300", "C320", "C350"), 
          no.space = T,
          summary=F, 
          align=TRUE, 
          column.sep.width = "2pt", 
          float.env="sidewaystable", 
          font.size = "tiny", 
          t.auto=F, 
          p.auto=F, 
          report = "vc*")
sink()


# AClass ------------------------------------------------------------------

sink("C:/Users/Nk/Documents/Uni/MA/MA_Code/Results/tab_cphA.tex")
stargazer(Models[["A140"]], Models[["A150"]], Models[["A160"]], Models[["A170"]], 
          Models[["A180"]], Models[["A190"]],Models[["A200"]], Models[["A210"]], 
          apply.coef = exp,
          apply.se = exp, 
          label="tab:cphA",
          title="Cox proportional hazards. Comparing buyer preferences for the Mercedes-Benz A-Class", 
          covariate.labels = c("Market size", "DOP", "Quantile",
                               "Age", "Leather seats", "Full service history", 
                               "Xenon lights", "Color"),
          dep.var.caption  = "Dependent variable",
          dep.var.labels   = "Time on market (in days)",
          column.labels = c("A140", "A150", "A160", "A170","A180", "A190", "A200", "A210"),  
          no.space = T,
          summary=F, 
          align=TRUE, 
          column.sep.width = "2pt", 
          float.env="sidewaystable", 
          font.size = "tiny", 
          t.auto=F, 
          p.auto=F, 
          report = "vc*")
sink()


# EClass ------------------------------------------------------------------
sink("C:/Users/Nk/Documents/Uni/MA/MA_Code/Results/tab_cphE.tex")
stargazer(Models[["E200"]], Models[["E220"]], Models[["E230"]], Models[["E240"]], 
          Models[["E250"]], Models[["E270"]],Models[["E280"]], Models[["E300"]],
          Models[["E320"]], Models[["E350"]], Models[["E400"]], Models[["E420"]], 
          Models[["E430"]], Models[["E500"]],
          apply.coef = exp,
          apply.se = exp, 
          label="tab:cphE",
          title="Cox proportional hazards. Comparing buyer preferences for the Mercedes-Benz E-Class", 
          covariate.labels = c("Market size", "DOP", "Quantile",
                               "Age", "Leather seats", "Full service history", 
                               "Xenon lights", "Color"),
          dep.var.caption  = "Dependent variable",
          dep.var.labels   = "Time on market (in days)",
          column.labels = c("E200", "E220", "E230", "E240", "E250", 
                            "E320", "E350", "E400", "E420", "E430", "E450"),  
          no.space = T,
          summary=F, 
          align=TRUE, 
          column.sep.width = "2pt", 
          float.env="sidewaystable", 
          font.size = "tiny", 
          t.auto=F, 
          p.auto=F, 
          report = "vc*")
sink()


# MClass ------------------------------------------------------------------

sink("C:/Users/Nk/Documents/Uni/MA/MA_Code/Results/tab_cphM.tex")
stargazer(Models[["ML230"]], Models[["ML250"]], Models[["ML270"]], Models[["ML280"]], 
          Models[["ML300"]], Models[["ML320"]], Models[["ML350"]], Models[["ML400"]],
          Models[["ML420"]], Models[["ML450"]], Models[["ML500"]], 
          apply.coef = exp,
          apply.se  = exp, 
          label="tab:cphM",
          title="Cox proportional hazards. Comparing buyer preferences for the Mercedes-Benz M-Class", 
          covariate.labels = c("Market size", "DOP", "Quantile",
                               "Age", "Leather seats", "Full service history", 
                               "Xenon lights", "Color"),
          dep.var.caption  = "Dependent variable",
          dep.var.labels   = "Time on market (in days)",
          column.labels = c("ML230", "ML250", "ML270", "ML280", "ML300", "ML320", 
                            "ML350", "ML400", "ML420", "ML450", "ML500"),
          no.space = T,
          summary=F, 
          align=TRUE, 
          column.sep.width = "2pt", 
          float.env="sidewaystable", 
          font.size = "tiny", 
          t.auto=F, 
          p.auto=F, 
          report = "vc*") 
sink()



# Visualizations ----------------------------------------------------------


# Make heatmap for the coeffs of all models -------------------------------
#Extract model coefficients:
coefs<-lapply(Models,function(x)coef(x))
heat_mat<-t(as.data.frame((coefs)))

nba_heatmap <- heatmap(heat_mat)
nba_heatmap <- heatmap(heat_mat, Colv = NA, col = cm.colors(256), scale="column", margins=c(5,10))
