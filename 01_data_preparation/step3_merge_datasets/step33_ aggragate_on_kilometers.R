#Part 1: General steps, ideas, progress summary and questions
rm(list=ls())

#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<- "/Pkw/MobileDaten/generatedData/"
wd<- paste0(project_directory, data_directory)

setwd(wd)

library("data.table")
library("stringr")

#Read files with car specifications:
load("Merged_data/df_merge_after_step32.RData")

# #Inspect Kilometers:
# df_merge<- df_merge[,.(Kilometer=max(Kilometer), km_change=max(Kilometer)-min(Kilometer),
#                        km_check= paste(Kilometer, collapse = ","), varKm= var(Kilometer),
#                        cars_lastChange=min(cars_lastChange), cars_lastDate=max(cars_lastDate)),
#                     by=.(car_ID, valuePrice, TOM, prices_firstDate, prices_lastDate, 
#                          Anzeigenanlage, Typ, Kategorie,Farbe, HU, 
#                          Erstzulassung, Emission, Kraftstoff, 
#                          Leistung, Schaltung, Klimatisierung, Hubraum, vendor_ID, Eigenschaften)]

#Aggregate Kilometers:
df_merge<- df_merge[,`:=`(cars_lastChange=min(cars_lastChange), 
                       maxDate_cars=max(cars_lastDate),
                       rows=length(unique(Kilometer))), 
                       #medianKm=median(Kilometer)),
                    
                    by=.(car_ID, valuePrice, TOM, prices_firstDate, prices_lastDate,
                         Anzeigenanlage, Typ, Kategorie,Farbe, HU,
                         Erstzulassung, Emission, Kraftstoff,
                         Leistung, Schaltung, Klimatisierung, Hubraum, vendor_ID, Eigenschaften)] 

View(df_merge[df_merge$rows>1,])
df_merge<- df_merge[df_merge$cars_lastDate==maxDate_cars,]



# #Correct unrealistic kilometers:
# #Inspect wrongly specified kilometers:
# View(df_merge[df_merge$km_change>100000,])
# df_merge$km_change[is.na(df_merge$km_change)]<- 0
# 
# df_merge$Kilometer[df_merge$km_change>100000]<- df_merge$Kilometer[df_merge$km_change>100000]-df_merge$km_change[df_merge$km_change>100000]
# 
# #Return value for overcorrected kilometers:
# View(df_merge[df_merge$km_change>100000 &df_merge$Kilometer<10000,])
# df_merge$Kilometer[df_merge$km_change>100000 &df_merge$Kilometer<10000]<- df_merge$Kilometer[df_merge$km_change>100000 &df_merge$Kilometer<10000] + df_merge$km_change[df_merge$km_change>100000 &df_merge$Kilometer<10000]
# 
#Check if kilometers are consistent:

#price_mod<- lm(df_merge$valuePrice~df_merge$Kilometer)
#residuals<- resid(price_mod)
#plot(df_merge$Kilometer[!is.na(df_merge$Kilometer)], residuals)


# Check for unrealistic values --------------------------------------------

summary(df_merge$Kilometer)

#Look at low values 
View(df_merge[df_merge$Kilometer<1500,])

df_merge<- df_merge[df_merge$Kilometer>1000,]

#Look at high values:
View(df_merge[df_merge$Kilometer>150000,])

#Idea! Detect outliers by regressing  price on Kilometer count. Q-Q Plots or something similar. 
#For those outliers find suitable prices



# Aggregate of color: -----------------------------------------------------
# df_merge<- df_merge[,.(Farbe=max(Farbe), farbe_check= paste(Farbe, collapse = ","), varFar= var(Farbe),
#                        cars_lastChange=min(cars_lastChange), cars_lastDate=max(cars_lastDate)),
#                     by=.(car_ID, valuePrice, TOM, prices_firstDate, prices_lastDate, 
#                          Anzeigenanlage, Typ, Kategorie,Kilometer, HU, 
#                          Erstzulassung, Emission, Kraftstoff, 
#                          Leistung, Schaltung, Klimatisierung, Hubraum, vendor_ID, Eigenschaften)]

#Often the colors are changed several times 
#(e.g. beige metalic to gold metalic, gray metalic to silver metalic)
#View(df_merge[df_merge$varFar>0,])
# CarID: 29124571
print(df_merge[car_ID=="29124571", c("car_ID", "Farbe", "Typ", "Erstzulassung"), with=F])


df_merge<- df_merge[,.(Farbe=max(Farbe), 
                       cars_lastChange=min(cars_lastChange), 
                       cars_lastDate=max(cars_lastDate)),
                    
                    by=.(car_ID, valuePrice, TOM, prices_firstDate, prices_lastDate, 
                         Anzeigenanlage, Typ, Kategorie,Kilometer, HU, 
                         Erstzulassung, Emission, Kraftstoff, 
                         Leistung, Schaltung, Klimatisierung, Hubraum, vendor_ID, Eigenschaften)]



# Aggregate on class ------------------------------------------------------

# 
# df_merge<- df_merge[,.(Kategorie=max(Kategorie), Kategorie_check= paste(Kategorie, collapse = ","), varKat= var(Kategorie),
#                        cars_lastChange=min(cars_lastChange), cars_lastDate=max(cars_lastDate)),
#                     by=.(car_ID, valuePrice, TOM, prices_firstDate, prices_lastDate, 
#                          Anzeigenanlage, Typ, Farbe,Kilometer, HU, 
#                          Erstzulassung, Emission, Kraftstoff, 
#                          Leistung, Schaltung, Klimatisierung, Hubraum, vendor_ID, Eigenschaften)]


df_merge<- df_merge[,.(Kategorie=max(Kategorie), 
                       cars_lastChange=min(cars_lastChange), 
                       cars_lastDate=max(cars_lastDate)),
                    
                    by=.(car_ID, valuePrice, TOM, prices_firstDate, prices_lastDate,
                         Anzeigenanlage, Typ, Farbe,Kilometer, HU,
                         Erstzulassung, Emission, Kraftstoff,
                         Leistung, Schaltung, Klimatisierung, Hubraum, vendor_ID, Eigenschaften)]

#-------------------------------------------
save(df_merge, file=paste0(project_directory, data_directory, "Merged_data/df_merge_after_step33.RData" ))

