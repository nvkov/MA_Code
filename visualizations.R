#Visualizations:
rm(list=ls())

library("stringi")
library("data.table")

load("C:/Users/Nk/Documents/Uni/MA/Pkw/MobileDaten/generatedData/Merged_data/df_merge_after_step31.RData")

#Changes in Farbe:
View(df_merge[car_ID=="29124571", 
               c("car_ID", "vendor_ID", "Typ",   "Erstzulassung", "Farbe", "Eigenschaften", "prices_firstDate", "prices_lastDate"), with=F])


#Changes in vendor_ID:
#big vendor pool:
View(df_merge[df_merge$car_ID %in% c("172076508", "172076573", "172076666", "172076691", "172076698"),
          c("car_ID", "vendor_ID", "Typ", "Erstzulassung", "Farbe", "Eigenschaften", "prices_firstDate", "prices_lastDate"), with=F])


visualization_vars<- c("car_ID", "vendor_ID", "Typ", "Erstzulassung", "Farbe", "Eigenschaften", "prices_firstDate", "prices_lastDate")

#Subcase 1: legitimate car ID pools: 

View(df_merge[df_merge$car_ID=="78641380", visualization_vars, with=F])
 
#Subcase 2: car ID pools due to typo:

View(df_merge[df_merge$car_ID=="172104419", visualization_vars, with=F])

#Subcase 3: mix of subcase 1 and 2:
View(df_merge[df_merge$car_ID=="126511389", visualization_vars, with=F])


#Examples:

#Subcase 1: Vendors uploading information simultaneously from several accounts:

View(df_merge[df_merge$vendor_ID %in% c("452484", "452482", "452486") & valuePrice==16800.42 , visualization_vars, with=F])

#Subcase 2: vendors uploading information simultaneously from several accounts:

View(df_merge[df_merge$car_ID %in% c("132041641", "132194616", "132243640", "132291267", "132342007"), visualization_vars, with=F]) 

# Behavior 3: Vendors changing specs: -------------------------------------------------
visualization_vars1<- c(visualization_vars, "valuePrice") 
View(df_merge[df_merge$car_ID=="221099", visualization_vars1,with=F ])