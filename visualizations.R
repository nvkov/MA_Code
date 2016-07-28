#Visualizations:
rm(list=ls())

library("stringi")
library("data.table")

load("C:/Users/Nk/Documents/Uni/MA/Pkw/MobileDaten/generatedData/Merged_data/df_merge_after_step31.RData")

#Changes in Farbe:
print(df_merge[car_ID=="29124571", 
               c("car_ID",  "Typ",   "Erstzulassung", "Farbe", "Eigenschaften", "prices_firstDate", "prices_lastDate"), with=F])


#Changes in vendor_ID:
#big vendor pool:
(df_merge[df_merge$car_ID %in% c("172076508", "172076573", "172076666", "172076691", "172076698"),
          c("car_ID",  "Typ", "Erstzulassung", "Farbe", "Eigenschaften", "prices_firstDate", "prices_lastDate"), with=F])


visualization_vars<- c("car_ID",  "Typ", "Erstzulassung", "Farbe", "Eigenschaften", "prices_firstDate", "prices_lastDate")

#Subcase 1: legitimate car ID pools: 

df_merge[df_merge$car_ID=="78641380", visualization_vars, with=F]

#Subcase 2: car ID pools due to typo:

df_merge[df_merge$car_ID=="172104419", visualization_vars, with=F]

#Subcase 3: mix of subcase 1 and 2:
df_merge[df_merge$car_ID=="126511389", visualization_vars, with=F]


#Examples:

#Subcase 1: Vendors uploading information simultaneously from several accounts:

(df_merge[df_merge$vendor_ID %in% c("452484", "452482", "452486") & valuePrice==16800.42 ,])



