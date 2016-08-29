rm(list=ls())

library("stringi")
library("data.table")
library("IRanges")

#NB!!!!! Drop unrealistically high prices!

load("C:/Users/Nk/Documents/Uni/MA/Pkw/MobileDaten/generatedData/Merged_data/leasing_after_step35.RData")
load("C:/Users/Nk/Documents/Uni/MA/Pkw/MobileDaten/generatedData/Merged_data/overlaps_after_step35.RData")


# Recognize censored data -------------------------------------------------
relVars<- c("valuePrice", "Typ", "Kategorie", "Farbe", "HU", "Erstzulassung", 
            "Emission", "Kraftstoff", "Leistung", "Schaltung", "Klimatisierung", 
            "Hubraum", "Eigenschaften", "Kilometer", "car_ID", "vendor_ID", 
            "prices_firstDate", "prices_lastDate", "cars_lastChange", "cars_lastDate", 
            "Anzeigenanlage", "TOM")

leasing<- leasing[ ,relVars,with=F]


# Find last observation for each car ID -----------------------------------

leasing<- leasing[, `:=`(maxDate_cars=max(cars_lastDate), 
                         maxDate_prices=max(prices_lastDate)),
                  
                  by=.(car_ID)]

leasing$right_censor<- ifelse(leasing$cars_lastDate==leasing$maxDate_cars & 
                                leasing$prices_lastDate==leasing$maxDate_prices,
                              0, 1)

leasing$right_censor[leasing$prices_lastDate=="2012-12-18",]<- 1

#  ------------------------------------------------------------------------

overlaps<- overlaps[ ,relVars,with=F]


# Find car ID pools -------------------------------------------------------

# Find last observation for each car ID -----------------------------------

overlaps<- overlaps[, `:=`(maxDate_cars=max(cars_lastDate), 
                         maxDate_prices=max(prices_lastDate)),
                  
                  by=.(car_ID)]

overlaps$right_censor<- ifelse(overlaps$cars_lastDate==overlaps$maxDate_cars & 
                                overlaps$prices_lastDate==overlaps$maxDate_prices,
                              0, 1)


overlaps$right_censor[overlaps$prices_lastDate=="2012-12-18"]<- 1

save(leasing, file="C:/Users/Nk/Documents/Uni/MA/Pkw/MobileDaten/generatedData/Merged_data/leasing_after_step42.RData")
save(overlaps, file="C:/Users/Nk/Documents/Uni/MA/Pkw/MobileDaten/generatedData/Merged_data/overlaps_after_step42.RData")
