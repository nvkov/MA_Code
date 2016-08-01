rm(list=ls())

library("stringi")
library("data.table")
library("IRanges")

#NB!!!!! Drop unrealistically high prices!

load("C:/Users/Nk/Documents/Uni/MA/Pkw/MobileDaten/generatedData/Merged_data/df_merge_after_step39.RData")


# Keep only relevant vars -------------------------------------------------

relVars<- c("valuePrice", "Typ", "Kategorie", "Farbe", "HU", "Erstzulassung", 
              "Emission", "Kraftstoff", "Leistung", "Schaltung", "Klimatisierung", 
              "Hubraum", "Eigenschaften", "Kilometer", "car_ID", "vendor_ID", 
              "prices_firstDate", "prices_lastDate", "cars_lastChange", "cars_lastDate", 
              "Anzeigenanlage", "TOM")

vendors<- vendors[ ,relVars,with=F]


# Find last observation for each car ID -----------------------------------

vendors<- vendors[, `:=`(maxDate_cars=max(cars_lastDate), 
                        maxDate_prices=max(prices_lastDate)),
                        
                        by=.(car_ID)]

vendors$right_censor<- ifelse(vendors$cars_lastDate==vendors$maxDate_cars & 
                                vendors$prices_lastDate==vendors$maxDate_prices,
                              0, 1)


# Explore for changes in car_ID but same prices---------------------------------

#mini<- vendors[vendors$right_censor==0,]

vendors<-vendors[vendors$right_censor==0 ,`:=`(car_group=length(unique(car_ID)), 
              IDs=paste(unique(car_ID), collapse = ", "), 
              maxDate_cars=max(cars_lastDate)
              #diff_price=length(unique(valuePrice))
              ),  
       by=.(Typ, Erstzulassung, vendor_ID, Farbe, valuePrice)]


#vendor ID: 1601144

vendors[vendors$right_censor==0 & vendors$maxDate_cars!=vendors$cars_lastDate]<- NA


vendors<- vendors[!is.na(vendors$valuePrice),]


# Inspect for changes in both prices and car ID ---------------------------


vendors<-vendors[vendors$right_censor==0 ,`:=`(car_group=length(unique(car_ID)), 
                                               IDs=paste(unique(car_ID), collapse = ", "), 
                                               maxDate_cars=max(cars_lastDate),
                                               diff_price=length(unique(valuePrice)), 
                                               lag_price=data.table::shift(valuePrice, 1, NA, "lag"),
                                               lag_km=data.table::shift(Kilometer, 1, NA, "lag")
),  
by=.(Typ, Erstzulassung, vendor_ID, Farbe, Kategorie)]

vendors$mon_price<- vendors$valuePrice - vendors$lag_price
vendors$mon_km<- vendors$Kilometer - vendors$lag_km

vendors<-vendors[car_group>1,`:=`(price_check=max(mon_price, na.rm = T),
                       km_check=min(mon_km, na.rm=T), 
                       maxDate_prices=max(prices_lastDate)),
                 by=.(IDs)]

vendors$right_censor[vendors$car_group>1 & 
                       vendors$price_check<=0 & 
                       vendors$km_check>=0 & 
                       vendors$maxDate_prices!=vendors$prices_lastDate]<-1

#Look at vendor 464738 for consecutive entries with different cars.


# Decide what do do with the rest -----------------------------------------
vendors<-vendors[vendors$right_censor==0 ,`:=`(car_group1=length(unique(car_ID)), 
                                               IDs1=paste(unique(car_ID), collapse = ", "), 
                                               maxDate_cars=max(cars_lastDate),
                                               diff_price=length(unique(valuePrice)), 
                                               lag_price=data.table::shift(valuePrice, 1, NA, "lag"),
                                               lag_km=data.table::shift(Kilometer, 1, NA, "lag")
                                                ),  
                by=.(Typ, Erstzulassung, vendor_ID, Farbe, Kategorie, Kilometer, Schaltung, Hubraum, Eigenschaften )]

vendors$mon_price<- vendors$valuePrice - vendors$lag_price
vendors$mon_km<- vendors$Kilometer - vendors$lag_km

vendors<-vendors[car_group1>1,`:=`(price_check1=max(mon_price, na.rm = T),
                                  #km_check=min(mon_km, na.rm=T), 
                                  maxDate_prices=max(prices_lastDate)),
                 by=.(IDs1)]


View(vendors[vendors$right_censor==0 & vendors$car_group1>1,])

vendors$right_censor[vendors$car_group1>1 & 
                       vendors$price_check1<=0 & 
                       vendors$maxDate_prices!=vendors$prices_lastDate]<-1

View(vendors[vendors$right_censor==0 & vendors$car_group>1,])

#See vendors 464738 and 466033


# Under construction:
# Count sequences of  monotonically decreasing prices ---------------------
vendors$mon_price[is.na(vendors$mon_price)]<- 0
vendors$decrease_idx<- ifelse(vendors$mon_price<=0, 0, 1)
vendors<- vendors[vendors$right_censor==0 & vendors$car_group1>1,
                  end:= data.table::shift(decrease_idx, 1, 2 ,"lead"), 
                  by=.(IDs1)]

vendors$right_censor[vendors$car_group1>1 & 
                       vendors$maxDate_prices!=vendors$prices_lastDate]<-1

vendors$right_censor[vendors$end==1]<-0


# Remove redundant vars ---------------------------------------------------

irrelVars<- c("car_group", "car_group1", "IDs", "IDs1", "lag_km", "lag_price", "price_check", "km_check", 
              "maxDate_cars", "maxDate_prices")

vendors<- vendors[,-irrelVars,with=F]

# # Relax the matching assumptions and try to form groups again -------------
# mini<-vendors[vendors$right_censor==0 ,`:=`(car_group1=length(unique(car_ID)), 
#                                                IDs1=paste(unique(car_ID), collapse = ", "), 
#                                                maxDate_cars=max(cars_lastDate),
#                                                diff_price=length(unique(valuePrice)), 
#                                                lag_price=data.table::shift(valuePrice, 1, NA, "lag"),
#                                                lag_km=data.table::shift(Kilometer, 1, NA, "lag")
# ),  
# by=.(Typ, Erstzulassung, vendor_ID, Farbe, Kategorie, Schaltung, Hubraum, Eigenschaften )]
# 
# mini<-vendors[vendors$right_censor==0 ,`:=`(overlaps= countOverlaps(IRanges(as.numeric(prices_firstDate), as.numeric(prices_lastDate)), 
#                                                                      IRanges(as.numeric(prices_firstDate), as.numeric(prices_lastDate)))
#                                             ),  
#               by=.(Typ, Erstzulassung, vendor_ID, Farbe, Kategorie, Schaltung, Hubraum, Eigenschaften )]

 

# View(vendors[vendors$right_censor==0 & vendors$car_group>1,])


# And again ---------------------------------------------------------------
# 
# mini1<-vendors[vendors$right_censor==0 ,`:=`(car_group1=length(unique(car_ID)), 
#                                             IDs1=paste(unique(car_ID), collapse = ", "), 
#                                             maxDate_cars=max(cars_lastDate),
#                                             diff_price=length(unique(valuePrice)), 
#                                             lag_price=data.table::shift(valuePrice, 1, NA, "lag"),
#                                             lag_km=data.table::shift(Kilometer, 1, NA, "lag")
# ),  
# by=.(Typ, Erstzulassung, vendor_ID, Farbe, Kategorie)]



#See vendors 1601114
#See vendors: 457567, 466033, 450932, 453049, 458427, 1601152
# Save data ---------------------------------------------------------------
save(vendors, file="C:/Users/Nk/Documents/Uni/MA/Pkw/MobileDaten/generatedData/Merged_data/df_merge_after_step41.RData")

