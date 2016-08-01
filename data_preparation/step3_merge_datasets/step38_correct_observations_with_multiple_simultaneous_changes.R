rm(list=ls())

library("stringi")
library("data.table")

load("C:/Users/Nk/Documents/Uni/MA/Pkw/MobileDaten/generatedData/Merged_data/vendor_behavior_after_data_clean.RData")


vendors<- vendors[n_unique_changes_price==1 & n_changes_total>1,
                    `:=`(firstDate=min(cars_lastChange), 
                         lastDate=max(cars_lastDate),
                         KM_lag=data.table::shift(Kilometer, 1, NA, "lag")),
                    by=.(valuePrice, car_ID, vendor_ID, Erstzulassung, 
                         Typ, prices_firstDate, prices_lastDate, Farbe)]

vendors<- vendors[car_ID_pool>1,
                  `:=`(firstDate=min(cars_lastChange), 
                       lastDate=max(cars_lastDate),
                       KM_lag=data.table::shift(Kilometer, 1, NA, "lag")),
                  by=.(valuePrice, car_ID, vendor_ID, Typ)]

vendors$KM_lag[is.na(vendors$KM_lag)]<-vendors$Kilometer[is.na(vendors$KM_lag)] 
vendors$KM_monotonicity= vendors$Kilometer-vendors$KM_lag


vendors<- vendors[n_unique_changes_price==1 & n_changes_total>1,
                  KM_mon_check:=min(KM_monotonicity),
                  
                  by=.(valuePrice, car_ID, vendor_ID, Erstzulassung, 
                       Typ, prices_firstDate, prices_lastDate, Farbe)]

vendors<- vendors[car_ID_pool>1,
                  KM_mon_check:=min(KM_monotonicity),
                  
                  by=.(valuePrice, car_ID, vendor_ID, Typ)]


# Key numbers -------------------------------------------------------------

nrow(vendors[vendors$n_unique_changes_price==1 & vendors$n_changes_total>1,])
nrow(vendors[vendors$KM_mon_check<0,])
nrow(vendors[vendors$car_ID_pool>1,])


# Keep only latest observations for doublets ------------------------------

vendors[!is.na(vendors$firstDate) & vendors$lastDate!=vendors$cars_lastDate,]<- NA 
vendors<- vendors[!is.na(vendors$vendor_ID),]
vendors$cars_lastChange[!is.na(vendors$firstDate)]<- vendors$firstDate[!is.na(vendors$firstDate)]


# Keep only realistic observations for car ID pool ------------------------
vendors[car_ID_pool>1 & cars_lastDate> prices_firstDate]<- NA
vendors<- vendors[!is.na(vendors$vendor_ID),]


# Look at car ID pools ----------------------------------------------------

# Select only vendors with repeating prices:
vendors_carID_pool<- vendors[vendors$car_ID_pool>1,]
vendors_carID_pool<- vendors_carID_pool[,duplics:=.N,
                                        by=.(valuePrice, car_ID)]

vendors_carID_pool<- vendors_carID_pool[,diff_types:=length(unique(Typ)),
                                        by=.(valuePrice, car_ID, Typ)]

vendors_carID_pool<- vendors_carID_pool[,diff_Erstzulassung:=length(unique(Erstzulassung)),
                                        by=.(valuePrice, car_ID, Typ)]

vendors_carID_pool<- vendors_carID_pool[,rows:=.N,
                                        by=.(valuePrice, car_ID, Typ)] 


vendors_carID_pool<- vendors_carID_pool[vendors_carID_pool$duplics>1,]


vendors_carID_pool<- vendors_carID_pool[ ,`:=`(firstDate=min(cars_lastChange), 
                                            lastDate=max(cars_lastDate),
                                            KM_lag=data.table::shift(Kilometer, 1, NA, "lag")),
                                          by=.(valuePrice, car_ID, Typ)]


vendors_carID_pool<- vendors_carID_pool[prices_firstDate>=cars_lastDate,]

# Save dataset ------------------------------------------------------------


#save(vendors_carID_pool, file="C:/Users/Nk/Documents/Uni/MA/Pkw/MobileDaten/generatedData/Merged_data/carIDpool.RData")
save(vendors, file="C:/Users/Nk/Documents/Uni/MA/Pkw/MobileDaten/generatedData/Merged_data/df_merge_after_step38.RData")

