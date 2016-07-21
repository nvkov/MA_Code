#Explore vendor behavior:

rm(list=ls())

#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<- "/Pkw/MobileDaten/generatedData/"
wd<- paste0(project_directory, data_directory)

setwd(wd)

library("data.table")
library("stringr")

#Read files with car specifications:
load("C:/Users/Nk/Documents/Uni/MA/Pkw/MobileDaten/generatedData/Vendors/step39_vendor_behavior.Rdata")

###########################################################################
#See if vendors have car_ID pools: 

vendors_small<- vendors[, .(car_ID_pool=var(Erstzulassung), 
                      paste(unique(Typ), collapse = ", ")) , by=.(car_ID, vendor_ID)]


# Create and save vendor indicator table -----------------------------------

vendor_ind<- vendors[,.(avg_specs_change=mean(lag_specs_change, 'na.rm'=T), 
                        avg_price_change=mean(lag_price_change, na.rm=T), 
                        avg_carID_change=mean(lag_carID_change, na.rm=T), 
                        #vendor_pool= paste(unique(vendorID_pool), collaspe=", "), 
                        avg_carID_change_count=(mean(n_changes_carID, na.rm=T)),
                        avg_change_vendor_pool= mean(n_changes_vendor_pool, 'na.rm'=T),
                        avg_unique_vendors= mean(n_unique_changes_vendor_pool, na.rm=T),
                        avg_similar_cars_diffIDs = mean(similar_cars_diff_IDs, nr.rm=T), 
                        avg_var_km= mean(variance_km, na.rm=T), 
                        avg_var_price=mean(variance_price, na.rm=T), 
                        leasing=  ifelse(max(leasing)>0, 1, 0), 
                        avg_TOM_specs= mean(avg_TOM_specs),
                        avg_TOM_price= mean(avg_TOM_price),
                        diff_cat<- length(unique(Kategorie)),
                        var_cat<- paste(unique(Kategorie), collapes=", "), 
                        avg_n_changes_specs= mean(n_chnages_specs, na.rm=T),
                        avg_n_changes_price= mean(n_changes_price, na.rm=T)
                        ),
by=.(vendor_ID)]


# Look at one case --------------------------------------------------------
pool<- vendor_ind$vendor_pool[vendor_ind$vendor_ID=="468998"]
pool<- pool[[2]]
pool<- gsub(" ", "", pool)
pool<- unlist(strsplit(pool, ","))
pool<- as.numeric(as.character(pool))

View(vendors[vendors$vendor_ID %in% pool,])


# Look at relevant columns -------------------------------------------------

relCols<- c("vendor_ID", "car_ID", "lag_specs_change", "lag_price_change", 
            "lag_carID_change", "vendorID_pool",  "n_chnages_specs", 
            "n_changes_price", "n_changes_carID", "n_changes_vendor_pool", 
            "variance_vendor_pool", "valuePrice", "similar_cars_diff_IDs", 
            "variance_km", "variance_price", "pool_firstDate", "leasing", 
            "leasing_carIDs", "variance_category")

head(vendors[,relCols, with=F])
View(vendors[vendors$leasing>1,])
vendors_exclude<- c("vendor_ID", "car_ID")


# Explore clusters of vendor behavior -------------------------------------

library(nFactors)
ev <- eigen(cor(vendors_cluster[,!vendors_exclude, with=F])) # get eigenvalues
ap <- parallel(subject=nrow(vendors_cluster[,!vendors_exclude, with=F]),var=ncol(vendors_cluster[,!c("vendor_ID", "car_ID"), with=F]),
               rep=5,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)