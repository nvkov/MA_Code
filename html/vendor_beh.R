library("stringi")
library("data.table")

load("C:/Users/Nk/Documents/Uni/MA/Pkw/MobileDaten/generatedData/Merged_data/df_merge_after_step32.RData")

#load("Merged_data/df_merge_after_step32.RData")

# Visualize vendor behaviors ----------------------------------------------


# Vendors using car ID pools ----------------------------------------------
visualization_vars<- c("car_ID", "vendor_ID", "valuePrice", "Typ", "Erstzulassung", 
                       "cars_lastDate", "cars_lastChange", "Kilometer", "Eigenschaften")


#Identificantion of such vendors:

vendors<- df_merge[ ,`:=`(car_ID_pool= var(Erstzulassung), 
                          types=paste(Typ, collapse=", ")), 
                          by=.(car_ID, vendor_ID)]


#Examples:
#Subcase 1: legitimate car ID pools: 

vendors[vendors$car_ID=="78641380", visualization_vars, with=F]

#Subcase 2: car ID pools due to typo:

vendors[vendors$car_ID=="172104419", visualization_vars, with=F]

#Subcase 3: mix of subcase 1 and 2:
vendors[vendors$car_ID=="126511389", visualization_vars, with=F]


# Vendors using vendor_ID pools -------------------------------------------

#Identificantion of such vendors:
vendors<- vendors[,`:=`(vendorID_pool=paste(unique(vendor_ID), collapse = ", "),
                        
                        #distinguishes one vendor from vendor pools:
                        variance_vendor_pool=var(vendor_ID),
                        
                        #addtionoal info on change patterns:
                        n_changes_vendor_pool=.N, 
                        n_unique_changes_vendor_pool=length(unique(vendor_ID)), 
                        
                        #distinguishes simmultaneous from sequential uploads:
                        var_Anzeigenanlage= var(Anzeigenanlage)),
                  
                  by=.(Typ, Erstzulassung, valuePrice, Schaltung, 
                       Leistung, Eigenschaften)]

#Examples:

#Subcase 1: Vendors uploading information simultaneously from several accounts:

(vendors[vendors$vendor_ID %in% c("452484", "452482", "452486") & valuePrice==16800.42 ,])

#Subcase 2: Vendors uploading information sequentially from several accounts:

View(vendors[vendors$n_unique_changes_vendor_pool==343,])


# Vendors changing prices -------------------------------------------------
vendors<- vendors[,`:=`(n_changes_price=.N,
                        n_unique_changes_price=length(unique(valuePrice)), 
                        avg_TOM_price=mean(TOM)),
                  by=.(car_ID, vendor_ID, Typ, Erstzulassung)]

#Examples:

