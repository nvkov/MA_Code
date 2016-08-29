rm(list=ls())

library("stringi")
library("data.table")

load("C:/Users/Nk/Documents/Uni/MA/Pkw/MobileDaten/generatedData/Merged_data/df_merge_after_step37.RData")

#load("Merged_data/df_merge_after_step32.RData")

# Visualize vendor behaviors ----------------------------------------------

# Irrelevant variables ----------------------------------------------------


# Behavior 1: Vendors using car ID pools ----------------------------------------------
visualization_vars<- c("car_ID", "vendor_ID", "valuePrice", "Typ", "Erstzulassung", 
                       "cars_lastDate", "cars_lastChange", "Kilometer", "Eigenschaften")



#Identificantion of such vendors:

vendors<- df_merge[ ,`:=`(car_ID_pool= var(Erstzulassung), 
                          types=paste(Typ, collapse=", "), 
                          n_carID_pool=length(unique(Typ))), 
                          by=.(car_ID, vendor_ID)]

#Examples:
#Subcase 1: legitimate car ID pools: 

vendors[vendors$car_ID=="78641380", visualization_vars, with=F]

#Subcase 2: car ID pools due to typo:

vendors[vendors$car_ID=="172104419", visualization_vars, with=F]

#Subcase 3: mix of subcase 1 and 2:
vendors[vendors$car_ID=="126511389", visualization_vars, with=F]


# Behavior 2: Vendors using vendor_ID pools -------------------------------------------

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


# Behavior 3: Vendors changing specs: -------------------------------------------------
vendors<- vendors[order(vendors$prices_firstDate),]
vendors<- vendors[,`:=`(n_changes_total=.N,
                        n_unique_changes_price=length(unique(valuePrice)),
                        n_unique_changes_km= length(unique(Kilometer)),
                        n_unique_changes_Hubraum=length(unique(Hubraum)),
                        n_unique_changes_Klimatisierung=length(unique(Klimatisierung)),
                        n_unique_changes_Leistung=length(unique(Leistung)),
                        n_unique_changes_HU=length(unique(HU)), 
                        n_unique_changes_Eigenschaften=length(unique(Eigenschaften)),
                        avg_TOM_price=mean(TOM)), 
                        by=.(car_ID, vendor_ID, Typ, Erstzulassung)] 


#Examples:

View(vendors[vendors$car_ID=="221099", ])


# Behavior 4: Vendors chaging specs and car IDs-----------------------------------------------

vendors<- vendors[,`:=`(similar_cars_diff_IDs=length(unique(car_ID)), 
                        monotonicity= data.table::shift(valuePrice, 1, NA, "lag")),
                  by=.(Typ, Erstzulassung, Schaltung, 
                       Leistung, Eigenschaften, vendor_ID, Farbe)]

# Identify observations that were grouped to the same car, but don't have monotonically sinking prices:
vendors$monotonicity[is.na(vendors$monotonicity)]<- vendors$valuePrice[is.na(vendors$monotonicity)] 
vendors$price_monotonicity<- vendors$valuePrice-vendors$monotonicity

vendors<- vendors[,`:=`(monotonicity_check=max(price_monotonicity)), 
                  by=.(Typ, Erstzulassung, Schaltung, 
                      Leistung, Eigenschaften, vendor_ID)]

#Examples:


#  Behavior 5: Vendors with leasing offers ---------------------------------------------

vendors<- vendors[,`:=`(leasing_cars=length(unique(car_ID))), 
                        by=.(Typ, Erstzulassung, Schaltung, 
                       Leistung, Eigenschaften, prices_firstDate, vendor_ID, Farbe)]



# Save data ---------------------------------------------------------------

#save(vendors, file="C:/Users/Nk/Documents/Uni/MA/Pkw/MobileDaten/generatedData/Merged_data/vendor_behavior_after_data_clean.RData")
load("C:/Users/Nk/Documents/Uni/MA/Pkw/MobileDaten/generatedData/Merged_data/vendor_behavior_after_data_clean.RData")

# Factor analysis for vendor behavior --------------------------------------------------

factor_vars<- c("vendor_ID",
                "car_ID",
                #Behavior 1:
                "n_carID_pool",
                
                #Behavior 2:
                "n_unique_changes_vendor_pool",
                
                #Behavior 3:
                "n_unique_changes_price",
                "n_unique_changes_km",
                "n_unique_changes_Hubraum",
                "n_unique_changes_Klimatisierung",
                "n_unique_changes_Leistung",
                "n_unique_changes_HU", 
                "n_unique_changes_Eigenschaften",
                
                #Behavior 4:
                "similar_cars_diff_IDs",
                
                #Behavior 5:
                "leasing_cars"
)

vendors<- vendors[,factor_vars, with=F]

# Determine Number of Factors to Extract
library(nFactors)
ev <- eigen(cor(vendors[,-c(1), with=F])) # get eigenvalues
ap <- parallel(subject=nrow(vendors[,-c(1), with=F]),var=ncol(vendors[,-c(1), with=F]),
               rep=10,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

# According to scree plot  -> 5 behaviors can be distinguished: 
# => Perform factor analysis with 5 factors

fit1 <- factanal(vendors[,-c(1), with=F], 4, rotation="varimax")
print(fit1, digits=2, cutoff=.3, sort=TRUE)

#Cluster vendor_IDs:

# Cluster vendor_IDs ------------------------------------------------------

vendors_cluster<- vendors[, .(n_carID_pool=max(n_carID_pool),
                          
                          #Behavior 2:
                          n_unique_changes_vendor_pool=max(n_unique_changes_vendor_pool),
                          
                          #Behavior 3:
                          n_unique_changes_price=max(n_unique_changes_price),
                          n_unique_changes_km=max(n_unique_changes_km),
                          n_unique_changes_Hubraum=max(n_unique_changes_Hubraum),
                          n_unique_changes_Klimatisierung=max(n_unique_changes_Klimatisierung),
                          n_unique_changes_Leistung= max(n_unique_changes_Leistung),
                          n_unique_changes_HU= max(n_unique_changes_HU), 
                          n_unique_changes_Eigenschaften=max(n_unique_changes_Eigenschaften),
                          
                          #Behavior 4:
                          similar_cars_diff_IDs=max(similar_cars_diff_IDs),
                          
                          #Behavior 5:
                          leasing_cars=max(similar_cars_diff_IDs))

                          , by=.(vendor_ID)]

#Cluster vendors:

#Extract number of clusters:
wss <- (nrow(vendors_cluster[,-c(1),with=F])-1)*sum(apply(vendors_cluster[,-c(1),with=F],2,var))
for (i in 2:15) wss[i] <- sum(kmeans(vendors_cluster[,-c(1),with=F], 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")



fit <- kmeans(scale(vendors_cluster[,-c(1),with=F]), 5) # 5 cluster solution
# get cluster means 
aggregate(vendors_cluster[,-c(1),with=F],by=list(fit$cluster),FUN=mean)
# append cluster assignment
vendors_cluster <- cbind(vendors_cluster, fit$cluster)


#Plot cluster:

# vary parameters for most readable graph
library(cluster) 
clusplot(vendors_cluster[,-c(1),with=F], fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(vendors_cluster[,-c(1),with=F], fit$cluster)


# Cluster car_IDs ---------------------------------------------------------

cars_cluster<- vendors[, .(n_carID_pool=max(n_carID_pool),
                           
                           #Behavior 2:
                           n_unique_changes_vendor_pool=max(n_unique_changes_vendor_pool),
                           
                           #Behavior 3:
                           n_unique_changes_price=max(n_unique_changes_price),
                           n_unique_changes_km=max(n_unique_changes_km),
                           n_unique_changes_Hubraum=max(n_unique_changes_Hubraum),
                           n_unique_changes_Klimatisierung=max(n_unique_changes_Klimatisierung),
                           n_unique_changes_Leistung= max(n_unique_changes_Leistung),
                           n_unique_changes_HU= max(n_unique_changes_HU), 
                           n_unique_changes_Eigenschaften=max(n_unique_changes_Eigenschaften),
                           
                           #Behavior 4:
                           similar_cars_diff_IDs=max(similar_cars_diff_IDs),
                           
                           #Behavior 5:
                           leasing_cars=max(similar_cars_diff_IDs))
                       
                       , by=.(car_ID)]

#Extract number of clusters:
wss <- (nrow(cars_cluster[,-c(1),with=F])-1)*sum(apply(cars_cluster[,-c(1),with=F],2,var))
for (i in 2:15) wss[i] <- sum(kmeans(cars_cluster[,-c(1),with=F], 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")



fit <- kmeans(scale(cars_cluster[,-c(1),with=F]), 5) # 5 cluster solution
# get cluster means 
aggregate(cars_cluster[,-c(1),with=F],by=list(fit$cluster),FUN=mean)
# append cluster assignment
cars_cluster <- cbind(cars_cluster, fit$cluster)


#Plot cluster:

# vary parameters for most readable graph
library(cluster) 
clusplot(vendors_cluster[,-c(1),with=F], fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(vendors_cluster[,-c(1),with=F], fit$cluster)

