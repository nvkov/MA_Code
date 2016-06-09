#Part 1: General steps, ideas, progress summary and questions
rm(list=ls())

#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<-"/Pkw/MobileDaten/generatedData/CleanFahrzeuge/"
data_directory_general<- "/Pkw/MobileDaten/generatedData/"
wd<- paste0(project_directory, data_directory)

setwd(wd)

library("data.table")
library("stringr")

#Read files with car specifications:
load("cars_full_after_step25.RData")
load("C:/Users/Nk/Documents/Uni/MA/Pkw/MobileDaten/generatedData/TOM/prices_df_full.Rdata")

setnames(df, "MobileID", "car_ID")
setkeyv(df, "car_ID")
setkeyv(tomdf, "car_ID")


df_merge<-tomdf[df,]
df_merge$Anzeigenanlage<- as.IDate(as.character(df_merge$Anzeigenanlage), format="%Y%m%d")

rm(df, tomdf)

setwd(project_directory)
sink("merged_data_descriptive_statistics.txt")
print("Number of rows after left join for cars_df")
nrow(df_merge)
sink()

df_merge$prices_firstDate<- as.IDate(df_merge$prices_firstDate, format="%Y-%m-%d")
df_merge$cars_lastChange<- as.IDate(df_merge$cars_lastChange, format="%Y-%m-%d")
df_merge$prices_lastDate<- as.IDate(df_merge$prices_lastDate, format="%Y-%m-%d")

df_merge<- subset(df_merge, df_merge$prices_firstDate<=df_merge$cars_lastDate 
                                    & df_merge$prices_lastDate>=df_merge$cars_lastChange
                                    & df_merge$prices_firstDate>=df_merge$Anzeigenanlage)
                                    
# Car change that indicates new entry or changed prices (#NB! Need to include cars registered before 18. Sept 2008. It's an exception):
#df_merge_first_time_prices<- subset(df_merge, df_merge$cars_lastChange<=df_merge$prices_firstDate)
#df_merge_first<- subset(df_merge, df_merge$cars_lastChange==df_merge$prices_firstDate)


# Cars that were corrected shortly after insertion
#df_merge_small_changes<- subset(df_merge, df_merge$cars_lastChange>I(df_merge$prices_firstDate +1))

#Car changes, but not price changes:
#df_merge_car_changes_same_prices<- subset(df_merge, df_merge$cars_lastChange>df_merge$prices_firstDate)

# Split df_merge in first time prices with no further changes and cars with price and car changes:




setwd(project_directory)
sink("merged_data_descriptive_statistics.txt", append=T)
print("Number of rows after left join for cars_df and subset for realistic dates")
nrow(df_merge)
sink()

save(df_merge, file=paste0(project_directory, data_directory_general, "Merged_data/df_merge_after_step31.RData" ))
#save(df_merge, file=paste0(project_directory, data_directory_general, "Merged_data/df_merge_first_time_prices_after_step31.RData" ))
#save(df_merge, file=paste0(project_directory, data_directory_general, "Merged_data/df_merge_car_changes_after_step31.RData" ))

#Inspect car changes:
#inspect_car_change<- df_merge[df_merge$car_ID %in% df_merge_car_changes_same_prices$car_ID & df_merge$car_ID %in% df_merge_small_changes$car_ID, ]
#inspect_car_change2<-df_merge_small_changes[df_merge_small_changes$car_ID %in% df_merge_car_changes_same_prices$car_ID,]

#inspect_changed_cars<- df_merge[df_merge$car_ID %in% inspect_car_change2$car_ID]
