#Assess full dataset:

rm(list=ls())
library("data.table")
library("sets")
library("survival")

#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<-"/Pkw/MobileDaten/generatedData/final_dataset/"

wd<- paste0(project_directory, data_directory)
setwd(wd)

#Load dataset:
load("df_full2007.RData")


# Load Mode function ------------------------------------------------------

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#################################################################
df$survival<- ifelse(df$right_censored==1, 0, 1)

# Add relevant variables --------------------------------------------------

df$year<- year(df$Erstzulassung)

# Look at Leistung --------------------------------------------------------

table(df$Leistung)

#create new Leitung variable:

df$Leistung<- as.numeric(as.character(df$Leistung))
df$newLeistung<- df$Leistung


# Plot Leistung according to a model --------------------------------------

# car_names<- unique(df$Typ)
# 
# sink("C:/Users/Nk/Documents/Uni/MA/descriptiv_stats/Inspect_leistung.txt", append=FALSE, split=FALSE)
# for(i in car_names){
#   for(j in c("70", "77")){
#     
#     temp.lvl1<- df[df$newLeistung[df$Typ==i & df$Kraftstoff==j],]  
#     
#     for(k in unique(temp.lvl1$year)){ 
#       temp.lvl2<- table(temp.lvl1$newLeistung[temp.lvl1$year==k])
#       print(paste0("Model: ", i, " ; fuel: ", j,  "; year: ", k))
#     
#       if(length(temp.lvl2)<=3){print(sort(temp.lvl2))}else{
#              print(sort(temp.lvl2)[I(length(temp.lvl2)-2):length(temp.lvl2)])
#         }
#      }
#   }
# }
# sink()


# Crosstable for leistung: type, fuel, year -------------------------------

summary_leistung<- df[, .(Count=.N), by=.(newLeistung, Leistung, Typ, Kraftstoff, year)]

# Recode unrealistic values to missing ------------------------------------
summary_leistung$newLeistung[summary_leistung$newLeistung<55] <- NA
summary_leistung$newLeistung[summary_leistung$newLeistung>300] <- NA

# Impute coerced missings in the hash table -------------------------------

summary_leistung[,newLeistung:= replace(newLeistung, is.na(newLeistung)==T, Mode(newLeistung[!is.na(newLeistung)])), 
   by=.(Typ, Kraftstoff, year)]


save(summary_leistung, file="C:/Users/Nk/Documents/Uni/MA/MA_Code/post_data_prep/variables_summary/summary_lesitung.Rdata")
#--------------------------------------------------------------------------


# Recode categories with no strong variation (see shiny) ------------------------------
hash_table<- summary_leistung
hash_table$newLeistung[hash_table$Typ=="A140" & Kraftstoff=="77"]<- 60
hash_table$newLeistung[hash_table$Typ=="A150"]<- 70
hash_table$newLeistung[hash_table$Typ=="A150"]<- 70

hash_table$newLeistung[hash_table$Typ=="A160"]<- 







# Plot a changing table: --------------------------------------------------

