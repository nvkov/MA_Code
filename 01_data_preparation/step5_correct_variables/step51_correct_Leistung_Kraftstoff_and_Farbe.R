#Correct Leistung:

rm(list=ls())
library("data.table")
library("sets")
library("survival")

#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<-"/Pkw/MobileDaten/generatedData/Merged_data/"

wd<- paste0(project_directory, data_directory)
setwd(wd)

#Load dataset:
load("df_merge_after_step43.RData")

# Load Mode function ------------------------------------------------------

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

###########################################################################

# Add relevant variables --------------------------------------------------

df$year<- year(df$Erstzulassung)

# Look at Leistung --------------------------------------------------------

table(df$Leistung)

#create new Leitung variable:

df$Leistung<- as.numeric(as.character(df$Leistung))
df$newLeistung<- df$Leistung


###########################################################################
#                 Explore Leistung for wrong values:                      #   
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
summary_leistung$newLeistung[summary_leistung$newLeistung>540] <- NA

# Impute coerced missings in the hash table -------------------------------

summary_leistung<- summary_leistung[,`:=`(newLeistung = replace(newLeistung, is.na(newLeistung)==T, Mode(newLeistung[!is.na(newLeistung)])),
                                          type_mode_leistung=Mode(newLeistung[!is.na(newLeistung)])), 
                                    by=.(Typ, Kraftstoff, year)]

#--------------------------------------------------------------------------

# Identify categories with slight deviation from the real value -----------
# Find distance to type modal value:
summary_leistung$leistung_deviation_mode<- summary_leistung$newLeistung - summary_leistung$type_mode_leistung
summary_leistung$newLeistung<- summary_leistung$newLeistung - summary_leistung$leistung_deviation_mode


# Impute the rest missing on type and Kraftstoff only ---------------------

summary_leistung<- summary_leistung[,`:=`(newLeistung = replace(newLeistung, is.na(newLeistung)==T, Mode(newLeistung[!is.na(newLeistung)]))), 
                                    by=.(Typ, Kraftstoff)]


# Save hash table ---------------------------------------------------------

save(summary_leistung, file="C:/Users/Nk/Documents/Uni/MA/MA_Code/post_data_prep/variables_summary/summary_lesitung.Rdata")
                                                                       # 
###########################################################################

# Perform same steps on dataset: ------------------------------------------

# Step 1: Find frequent values: -------------------------------------------
df<- df[, Count:=.N, by=.(newLeistung, Leistung, Typ, Kraftstoff, year)]


# Step 2: Recode unrealistic values to missing ----------------------------
df$newLeistung[df$newLeistung<55] <- NA
df$newLeistung[df$newLeistung>540] <- NA

# Step 3: Impute coerced missings in the hash table -----------------------

df<- df[,`:=`(newLeistung = replace(newLeistung, is.na(newLeistung)==T, Mode(newLeistung[!is.na(newLeistung)])),
                                          type_mode_leistung=Mode(newLeistung[!is.na(newLeistung)])), 
                                    by=.(Typ, Kraftstoff, year)]


# Step 4: Identify categories with slight deviation from the real value ---
# Find distance to type modal value:
df$leistung_deviation_mode<- df$newLeistung - df$type_mode_leistung
df$newLeistung<- df$newLeistung - df$leistung_deviation_mode


# Step 5: Impute the rest missing on type and Kraftstoff only --------------

df<- df[,`:=`(newLeistung = replace(newLeistung, is.na(newLeistung)==T, Mode(newLeistung[!is.na(newLeistung)]))), 
                                    by=.(Typ, Kraftstoff)]


# Step 6: Impute the rest missing only on type ----------------------------


df<- df[,`:=`(newLeistung = replace(newLeistung, is.na(newLeistung)==T, Mode(newLeistung[!is.na(newLeistung)]))), 
        by=.(Typ)]


# Bin Leistung ------------------------------------------------------------

df$Leistung<- df$newLeistung
df$Leistung[df$Leistung==66]<- 70
df$Leistung[df$Leistung==69]<- 70

df$Leistung[df$Leistung==77]<- 80
df$Leistung[df$Leistung==81]<- 80
df$Leistung[df$Leistung==83]<- 85
df$Leistung[df$Leistung==91]<- 90
df$Leistung[df$Leistung==92]<- 90

df$Leistung[df$Leistung==102]<- 103

df$Leistung[df$Leistung==119]<- 120
df$Leistung[df$Leistung==156]<- 155
df$Leistung[df$Leistung==153]<- 155
df$Leistung[df$Leistung==162]<- 160
df$Leistung[df$Leistung==169]<- 170
df$Leistung[df$Leistung==172]<- 173
df$Leistung[df$Leistung==185]<- 184
df$Leistung[df$Leistung==204]<- 205

df$Leistung[df$Leistung==206]<- 205
df$Leistung[df$Leistung==210]<- 205
df$Leistung[df$Leistung==220]<- 215

df$Leistung[df$Leistung==222]<- 224
df$Leistung[df$Leistung==244]<- 250
df$Leistung[df$Leistung==253]<- 250
df$Leistung[df$Leistung==270]<- 285
df$Leistung[df$Leistung==281]<- 285
df$Leistung[df$Leistung==325]<- 320



# drop unnecessary variables ----------------------------------------------

irrelCols<- c("Count", "type_mode_leistung", "leistung_deviation_mode")
df[, irrelCols]<- NULL


###########################################################################
# Correct Kraftstoff ------------------------------------------------------

# Look at Kraftstoff-------------------------------------------------------

table(df$Kraftstoff)

# Recode missings:
df$Kraftstoff<- as.numeric(as.character(df$Kraftstoff))
df$oldKraftstoff<- df$Kraftstoff

# Impute on type, car performance, categorie: 
df<- df[,`:=`(Kraftstoff = replace(Kraftstoff, is.na(Kraftstoff)==T, Mode(Kraftstoff[!is.na(Kraftstoff)]))), 
        by=.(Typ, newLeistung, Kategorie)]


# Impute on type, car performance: 

df<- df[,`:=`(Kraftstoff = replace(Kraftstoff, is.na(Kraftstoff)==T, Mode(Kraftstoff[!is.na(Kraftstoff)]))), 
        by=.(Typ, newLeistung)]

# Impute on type: 

df<- df[,`:=`(Kraftstoff = replace(Kraftstoff, is.na(Kraftstoff)==T, Mode(Kraftstoff[!is.na(Kraftstoff)]))), 
        by=.(Typ)]

# drop unnecessary variables ----------------------------------------------

irrelCols<- c("oldKraftstoff")
df[, irrelCols]<- NULL


###########################################################################

# Bin Farbe ---------------------------------------------------------------


df$color_cat<- ifelse(df$Farbe %in% c("40", "59", "41", "58", "50", "51", "110"), "classic", ifelse(df$Farbe=="", "NA", "extravagant"))




# Save data ---------------------------------------------------------------
save(df, file="df_merge_after_step51.RData")

# End ---------------------------------------------------------------------


