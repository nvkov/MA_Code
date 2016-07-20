#Assess full dataset:

rm(list=ls())
library("data.table")
library("sets")
library("survival")
library("xtable")

#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<-"/Pkw/MobileDaten/generatedData/final_dataset/"
table_directory<- "C:/Users/Nk/Documents/Uni/MA/MA_Code/Tables/"

wd<- paste0(project_directory, data_directory)
setwd(wd)

#Load dataset:
load("dataset_full.RData")

# Correct scraped names: --------------------------------------------------

# Drop unrealistic names
df[grep("[1-9]{3}", df$Typ)]<- NA

#Switch reverse names:
df$Typ[grep("[0-9]{3}[A-Z]", df$Typ)]<- paste0(regmatches(df$Typ[grep("[0-9]{3}[A-Z]", df$Typ)], gregexpr("[A-Z]+", df$Typ[grep("[0-9]{3}[A-Z]", df$Typ)])),
                                               regmatches(df$Typ[grep("[0-9]{3}[A-Z]", df$Typ)], gregexpr("[0-9]+", df$Typ[grep("[0-9]{3}[A-Z]", df$Typ)])))


# Take a look at different classes: ---------------------------------------

# A-Class:
sort(table(df$Typ[grep("A", df$Typ)]))
     
# B-Class:
sort(table(df$Typ[grep("B", df$Typ)]))

# C-Class:
sort(table(df$Typ[grep("C", df$Typ)]))

# S-Class:
sort(table(df$Typ[grep("S", df$Typ)]))

# M-Class:
sort(table(df$Typ[grep("M", df$Typ)]))


# Drop observations from unrealistic or rare classes ----------------------

drop_types<- unlist(dimnames(table(df$Typ)[table(df$Typ)<=100]))

df[df$Typ %in% drop_types,]<- NA

df[grep("E55", df$Typ)]<- NA
df[grep("S55", df$Typ)]<- NA

# Erase the NA-Coerced Types ----------------------------------------------

df<- df[!is.na(df$Typ),]

# save table with left cars:
sink(file=paste0(table_directory, "tab_car_types.txt"), append=F)
xtable(table(df$Typ), 
       caption="List of car types included in the analysis", 
       label="tab:cartypes")
sink()


# Save data ---------------------------------------------------------------

save(df, file=paste0(project_directory, data_directory, "df_full2007.RData"))