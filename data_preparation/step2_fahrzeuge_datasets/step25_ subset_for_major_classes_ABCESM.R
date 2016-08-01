#Part 1: General steps, ideas, progress summary and questions
rm(list=ls())

#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<-"/Pkw/MobileDaten/generatedData/CleanFahrzeuge/"
wd<- paste0(project_directory, data_directory)

setwd(wd)

library("data.table")
library("stringr")

#Read files with car specifications:
load("cars_full_after_step24.RData")

#Change format of dates for Erstzulassung and lastChange:
df<- df[grep("A|B|C|E|S|M", df$Typ),]
df<- df[!grep("Sprinter|AMG|CL|Citan|T|ANDERE", df$Typ),]

#Correct unexisting names with Hubraum:
df<-df[grep("[A-Z][0-9]{2}0|[A-Z][0-9]{1}5|[0-9]{2}0[A-Z]", df$Typ),]

#Same but without S55/S65 or the scraped cars:
df<-df[grep("[A-Z][0-9]{2}0", df$Typ),]


# Check if all car names are realistic ------------------------------------
tab_typ<- table(df$Typ)
tab_typ[tab_typ<100]

# Subset all car types with 100 or less observations  ---------------------

drop<- as.vector(unlist(dimnames(tab_typ[tab_typ<100])))


# Subset unrealistic obs --------------------------------------------------

df<- df[!df$Typ %in% drop,]


setwd(project_directory)
sink("cars_descriptive_statistics.txt", append=T)
print("Step 25: Number of cars after subsetting for major classes")
nrow(df)
sink()

#-------------------------------------------------------------------------------
save(df, file=paste0(project_directory, data_directory, "cars_full_after_step25.RData"))

