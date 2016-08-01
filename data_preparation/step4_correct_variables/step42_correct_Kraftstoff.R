#Correct Kraftstoff:

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

###########################################################################

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

# Save data ---------------------------------------------------------------
save(df, file="df_full2007.RData")

# End ---------------------------------------------------------------------

