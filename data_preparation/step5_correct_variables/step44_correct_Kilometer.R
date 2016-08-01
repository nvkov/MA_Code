#Correct Kilometer:

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

# Look at Kilometer--------------------------------------------------------

summary(df$Kilometer)


# Save data ---------------------------------------------------------------
save(df, file="df_merge_after_step44.RData")

# End ---------------------------------------------------------------------

