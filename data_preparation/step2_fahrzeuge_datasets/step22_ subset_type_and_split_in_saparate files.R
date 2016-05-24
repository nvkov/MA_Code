#
rm(list=ls())

#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<-"/Pkw/MobileDaten/generatedData/CleanFahrzeuge/"
wd<- paste0(project_directory)

setwd(wd)

library("data.table")
library("stringr")

#Load car_data and look for doubles:
load(paste0(project_directory, data_directory, "cars_commercial_vendors_full.RData"))

sink("cars_descriptive_statistics.txt")
print("Number of cars before looking for doubles, merged file")
nrow(df)
sink()


#Keep only relevant models: (A, B, C, E, S, M):

#A-Class:
cars_df_A<- df[grep("A[0-9]{3}|[0-9]{3}A", df$Typ),]
cars_df_A<- cars_df_A[!grep("AMG", cars_df_A$Typ),]
save(cars_df_A, file=paste0(project_directory, data_directory, "cars_df_A.RData"))

sink("cars_descriptive_statistics.txt", append=T)
print("Number of A-Class cars  before looking for doubles, merged file")
nrow(cars_df_A)
sink()

#B-Class:
cars_df_B<- df[grep("B[0-9]{3}|[0-9]{3}B", df$Typ),]
cars_df_B<- cars_df_B[!grep("MB", cars_df_B$Typ),]
save(cars_df_B, file=paste0(project_directory, data_directory, "cars_df_B.RData"))


sink("cars_descriptive_statistics.txt", append=T)
print("Number of B-Class cars  before looking for doubles, merged file")
nrow(cars_df_B)
sink()

#C-Class:
cars_df_C<- df[grep("C[0-9]{3}|[0-9]{3}C", df$Typ),]
cars_df_C<- cars_df_C[!grep("L|S|SE|SEC", cars_df_C$Typ),]
save(cars_df_C, file=paste0(project_directory, data_directory, "cars_df_C.RData"))


sink("cars_descriptive_statistics.txt", append=T)
print("Number of C-Class cars  before looking for doubles, merged file")
nrow(cars_df_C)
sink()

#E-Class:
cars_df_E<- df[grep("E[0-9]{3}|[0-9]{3}E", df$Typ),]
cars_df_E<- cars_df_E[!grep("S|T|G|C", cars_df_E$Typ),]
save(cars_df_E, file=paste0(project_directory, data_directory, "cars_df_E.RData"))


sink("cars_descriptive_statistics.txt", append=T)
print("Number of E-Class cars  before looking for doubles, merged file")
nrow(cars_df_E)
sink()


#S-Class:
cars_df_S<- df[grep("S[0-9]{3}|[0-9]{3}S", df$Typ),]
save(cars_df_S, file=paste0(project_directory, data_directory, "cars_df_S.RData"))

sink("cars_descriptive_statistics.txt", append=T)
print("Number of S-Class cars  before looking for doubles, merged file")
nrow(cars_df_S)
sink()


#M-Class:
cars_df_M<- df[grep("M", df$Typ),]
cars_df_M<- cars_df_M[!grep("AMG", cars_df_M$Typ),]
save(cars_df_M, file=paste0(project_directory, data_directory, "cars_df_M.RData"))

sink("cars_descriptive_statistics.txt", append=T)
print("Number of M-Class cars  before looking for doubles, merged file")
nrow(cars_df_M)
sink()

