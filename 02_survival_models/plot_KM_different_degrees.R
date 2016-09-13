#Plot RFS parameters:

rm(list=ls())
library("data.table")
library("sets")
library("survival")
library("stargazer")
library("beepr")
library("rpart")
library("partykit")
library("coin")
library("caret")
library("verification") # for Brier score
library("pec")
library("survAUC")
library("LTRCtrees")
library("peperr")
library("rms")
library("randomForestSRC")
library("party")
library("ipred")
library("survivalROC")
library("ggplot")
library("doMC")
registerDoMC()

#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<-"/Pkw/MobileDaten/generatedData/final_dataset/"

wd<- paste0(project_directory, data_directory)
setwd(wd)

#Load dataset:
load("ready_for_survival.RData")


# Select subsample for tests ----------------------------------------------
#df_A<- df1[grep("A", df1$Class),]
#rm(df1)

# Split in test and training ----------------------------------------------
# df_A$rows<- rownames(df_A)
# setkey(df_A, "rows")
# set.seed(42)
# split<- sample(rownames(df_A), size=floor(0.6*nrow(df_A)))
# 
# 
# train1<- df_A[split,]
# valid1<- df_A[!split,]
# rm(df_A)
# 
# nrows<- list(as.integer(c(1:200)), as.integer(c(2:300)))
# train<- train1[20000:25000]
# valid<- valid1[2000:3500]

train1<- df1

# Select form -------------------------------------------------------------
fitform<- Surv(newTOM,status)~ MS + DOP + Quantile + age + size_vendor
fitform0<- Surv(newTOM,status)~1

# Select bounds -----------------------------------------------------------

DOPbounds<- quantile(train1$DOP)


KM.train<- survfit(fitform0, data=train1)
KM.test<- survfit(fitform0, data=valid1)

plot(KM.train)
lines(KM.test, col="red")

# plot DOP Categories -----------------------------------------------------
KM.DOP.Q1<- survfit(fitform0, data=train1[train1$DOP<=DOPbounds[2]])
KM.DOP.Q2<- survfit(fitform0, data=train1[train1$DOP>=DOPbounds[2] & train1$DOP<=DOPbounds[3]])
KM.DOP.Q3<- survfit(fitform0, data=train1[train1$DOP<=DOPbounds[4] & train1$DOP>=DOPbounds[3]])
KM.DOP.Q4<- survfit(fitform0, data=train1[train1$DOP<=DOPbounds[5] & train1$DOP>=DOPbounds[4]])

png("C:/Users/Nk/Documents/Uni/MA/Graphs/KMDOP.png")
plot(KM.DOP.Q1, xlab="Survival time", ylab="Survival probability")
lines(KM.DOP.Q2, col="red")
lines(KM.DOP.Q3, col="blue")
lines(KM.DOP.Q4, col="green")
legend(300,0.9, c("Q1", "Q2", "Q3", "Q4"), lwd=c(2,2,2,2), col=c("black", "red", "blue", "green"))
dev.off()




# plot MS Categories ------------------------------------------------------
MSbounds<- quantile(train1$MS)

KM.MS.Q1<- survfit(fitform0, data=train1[train1$MS<=MSbounds[2]])
KM.MS.Q2<- survfit(fitform0, data=train1[train1$MS>=MSbounds[2] & train1$MS<=MSbounds[3]])
KM.MS.Q3<- survfit(fitform0, data=train1[train1$MS<=MSbounds[4] & train1$MS>=MSbounds[3]])
KM.MS.Q4<- survfit(fitform0, data=train1[train1$MS<=MSbounds[5] & train1$MS>=MSbounds[4]])

png("C:/Users/Nk/Documents/Uni/MA/Graphs/KMMS.png")
plot(KM.MS.Q1, xlab="Survival time", ylab="Survival probability")
lines(KM.MS.Q2, col="red")
lines(KM.MS.Q3, col="blue")
lines(KM.MS.Q4, col="green")
dev.off()

# plot MS Categories ------------------------------------------------------
Qbounds<- quantile(train1$Quantile)

KM.Q.Q1<- survfit(fitform0, data=train1[train1$Quantile<=Qbounds[2]])
KM.Q.Q2<- survfit(fitform0, data=train1[train1$Quantile>=Qbounds[2] & train1$Quantile<=Qbounds[3]])
KM.Q.Q3<- survfit(fitform0, data=train1[train1$Quantile<=Qbounds[4] & train1$Quantile>=Qbounds[3]])
KM.Q.Q4<- survfit(fitform0, data=train1[train1$Quantile<=Qbounds[5] & train1$Quantile>=Qbounds[4]])

png("C:/Users/Nk/Documents/Uni/MA/Graphs/KMQ.png")
plot(KM.Q.Q1)
lines(KM.Q.Q2, col="red", xlab="Survival time", ylab="Survival probability")
lines(KM.Q.Q3, col="blue")
lines(KM.Q.Q4, col="green")
dev.off()

# plot age ----------------------------------------------------------------

Abounds<- quantile(train1$age)

KM.A.Q1<- survfit(fitform0, data=train1[train1$age<=Abounds[2]])
KM.A.Q2<- survfit(fitform0, data=train1[train1$age>=Abounds[2] & train1$age<=Abounds[3]])
KM.A.Q3<- survfit(fitform0, data=train1[train1$age<=Abounds[4] & train1$age>=Abounds[3]])
KM.A.Q4<- survfit(fitform0, data=train1[train1$age<=Abounds[5] & train1$age>=Abounds[4]])

png("C:/Users/Nk/Documents/Uni/MA/Graphs/KMage.png")
plot(KM.A.Q1, xlab="Survival time", ylab="Survival probability")
lines(KM.A.Q2, col="red")
lines(KM.A.Q3, col="blue")
lines(KM.A.Q4, col="green")
dev.off()

# plot Vendor size --------------------------------------------------------

Vendorbounds<- quantile(train1$size_vendor)

KM.V.Q1<- survfit(fitform0, data=train1[train1$size_vendor<=Vendorbounds[2]])
KM.V.Q2<- survfit(fitform0, data=train1[train1$size_vendor>=Vendorbounds[2] & train1$size_vendor<=Vendorbounds[3]])
KM.V.Q3<- survfit(fitform0, data=train1[train1$size_vendor<=Vendorbounds[4] & train1$size_vendor>=Vendorbounds[3]])
KM.V.Q4<- survfit(fitform0, data=train1[train1$size_vendor<=Vendorbounds[5] & train1$size_vendor>=Vendorbounds[4]])
KM.V.Q0<- survfit(fitform0, data=train1[train1$size_vendor<=Vendorbounds[1],])

png("C:/Users/Nk/Documents/Uni/MA/Graphs/KMvendor.png")
plot(KM.V.Q1, xlab="Survival time", ylab="Survival probability")
lines(KM.V.Q2, col="red")
lines(KM.V.Q3, col="blue")
lines(KM.V.Q4, col="green")
#lines(KM.V.Q0, col="purple")
dev.off()

