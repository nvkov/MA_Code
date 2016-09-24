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


# Helper functions --------------------------------------------------------

add_legend <- function(...) {
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
              mar=c(0, 0, 0, 0), new=TRUE)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
  } 

#  ------------------------------------------------------------------------


#Load dataset:
load("ready_for_survival.RData")


# Select subsample for tests ----------------------------------------------

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

pdf("C:/Users/Nk/Documents/Uni/MA/Graphs/KMDOP.pdf")
plot(KM.DOP.Q1, xlab="Survival time", ylab="Survival probability")
lines(KM.DOP.Q2, col="red")
lines(KM.DOP.Q3, col="blue")
lines(KM.DOP.Q4, col="green")
#legend(300,0.9, c("Q1", "Q2", "Q3", "Q4"), lwd=c(2,2,2,2), col=c("black", "red", "blue", "green"))

add_legend("topright", legend=c(bquote(Q1<.(round(DOPbounds[2], digits=2))), 
                                bquote(Q2<.(round(DOPbounds[3], digits=2))),
                                bquote(Q3<.(round(DOPbounds[4], digits=2))),
                                bquote(Q4<.(round(DOPbounds[5], digits=2)))), pch=19, 
           col=c("black", "red", "blue", "green"),
           horiz=TRUE, bty='n', cex=0.9) 

dev.off()




# plot MS Categories ------------------------------------------------------
MSbounds<- quantile(train1$MS)

KM.MS.Q1<- survfit(fitform0, data=train1[train1$MS<=MSbounds[2]])
KM.MS.Q2<- survfit(fitform0, data=train1[train1$MS>=MSbounds[2] & train1$MS<=MSbounds[3]])
KM.MS.Q3<- survfit(fitform0, data=train1[train1$MS<=MSbounds[4] & train1$MS>=MSbounds[3]])
KM.MS.Q4<- survfit(fitform0, data=train1[train1$MS<=MSbounds[5] & train1$MS>=MSbounds[4]])

pdf("C:/Users/Nk/Documents/Uni/MA/Graphs/KMMS.pdf")
plot(KM.MS.Q1, xlab="Survival time", ylab="Survival probability")
lines(KM.MS.Q2, col="red")
lines(KM.MS.Q3, col="blue")
lines(KM.MS.Q4, col="green")
add_legend("topright", legend=c(bquote(Q1<.(round(MSbounds[2], digits=2))), 
                                bquote(Q2<.(round(MSbounds[3], digits=2))),
                                bquote(Q3<.(round(MSbounds[4], digits=2))),
                                bquote(Q4<.(round(MSbounds[5], digits=2)))), pch=19, 
           col=c("black", "red", "blue", "green"),
           horiz=TRUE, bty='n', cex=0.9) 


dev.off()

# plot MS Categories ------------------------------------------------------
Qbounds<- quantile(train1$Quantile)

KM.Q.Q1<- survfit(fitform0, data=train1[train1$Quantile<=Qbounds[2]])
KM.Q.Q2<- survfit(fitform0, data=train1[train1$Quantile>=Qbounds[2] & train1$Quantile<=Qbounds[3]])
KM.Q.Q3<- survfit(fitform0, data=train1[train1$Quantile<=Qbounds[4] & train1$Quantile>=Qbounds[3]])
KM.Q.Q4<- survfit(fitform0, data=train1[train1$Quantile<=Qbounds[5] & train1$Quantile>=Qbounds[4]])

pdf("C:/Users/Nk/Documents/Uni/MA/Graphs/KMQ.pdf")
plot(KM.Q.Q1)
lines(KM.Q.Q2, col="red", xlab="Survival time", ylab="Survival probability")
lines(KM.Q.Q3, col="blue")
lines(KM.Q.Q4, col="green")
add_legend("topright", legend=c(bquote(Q1<.(round(Qbounds[2], digits=2))), 
                                bquote(Q2<.(round(Qbounds[3], digits=2))),
                                bquote(Q3<.(round(Qbounds[4], digits=2))),
                                bquote(Q4<.(round(Qbounds[5], digits=2)))), pch=19, 
           col=c("black", "red", "blue", "green"),
           horiz=TRUE, bty='n', cex=0.9) 

dev.off()

# plot age ----------------------------------------------------------------

Abounds<- quantile(train1$age)

KM.A.Q1<- survfit(fitform0, data=train1[train1$age<=Abounds[2]])
KM.A.Q2<- survfit(fitform0, data=train1[train1$age>=Abounds[2] & train1$age<=Abounds[3]])
KM.A.Q3<- survfit(fitform0, data=train1[train1$age<=Abounds[4] & train1$age>=Abounds[3]])
KM.A.Q4<- survfit(fitform0, data=train1[train1$age<=Abounds[5] & train1$age>=Abounds[4]])

pdf("C:/Users/Nk/Documents/Uni/MA/Graphs/KMage.pdf")
plot(KM.A.Q1, xlab="Survival time", ylab="Survival probability")
lines(KM.A.Q2, col="red")
lines(KM.A.Q3, col="blue")
lines(KM.A.Q4, col="green")
add_legend("topright", legend=c(bquote(Q1<.(round(Abounds[2], digits=2))), 
                                bquote(Q2<.(round(Abounds[3], digits=2))),
                                bquote(Q3<.(round(Abounds[4], digits=2))),
                                bquote(Q4<.(round(Abounds[5], digits=2)))), pch=19, 
           col=c("black", "red", "blue", "green"),
           horiz=TRUE, bty='n', cex=0.9) 

dev.off()

# plot Vendor size --------------------------------------------------------

Vendorbounds<- quantile(train1$size_vendor)

KM.V.Q1<- survfit(fitform0, data=train1[train1$size_vendor<=Vendorbounds[2]])
KM.V.Q2<- survfit(fitform0, data=train1[train1$size_vendor>=Vendorbounds[2] & train1$size_vendor<=Vendorbounds[3]])
KM.V.Q3<- survfit(fitform0, data=train1[train1$size_vendor<=Vendorbounds[4] & train1$size_vendor>=Vendorbounds[3]])
KM.V.Q4<- survfit(fitform0, data=train1[train1$size_vendor<=Vendorbounds[5] & train1$size_vendor>=Vendorbounds[4]])
KM.V.Q0<- survfit(fitform0, data=train1[train1$size_vendor<=Vendorbounds[1],])

pdf("C:/Users/Nk/Documents/Uni/MA/Graphs/KMvendor.pdf")
plot(KM.V.Q1, xlab="Survival time", ylab="Survival probability")
lines(KM.V.Q2, col="red")
lines(KM.V.Q3, col="blue")
lines(KM.V.Q4, col="green")
add_legend("topright", legend=c(bquote(Q1<.(round(Vendorbounds[2], digits=2))), 
                                bquote(Q2<.(round(Vendorbounds[3], digits=2))),
                                bquote(Q3<.(round(Vendorbounds[4], digits=2))),
                                bquote(Q4<.(round(Vendorbounds[5], digits=2)))), pch=19, 
           col=c("black", "red", "blue", "green"),
           horiz=TRUE, bty='n', cex=0.9) 

#lines(KM.V.Q0, col="purple")
dev.off()

