#Survival analysis:

rm(list=ls())
library("data.table")
library("sets")
library("survival")

#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<-"/Pkw/MobileDaten/generatedData/step5/"

load(paste0(project_directory, data_directory, "new_df_merge_A180.RData"))

temp$newDOP<- as.factor(ifelse(temp$DOP==1, 1, ifelse(temp$DOP>1, 2, ifelse(tempDOP>0.5, 3,0))))
temp$newMS<- as.factor(ifelse(temp$MS<=52,1, 
                              ifelse(temp$MS<483, 2, 
                                    ifelse(temp$MS<650, 3, 4)) ))
temp<-temp[temp$DOP<2,]

mini.surv <- survfit(Surv(temp$newTOM, temp$right_censored)~ temp$newDOP+ temp$age, conf.type="none")

summary(mini.surv)

plot(mini.surv, lty=c(6, 1, 4, 3),col=c(4,2,3,1), xlab="Time", ylab="Survival Probability")
legend(800, 1, c("0", "1", "2", "3"), 
       lty=c(6, 1, 4, 3), col=c(4,2,3,1))


a<- survfit(coxph(Surv(temp$TOM,temp$right_censored)~1), type="aalen")
summary(a)