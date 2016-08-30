#Plot stuff:


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

#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/MA"
data_directory<-"/Pkw/MobileDaten/generatedData/final_dataset/"

wd<- paste0(project_directory, data_directory)
setwd(wd)

#Load dataset:
load("ready_for_survival.RData")


# Select subsample for tests ----------------------------------------------
df_A<- df1[grep("A", df1$Class),]
rm(df1)

# Split in test and training ----------------------------------------------
df_A$rows<- rownames(df_A)
setkey(df_A, "rows")
set.seed(42)
split<- sample(rownames(df_A), size=floor(0.6*nrow(df_A)))


train1<- df_A[split,]
valid1<- df_A[!split,]
rm(df_A)

nrows<- list(as.integer(c(1:200)), as.integer(c(2:300)))
train<- train1[1:2000]
valid<- valid1[2000:30000]



# Select form -------------------------------------------------------------
fitform<- Surv(newTOM,status)~ MS + DOP + Quantile + age + size_vendor
fitform1<- Surv(newTOM,status)~ MS + DOP + Quantile + age 
fitform2<- Surv(newTOM,status)~ MS + DOP + Quantile +  size_vendor
fitform3<- Surv(newTOM,status)~ MS + DOP +  age + size_vendor
fitform4<- Surv(newTOM,status)~ MS  + Quantile + age + size_vendor
fitform5<- Surv(newTOM,status)~ DOP + Quantile + age + size_vendor


# Select parameters -------------------------------------------------------
cox.fitform<- list(fitform, fitform1, fitform2, fitform3, fitform4, fitform5)

# creating model arrays
m.cox <- array(vector(mode = "list", length = 1), c(2, length(cox.fitform)))

#define validation sets:
d<- c(1:nrow(valid))
validationsets<- as.list(split(d, ceiling(seq_along(d)/1500)))


cox<- selectCox(fitform, data=train)
rsf<- rfsrc(fitform, data=train, ntree = 50)
cforest<- pecCforest(fitform, data=train, control=cforest_control(ntree=50))

spcox<- NULL
sprsf<- NULL
spcforest<- NULL


# Plot three examples -----------------------------------------------------

par(mfrow=c(1,3))

for(nrow in c(2537, 8217, 281)){
  spcox<- predictSurvProb(cox, newdata=valid[nrow,], time=c(1:60))
  sprsf<- predictSurvProb(rsf, newdata=valid[nrow,], time=c(1:60))
  spcforest<- predictSurvProb(cforest, newdata=valid[nrow,], time=c(1:60))
  
  png("C:/Users/Nk/Documents/Uni/MA/Graphs/exampleSurvProb.png") 
      #type="cairo", 
      #units="px", 
      #pointsize=12
      #)
  
  plot(c(1:60), spcox, type="l", col="black", xlab="days", ylab="Survival probability",lwd=2)
  lines(c(1:60),sprsf, col="green", lwd=2)
  lines(c(1:60),spcforest, col="blue", lwd=2)
  abline(v=valid$newTOM[nrow], col="red", xlab="Days", ylab="Predicted survival probability", lwd=2)
  legend(20, 0.9, c("RSF", "CoxPH", "CForest"), col=c("black", "green", "blue"), lwd=c(2,2,2))

}
dev.off()



#  ------------------------------------------------------------------------
