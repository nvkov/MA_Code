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
train<- train1[20000:25000]
valid<- valid1[2000:3500]



# Select form -------------------------------------------------------------
fitform<- Surv(newTOM,status)~ MS + DOP + Quantile + age + size_vendor


# Select parameters -------------------------------------------------------
#rfs.ntree<- seq(10,50,10)
cforest.ntree<- c(50)
#rfs.mtry<- seq(2,4,1)
cforest.mtry<- c(2)
cforest.mincriterion<- (seq(0.90, 0.99, 0.01))
#rfs.nodedepth=c(5)
#d<- c(1:nrow(train))
#rfs.nrows<- as.list(split(d, ceiling(seq_along(d)/5000)))

# creating parameter grids
cforest.grid <- expand.grid(ntree = cforest.ntree,  mtry=cforest.mtry, mincriterion=cforest.mincriterion)
grid   <- list(rfs.grid) 

# creating model arrays
m.cforest <- array(vector(mode = "list", length = 1), c(2, nrow(cforest.grid)))

# creating error matrix
errors <- vector(mode = "numeric", length = models)
names(errors) <- rep("RFS", length(errors))

# setting up the clock
time.set <- array(vector(mode = "list", length = 1), c(1, 10))
time.set[[1, 1]] <- time.start

oob.brier<- matrix(nrow = nrow(rfs.grid), ncol = 1)
# Calculate models --------------------------------------------------------

# k<- 1
forecasts<- NULL
bcvCindex<- NULL

print("Training regularized logit...")
for (i in 1:nrow(cforest.grid)) {
  
  # displaying model number
  print(paste0("Model ", i, " out of ", nrow(cforest.grid)))
  
  # training models
  m.cf<- pecCforest(fitform, control=cforest_control(mincriterion = cforest.grid$mincriterion[i], 
                                                     ntree=cforest.grid$ntree[i], 
                                                     mtry=cforest.grid$mtry[i]), data=train)
  
  
  bcvCindex[i]  <- cindex(list("CForest"=m.cf),
                          formula=fitform,
                          data=valid,
                          splitMethod="bootcv",
                          B=5,
                          eval.times=c(1:30))$BootCvCindex
}

png("C:/Users/Nk/Documents/Uni/MA/Graphs/CforestMincriterion2.png")
plot(c(1:30), bcvCindex[[1]], type="l", ylab="Concordance index", xlab="Time", ylim=c(0.63, 0.75), lwd=2.5)
lines(c(1:30), bcvCindex[[2]], col="red", lwd=2.5)
lines(c(1:30), bcvCindex[[3]], col="blue", lwd=2.5)
lines(c(1:30), bcvCindex[[4]], col="green", lwd=2.5)
lines(c(1:30), bcvCindex[[5]], col="pink", lwd=2.5)

lines(c(1:30), bcvCindex[[6]], col="red", lwd=2.5, lty=2)
lines(c(1:30), bcvCindex[[7]], col="blue", lwd=2.5, lty=2)
lines(c(1:30), bcvCindex[[8]], col="green", lwd=2.5, lty=2)
lines(c(1:30), bcvCindex[[9]], col="pink", lwd=2.5, lty=2)
lines(c(1:30), bcvCindex[[10]], col="black", lwd=2.5, lty=2)

legend(22, 0.75, c("0.90","0.91","0.92","0.93","0.94","0.95", 
                   "0.96","0.97", "0.98", "0.99"), 
       col=c("black", "red", "blue", "green", "pink",  "red", "blue", "green", "pink", "black"),
       lwd=c(2,2,2,2,2,2,2,2,2), lty=c(1,1,1,1,1,2,2,2,2,2))
dev.off()