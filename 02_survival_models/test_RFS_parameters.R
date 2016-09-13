#Test RFS parameters:

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
train<- train1
valid<- valid1[2000:3500]


# Select form -------------------------------------------------------------
fitform<- Surv(newTOM,status)~ MS + DOP + Quantile + age + size_vendor


# Select parameters -------------------------------------------------------
#rfs.ntree<- seq(10,50,10)
rfs.ntree<- c(1:200)
#rfs.mtry<- seq(2,4,1)
rfs.mtry<- 1
#rfs.nodedepth=c(5)
rfs.splitrule<-c("logrank")
d<- c(1:nrow(train))
rfs.nrows<- as.list(split(d, ceiling(seq_along(d)/2000)))

# creating parameter grids
rfs.grid <- expand.grid(ntree = rfs.ntree, splitrule=rfs.splitrule, mtry=rfs.mtry, hi=rfs.nrows)
grid   <- list(rfs.grid) 

# calculating number of models
models <- 1 + nrow(rfs.grid) 

# creating model arrays
m.rfs <- array(vector(mode = "list", length = 1), c(2, nrow(rfs.grid)))

# creating error matrix
errors <- vector(mode = "numeric", length = models)
names(errors) <- rep("RFS", length(errors))

# setting up the clock
time.set <- array(vector(mode = "list", length = 1), c(1, 10))
time.set[[1, 1]] <- time.start

oob.brier<- matrix(nrow = nrow(rfs.grid), ncol = 1)
# Calculate models --------------------------------------------------------

k<- 1
forecasts<- NULL
print("Training regularized logit...")
for (i in 1:nrow(rfs.grid)) {
  
  # displaying model number
  print(paste0("Model ", i, " out of ", nrow(rfs.grid)))
  
  # training models
  #m.rfs[[1, i]] <-rfsrc(fitform, ntree=rfs.grid$ntree[i], splitrule=rfs.grid$splitrule[i], mtry=rfs.grid$mtry[i], data=train[as.integer(rfs.grid$nrows[[i]])]) 
  #m.temp <-rfsrc(fitform, ntree=rfs.grid$ntree[i], splitrule=rfs.grid$splitrule[i], mtry=rfs.grid$mtry[i], data=train[as.integer(rfs.grid$nrows[[i]])]) 
  m.temp <-rfsrc(fitform, ntree=rfs.grid$ntree[i], splitrule=rfs.grid$splitrule[i], mtry=rfs.grid$mtry[i], data=train[20000:25000,]) 
  
  
  # extracting predictions
  #forecasts[i] <- m.rfs[[1,i]]$err.rate[!is.na(m.rfs[[1,i]]$err.rate)]
  forecasts[i] <- m.temp$err.rate[!is.na(m.temp$err.rate)]

}  

# information
time.set[[1, 4]] <- proc.time()
time.trial <- time.set[[1, 4]] - time.set[[1, 3]]
print(paste0("Random Survival forest took ", round(time.trial[3]/60, digits = 0), " minutes."))
#beep(5)

rfs.results<- cbind(rfs.grid, forecasts)

save(rfs.results, file="C:/Users/Nk/Documents/Uni/MA/MA_Code/Results/rsf.results5000_ASeries.RData")

forecasts2<- forecasts

png("C:/Users/Nk/Documents/Uni/MA/MA_Code/Results/rsf_ntree_stable.png")
plot(c(1:200), forecasts1, type="l", xlab="Time", ylab="Predicted error")
lines(c(1:200), forecasts2, type="l", col="red")
lines(c(1:200), forecasts, type="l", col="blue")

legend(150, 0.42, legend=c("mtry=1", "mtry=2","mtry=3"),
       lty=c(1,1,1), 
       lwd=c(2.5,2.5, 2.5), col=c("blue","black","red"))
dev.off()




m.temp <-rfsrc(fitform, ntree=10, splitrule="conserve", mtry=2, data=train[1:300,]) 


m.temp <-rfsrc(fitform, ntree=120, mtry=2, data=train[20000:25000,]) 

plot(as.party(m.temp))

plot(gg_vimp(m.temp)) +
  theme(legend.position = c(0.8, 0.2)) +
  labs(fill = "VIMP > 0")


varsel_pbc <- var.select(m.temp)
gg_md <- gg_minimal_depth(varsel_pbc)
print(gg_md)

plot(gg_minimal_vimp(gg_md)) +
  theme(legend.position=c(0.8, 0.2))

gg_v <- gg_variable(m.temp, time = c(10,20, 30, 60),
                    time.labels = c("10 days","20 days", "30 days", "60 days"))

plot(gg_v, xvar = "Quantile", alpha = 0.9) + labs(y = "Survival") +
  theme(legend.position = "none") +
  scale_color_manual(values = strCol, labels = event.labels) +
  scale_shape_manual(values = event.marks, labels = event.labels) +
  coord_cartesian(ylim = c(-0.01, 1.02))








