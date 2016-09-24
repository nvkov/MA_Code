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


# Select form -------------------------------------------------------------
fitform<- Surv(newTOM,status)~ MS + DOP + Quantile + age + size_vendor


# Select parameters -------------------------------------------------------
cox.Class<- c("A", "B", "C", "E",  "ML", "S", "SL", "SLK")
cox.Vars<- c("MS", "DOP", "Quantile", "age", "size_vendor")
# creating parameter grids
cox.grid <- expand.grid(Class=cox.Class)
grid   <- list(cox.grid) 

# calculating number of models
models <- 1 + nrow(cox.grid) 

# creating model arrays
m.cox <- array(vector(mode = "list", length = 1), c(2, nrow(cox.grid)))

# creating error matrix
errors <- vector(mode = "numeric", length = models)
names(errors) <- rep("RFS", length(errors))

# setting up the clock
time.set <- array(vector(mode = "list", length = 1), c(1, 10))
time.set[[1, 1]] <- time.start

coeffs<- matrix("NA",nrow=length(cox.Class), ncol=5)
# Calculate models --------------------------------------------------------

print("Estimating Cox...")
for (i in 1:nrow(cox.grid)) {
  
  # displaying model number
  print(paste0("Model ", i, " out of ", nrow(cox.grid)))
  
  # training models
  m.cox[[i]] <-coxph(fitform, data=df1[df1$Class==cox.grid$Class[i],])
  
}  


# information
time.set[[1, 4]] <- proc.time()
time.trial <- time.set[[1, 4]] - time.set[[1, 3]]
print(paste0("Random Survival forest took ", round(time.trial[3]/60, digits = 0), " minutes."))
#beep(5)


# Save models in latex table ----------------------------------------------

sink("C:/Users/Nk/Documents/Uni/MA/MA_Code/Results/tab_cph_Classes.tex")
stargazer(m.cox[[1]],m.cox[[2]],m.cox[[3]],m.cox[[4]],m.cox[[5]],m.cox[[6]],m.cox[[7]],m.cox[[8]],
          apply.coef = exp,
          apply.se = exp, 
          label="tab:cphE",
          title="Cox proportional hazards. Comparing buyer preferences for major Mercedes-Benz Classes", 
          covariate.labels = c("Market size", "DOP", "Quantile",
                               "Age", "Size vendor"),
          dep.var.caption  = "Dependent variable",
          dep.var.labels   = "Time on market (in days)",
          column.labels = c("A", "B", "C", "E", "M", 
                            "S", "SL", "SLK"),  
          no.space = T,
          summary=F, 
          align=TRUE, 
          column.sep.width = "2pt", 
          float.env="sidewaystable", 
          font.size = "tiny", 
          t.auto=F, 
          p.auto=F, 
          report = "vc*")
sink()

save(gridsearch.rfs, file="C:/Users/Nk/Documents/Uni/MA/MA_Code/Results/gridserach.rfs5000.RData")

fastbw(coxph(fitform, data=df1[1:1000,]), rule="aic")

# Plot Schoenfeld residuals -----------------------------------------------
coxfull<- coxph(fitform, data=df1)


# Save as Latex table:
sink("C:/Users/Nk/Documents/Uni/MA/Tables/CoxzphFull.txt")
print(cox.zph(coxfull)) 
sink()

# pdf("C:/Users/Nk/Documents/Uni/MA/Tables/CoxFullSchoenfeld_residuals.pdf")
# par(mfrow=c(1,5))
# plot(cox.zph(coxfull,transform='log'), col="red", resid=F)
# dev.off()


# Plot martingale residuals -----------------------------------------------
res <- residuals(coxfull, type="martingale")
X <- as.matrix(df1[, c("MS", "DOP", "Quantile", "age", "size_vendor"), with=F]) # matrix of covariates

pdf("C:/Users/Nk/Documents/Uni/MA/Tables/CoxFullResiduals.pdf", width=12)

par(mfrow=c(2, 5), mar=c(5,1,1,1), oma=c(0,2,0,0))

  for (j in 1:5) { # residual plots
   plot(X[, j], res, xlab=c("MS", "DOP", "Quantile", "Age", "Size vendors")[j], ylab="residuals", 
        ylim=c(-1, 1), cex.lab=1.5, type="n")
   abline(h=0, lty=2)
   lines(lowess(X[, j], res, iter=0), col="red", lwd=1.75)
   }
 plot(cox.zph(coxfull,transform='log'), col="red", resid=F, lwd=1.75)
 
 dev.off() 

# Stepwise Regression AIC criterion ---------------------------------------

# Stepwise Regression:
 library(MASS)
 sink("C:/Users/Nk/Documents/Uni/MA/Tables/CoxBWSelectionASeries.txt")
 step <- stepAIC(coxph(fitform, data=df1), direction="both")
 sink()
 step$anova # display results
 
# Save as Latex table:
 sink("C:/Users/Nk/Documents/Uni/MA/Tables/CoxzphASeries.txt")
print(cox.zph(step)) 
 sink()