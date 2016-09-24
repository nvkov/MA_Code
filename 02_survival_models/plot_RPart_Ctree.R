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

set.seed(22)
# Select subsample for tests ----------------------------------------------
dat<- df1[,i:=.I][sample(i, 13500)]

# Select form -------------------------------------------------------------
fitform<- Surv(newTOM,status)~ MS + DOP + Quantile + age + size_vendor


# Select parameters -------------------------------------------------------
rpart<- rpart(fitform, data=dat)

tree<- as.party(rpart)
pdf("C:/Users/Nk/Documents/Uni/MA/Graphs/rpartSample.pdf")
plot(tree) 
dev.off()


ctree<- ctree(fitform,contro=ctree_control(mincriterion = 0.999999), data=dat)
pdf("C:/Users/Nk/Documents/Uni/MA/Graphs/ctreeSample.pdf", width=20)
plot(ctree)
#dev.off()

node_surv(ctree, mainlab=)

pdf("C:/Users/Nk/Documents/Uni/MA/Graphs/ctreeSample.pdf", width=20)
plot(ctree, gp = gpar(fontsize = 2),     # font size changed to 6
     inner_panel=node_inner,
     ip_args=list(
       abbreviate = F, 
       id = T
       ), 
     terminal_panel=node_surv, 
     tp_args=list(id=F)
)
dev.off()



