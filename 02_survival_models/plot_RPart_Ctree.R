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
rpart<- rpart(fitform, data=train)

tree<- as.party(rpart)
png("C:/Users/Nk/Documents/Uni/MA/Graphs/rpartAseries.png")
plot(tree, type="simple")
dev.off()


ctree<- ctree(fitform,contro=ctree_control(mincriterion = 0.9999), data=train)

png("C:/Users/Nk/Documents/Uni/MA/Graphs/ctreeAseries.png")
plot(ctree)
dev.off()




