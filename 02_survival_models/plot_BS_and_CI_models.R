# Plot Brier score and C-index:

rm(list=ls())

setwd("C:/Users/Nk/Documents/Uni/MA/results_ensembles/")
# Helper functions: -------------------------------------------------------

is.NullOb <- function(x) is.null(x) | all(sapply(x, is.null))

## Recursively step down into list, removing all such objects 
rmNullObs <- function(x) {
  x <- Filter(Negate(is.NullOb), x)
  lapply(x, function(x) if (is.list(x)) rmNullObs(x) else x)
  
}

add_legend <- function(...) {
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
              mar=c(0, 0, 0, 0), new=TRUE)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
}



# Load data ---------------------------------------------------------------
pattern<- "train31"
files.list<- list.files(pattern=paste0("*", pattern, ".RData"))

lapply(files.list, FUN=load, .GlobalEnv)
BSnames<- ls(pattern="BS*")
CInames<- ls(patter="CI*")

BScforest<- as.data.frame(rmNullObs(BScforest))
colnames(BScforest)<- c(1:100)

BSctree<- as.data.frame(rmNullObs(BSctree))
colnames(BSctree)<- c(1:100)
BSrsf<- as.data.frame(rmNullObs(BSrsf))
colnames(BSrsf)<- c(1:100)
BSrpart<- as.data.frame(rmNullObs(BSrpart))
colnames(BSrpart)<- c(1:100)
BSCox<- as.data.frame(rmNullObs(BSCox))
colnames(BSCox)<- c(1:100)


BSrpart<- as.data.frame(BSrpart)
meanRpart<- apply(BSrpart, 1, mean)
sdRpart<- apply(BSrpart, 1, sd)
ulRpart<- meanRpart + 1.98*sdRpart

BSrsf<- as.data.frame(BSrsf)
meanRsf<- apply(BSrsf, 1, mean)
sdRsf<- apply(BSrsf, 1, sd)

BSctree<- as.data.frame(BSctree)
meanCtree<- apply(BSctree, 1, mean)
sdCtree<- apply(BSctree, 1, sd)

BSCox<- as.data.frame(BSCox)
meanCox<- apply(BSCox, 1, mean)
sdCox<- apply(BSCox, 1, sd)


BSCforest<- as.data.frame(BScforest)
meanCforest<- apply(BSCforest, 1, mean)
sdCforest<- apply(BSCforest, 1, sd)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

pdf(paste0("C:/Users/Nk/Documents/Uni/MA/Plots_BS/BSmeans", pattern, ".pdf"))
par(mar = c(4, 4, 1.6, 0.2))

plot(meanRsf, ylim=c(0,0.3), type="l", lwd=2, ylab = "Brier Score", xlab="days")
lines(meanRpart, col="red", lwd=2)
lines(meanCtree, col="blue", lwd=2)
lines(meanCox, col="pink", lwd=2)
lines(meanCforest, col="green", lwd=2)
abline(h=0.25, col="grey", lty=2)

add_legend("topright", legend=c("RSF", "Cforest", "Cox", "Rpart", "Ctree"), pch=19, 
           col=c("black", "green", "pink", "red", "blue"),
           horiz=TRUE, bty='n', cex=0.9)
dev.off()


# Plot the CI -------------------------------------------------------------

CIcforest<- as.data.frame(rmNullObs(CIcforest))
colnames(CIcforest)<- c(1:length(CIcforest))

CIctree<- as.data.frame(rmNullObs(CIctree))
colnames(CIctree)<- c(1:length(CIctree))
CIrsf<- as.data.frame(rmNullObs(CIrsf))
colnames(CIrsf)<- c(1:length(CIrsf))
CIrpart<- as.data.frame(rmNullObs(CIrpart))
colnames(CIrpart)<- c(1:length(CIrpart))
CICox<- as.data.frame(rmNullObs(CICox))
colnames(CICox)<- c(1:length(CICox))


CIrpart<- as.data.frame(CIrpart)
meanRpart<- apply(CIrpart, 1, mean)
sdRpart<- apply(CIrpart, 1, sd)
ulRpart<- meanRpart + 1.98*sdRpart

CIrsf<- as.data.frame(CIrsf)
meanRsf<- apply(CIrsf, 1, mean)
sdRsf<- apply(CIrsf, 1, sd)

CIctree<- as.data.frame(CIctree)
meanCtree<- apply(CIctree, 1, mean)
sdCtree<- apply(CIctree, 1, sd)

CICox<- as.data.frame(CICox)
meanCox<- apply(CICox, 1, mean)
sdCox<- apply(CICox, 1, sd)


CICforest<- as.data.frame(CIcforest)
meanCforest<- apply(CICforest, 1, mean)
sdCforest<- apply(CICforest, 1, sd)

pdf(paste0("C:/Users/Nk/Documents/Uni/MA/Plots_CI/CImeans", pattern, ".pdf"))
par(mar = c(4, 4, 1.6, 0.2))
plot(meanRsf, ylim=c(0.55, 0.75), type="l", lwd=2)
#lines(I(meanRsf+1.98*sdRsf), col="black", lty=2)
#lines(I(meanRsf-1.98*sdRsf), col="black", lty=2)


#lines(I(meanRpart+1.98*sdRpart), col="red", lty=2)
#lines(I(meanRpart-1.98*sdRpart), col="red", lty=2)

lines(meanRpart, col="red", lwd=2)
lines(meanCtree, col="blue", lwd=2)
abline(h=0.25, col="grey", lty=2 )

#lines(I(meanCtree+1.98*sdCtree), col="blue", lty=2)
#lines(I(meanCtree-1.98*sdCtree), col="blue", lty=2)


lines(meanCox, col="pink", lwd=2)
#lines(I(meanCox+1.98*sdCox), col="pink", lty=2)
#lines(I(meanCox-1.98*sdCox), col="pink", lty=2)


lines(meanCforest, col="green", lwd=2)
#lines(I(meanCforest+1.98*sdCforest), col="green", lty=2)
#lines(I(meanCforest-1.98*sdCforest), col="green", lty=2)
# 
# legend(80, 0.7, c("RSF", "Cforest", "Cox", "Rpart", "Ctree"), col=c("black", "green", "pink", "red", "blue"),
#        lwd=c(2,2,2,2,2))

add_legend("topright", legend=c("RSF", "Cforest", "Cox", "Rpart", "Ctree"), pch=19, 
           col=c("black", "green", "pink", "red", "blue"),
           horiz=TRUE, bty='n', cex=0.9)
dev.off()

# Build difference matrix -------------------------------------------------

rsfcforest<- BSrsf- BScforest 
rsfctree<- BSrsf- BSctree
rsfrpart<- BSrsf- BSrpart
rsfcox<- BSrsf- BSCox

coxrsf<- BSCox - BSrsf
coxcforest<- BSCox - BScforest
coxctree<- BSCox - BSctree
coxrpart<- BSCox - BSrpart

cforestrsf<- BScforest - BSrsf 
cforestctree<- BScforest - BSctree
cforestrpart<- BScforest - BSrpart
cforestcox<- BScforest - BSCox

rpartrsf<- BSrpart - BSrsf
rpartctree<- BSrpart - BSctree
rpartcforest<- BSrpart - BScforest
rpartcox<- BSrpart - BSCox

ctreerpart<- BSctree - BSrpart
ctreersf<- BSctree - BSrsf
ctreecforest<- BSctree - BScforest
ctreecox<- BSctree - BSCox



# Calculate means and sds -------------------------------------------------

meanrsfcforest<- apply(rsfcforest, 1, mean) 
meanrsfctree<- apply(rsfctree, 1, mean)
meanrsfrpart<- apply(rsfrpart, 1, mean)
meanrsfcox<- apply(rsfcox, 1, mean)

meancoxrsf<- apply(coxrsf, 1, mean)
meancoxcforest<- apply(coxcforest, 1, mean)
meancoxctree<- apply(coxctree, 1, mean)
meancoxrpart<- apply(coxrpart, 1, mean)

meancforestrsf<- apply(cforestrsf, 1, mean) 
meancforestctree<- apply(cforestctree, 1, mean)
meancforestrpart<- apply(cforestrpart, 1, mean)
meancforestcox<- apply(cforestcox, 1, mean)

meanrpartrsf<- apply(rpartrsf, 1, mean)
meanrpartctree<- apply(rpartctree, 1, mean)
meanrpartcforest<- apply(rpartcforest, 1, mean)
meanrpartcox<- apply(rpartcox, 1, mean)

meanctreerpart<- apply(ctreerpart, 1, mean)
meanctreersf<- apply(ctreersf, 1, mean)
meanctreecforest<- apply(ctreecforest, 1, mean)
meanctreecox<- apply(ctreecox, 1, mean)


# Calculate standard errors -----------------------------------------------

sdrsfcforest<- apply(rsfcforest, 1, sd) 
sdrsfctree<- apply(rsfctree, 1, sd)
sdrsfrpart<- apply(rsfrpart, 1, sd)
sdrsfcox<- apply(rsfcox, 1, sd)

sdcoxrsf<- apply(coxrsf, 1, sd)
sdcoxcforest<- apply(coxcforest, 1, sd)
sdcoxctree<- apply(coxctree, 1, sd)
sdcoxrpart<- apply(coxrpart, 1, sd)

sdcforestrsf<- apply(cforestrsf, 1, sd) 
sdcforestctree<- apply(cforestctree, 1, sd)
sdcforestrpart<- apply(cforestrpart, 1, sd)
sdcforestcox<- apply(cforestcox, 1, sd)

sdrpartrsf<- apply(rpartrsf, 1, sd)
sdrpartctree<- apply(rpartctree, 1, sd)
sdrpartcforest<- apply(rpartcforest, 1, sd)
sdrpartcox<- apply(rpartcox, 1, sd)

sdctreerpart<- apply(ctreerpart, 1, sd)
sdctreersf<- apply(ctreersf, 1, sd)
sdctreecforest<- apply(ctreecforest, 1, sd)
sdctreecox<- apply(ctreecox, 1, sd)


# Plot differences --------------------------------------------------------
png("C:/Users/Nk/Documents/Uni/MA/Plots_BS/rsfcforest.png")
#par(mfrow=c(1,4))
plot(c(1:120), meanrsfcforest, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="")
lines(c(1:120), I(meanrsfcforest-1.98*sdrsfcforest), lty=2)
lines(c(1:120), I(meanrsfcforest+1.98*sdrsfcforest), lty=2)
abline(h=0, col="red")
dev.off()

png("C:/Users/Nk/Documents/Uni/MA/Plots_BS/rsfcox.png")
plot(c(1:120), meanrsfcox, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="")
lines(c(1:120), I(meanrsfcox-1.98*sdrsfcox), lty=2)
lines(c(1:120), I(meanrsfcox+1.98*sdrsfcox), lty=2)
abline(h=0, col="red")
dev.off()

png("C:/Users/Nk/Documents/Uni/MA/Plots_BS/rsfctree.png")
plot(c(1:120), meanrsfctree, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="")
lines(c(1:120), I(meanrsfctree-1.98*sdrsfctree), lty=2)
lines(c(1:120), I(meanrsfctree+1.98*sdrsfctree), lty=2)
abline(h=0, col="red")
dev.off()

png("C:/Users/Nk/Documents/Uni/MA/Plots_BS/rsfrpart.png")
plot(c(1:120), meanrsfrpart, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="")
lines(c(1:120), I(meanrsfrpart-1.98*sdrsfrpart), lty=2)
lines(c(1:120), I(meanrsfrpart+1.98*sdrsfrpart), lty=2)
abline(h=0, col="red")
dev.off()

#  ------------------------------------------------------------------------

png("C:/Users/Nk/Documents/Uni/MA/Plots_BS/cforestcox.png")
plot(c(1:120), meancforestcox, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="")
lines(c(1:120), I(meancforestcox-1.98*sdcforestcox), lty=2)
lines(c(1:120), I(meancforestcox+1.98*sdcforestcox), lty=2)
abline(h=0, col="red")
dev.off()


png("C:/Users/Nk/Documents/Uni/MA/Plots_BS/cforestrsf.png")
plot(c(1:120), meancforestrsf, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="")
lines(c(1:120), I(meancforestrsf-1.98*sdcforestrsf), lty=2)
lines(c(1:120), I(meancforestrsf+1.98*sdcforestrsf), lty=2)
abline(h=0, col="red")
dev.off()


png("C:/Users/Nk/Documents/Uni/MA/Plots_BS/cforestctree.png")
plot(c(1:120), meancforestctree, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="")
lines(c(1:120), I(meancforestctree-1.98*sdcforestctree), lty=2)
lines(c(1:120), I(meancforestctree+1.98*sdcforestctree), lty=2)
abline(h=0, col="red")
dev.off()


png("C:/Users/Nk/Documents/Uni/MA/Plots_BS/cforestrpart.png")
plot(c(1:120), meancforestrpart, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="")
lines(c(1:120), I(meancforestrpart-1.98*sdcforestrpart), lty=2)
lines(c(1:120), I(meancforestrpart+1.98*sdcforestrpart), lty=2)
abline(h=0, col="red")
dev.off()


#  ------------------------------------------------------------------------



png("C:/Users/Nk/Documents/Uni/MA/Plots_BS/coxrsf.png")
plot(c(1:120), meancoxrsf, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="")
lines(c(1:120), I(meancoxrsf-1.98*sdcoxrsf), lty=2)
lines(c(1:120), I(meancoxrsf+1.98*sdcoxrsf), lty=2)
abline(h=0, col="red")
dev.off()


png("C:/Users/Nk/Documents/Uni/MA/Plots_BS/coxctree.png")
plot(c(1:120), meancoxctree, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="")
lines(c(1:120), I(meancoxctree-1.98*sdcoxctree), lty=2)
lines(c(1:120), I(meancoxctree+1.98*sdcoxctree), lty=2)
abline(h=0, col="red")
dev.off()


png("C:/Users/Nk/Documents/Uni/MA/Plots_BS/coxcforest.png")
plot(c(1:120), meancoxcforest, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="")
lines(c(1:120), I(meancoxcforest-1.98*sdcoxcforest), lty=2)
lines(c(1:120), I(meancoxcforest+1.98*sdcoxcforest), lty=2)
abline(h=0, col="red")
dev.off()

png("C:/Users/Nk/Documents/Uni/MA/Plots_BS/coxrpart.png")
plot(c(1:120), meancoxrpart, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="")
lines(c(1:120), I(meancoxrpart-1.98*sdcoxrpart), lty=2)
lines(c(1:120), I(meancoxrpart+1.98*sdcoxrpart), lty=2)
abline(h=0, col="red")
dev.off()


#  ------------------------------------------------------------------------


png("C:/Users/Nk/Documents/Uni/MA/Plots_BS/rpartcox.png")
plot(c(1:120), meanrpartcox, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="")
lines(c(1:120), I(meanrpartcox-1.98*sdrpartcox), lty=2)
lines(c(1:120), I(meanrpartcox+1.98*sdrpartcox), lty=2)
abline(h=0, col="red")
dev.off()

png("C:/Users/Nk/Documents/Uni/MA/Plots_BS/rpartrsf.png")
plot(c(1:120), meanrpartrsf, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="")
lines(c(1:120), I(meanrpartrsf-1.98*sdrpartrsf), lty=2)
lines(c(1:120), I(meanrpartrsf+1.98*sdrpartrsf), lty=2)
abline(h=0, col="red")
dev.off()

png("C:/Users/Nk/Documents/Uni/MA/Plots_BS/rpartcforest.png")
plot(c(1:120), meanrpartcforest, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="")
lines(c(1:120), I(meanrpartcforest-1.98*sdrpartcforest), lty=2)
lines(c(1:120), I(meanrpartcforest+1.98*sdrpartcforest), lty=2)
abline(h=0, col="red")
dev.off()

png("C:/Users/Nk/Documents/Uni/MA/Plots_BS/rpartctree.png")
plot(c(1:120), meanrpartctree, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="")
lines(c(1:120), I(meanrpartctree-1.98*sdrpartctree), lty=2)
lines(c(1:120), I(meanrpartctree+1.98*sdrpartctree), lty=2)
abline(h=0, col="red")
dev.off()


#  ------------------------------------------------------------------------

png("C:/Users/Nk/Documents/Uni/MA/Plots_BS/ctreecox.png")
plot(c(1:120), meanctreecox, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="")
lines(c(1:120), I(meanctreecox-1.98*sdctreecox), lty=2)
lines(c(1:120), I(meanctreecox+1.98*sdctreecox), lty=2)
abline(h=0, col="red")
dev.off()

png("C:/Users/Nk/Documents/Uni/MA/Plots_BS/ctreecforest.png")
plot(c(1:120), meanctreecforest, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="")
lines(c(1:120), I(meanctreecforest-1.98*sdctreecforest), lty=2)
lines(c(1:120), I(meanctreecforest+1.98*sdctreecforest), lty=2)
abline(h=0, col="red")
dev.off()

png("C:/Users/Nk/Documents/Uni/MA/Plots_BS/ctreersf.png")
plot(c(1:120), meanctreersf, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="")
lines(c(1:120), I(meanctreersf-1.98*sdctreersf), lty=2)
lines(c(1:120), I(meanctreersf+1.98*sdctreersf), lty=2)
abline(h=0, col="red")
dev.off()

png("C:/Users/Nk/Documents/Uni/MA/Plots_BS/ctreerpart.png")
plot(c(1:120), meanctreerpart, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="")
lines(c(1:120), I(meanctreerpart-1.98*sdctreerpart), lty=2)
lines(c(1:120), I(meanctreerpart+1.98*sdctreerpart), lty=2)
abline(h=0, col="red")
dev.off()

# Experiment with layout --------------------------------------------------
dev.off()



#pdf("C:/Users/Nk/Documents/Uni/MA/Plots_CI/allCI.pdf")

#png(paste0("C:/Users/Nk/Documents/Uni/MA/Plots_BS/allBS",pattern,".png"))
pdf(paste0("C:/Users/Nk/Documents/Uni/MA/Plots_BS/allBS",pattern,".pdf"))

pp <- layout(matrix(c(1,0,0,0,0,2,3,0,0,0, 4, 5, 6,0,0, 7, 8, 9, 10, 0, 11, 12, 13, 14, 15), 5, 5, byrow=T))
layout.show(pp)


# First line: RSF ---------------------------------------------------------
par(mar=c(0.5, 0.5, 0.5, 0.5))
par(oma=c(2.5,2.5,0,0))

plot(c(0,120), c(-0.04,0.04), yaxt='n', xaxt='n', frame.plot=F, type='n') 
text(60, -0.035, "RSF", cex=1.2)

# Second line: Ctree ------------------------------------------------------

#par(mar=c(0.5,2,0.5,0.5))

#png("C:/Users/Nk/Documents/Uni/MA/Plots_CI/cforestrsf.png")
plot(c(1:120), meancforestrsf, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="", xaxt='n')
lines(c(1:120), I(meancforestrsf-1.98*sdcforestrsf), lty=2)
lines(c(1:120), I(meancforestrsf+1.98*sdcforestrsf), lty=2)
abline(h=0, col="red")
box(lty = '1373', col = 'gray')

#dev.off()



plot(c(0,120), c(-0.04,0.04),  yaxt='n', xaxt='n', frame.plot=F, type='n')
text(60, -0.035, "Cforest", cex=1.2)
text(5, 0, "Cforest", srt=270, cex=1.2)
lines(x = c(0,60), y = c(-0.04,0), lty=3, col="grey")



# Third line: RPart -------------------------------------------------------

#par(mar=c(0.5,2,0.5,0.5))


#png("C:/Users/Nk/Documents/Uni/MA/Plots_CI/rpartrsf.png")
plot(c(1:120), meanrpartrsf, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="", xaxt='n')
lines(c(1:120), I(meanrpartrsf-1.98*sdrpartrsf), lty=2)
lines(c(1:120), I(meanrpartrsf+1.98*sdrpartrsf), lty=2)
abline(h=0, col="red")
box(lty = '1373', col = 'gray')
#dev.off()

#par(mar=c(0.5,0.5,0.5,0.5))

#png("C:/Users/Nk/Documents/Uni/MA/Plots_CI/rpartcforest.png")
plot(c(1:120), meanrpartcforest, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="", yaxt='n', xaxt='n')
lines(c(1:120), I(meanrpartcforest-1.98*sdrpartcforest), lty=2)
lines(c(1:120), I(meanrpartcforest+1.98*sdrpartcforest), lty=2)
abline(h=0, col="red")
box(lty = '1373', col = 'gray')
#dev.off()

plot(c(0,120), c(-0.04,0.04), text("Cforest"), yaxt='n', xaxt='n', frame.plot=F, type='n')
text(60, -0.035, "Rpart", cex=1.2)
text(5, 0, "Rpart", srt=270, cex=1.2)
lines(x = c(0,60), y = c(-0.04,0), lty=3, col="grey")



# Fourth line: ctree ------------------------------------------------------
#par(mar=c(0.5,2,0.5,0.5))


#png("C:/Users/Nk/Documents/Uni/MA/Plots_CI/ctreersf.png")
plot(c(1:120), meanctreersf, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="", xaxt='n')
lines(c(1:120), I(meanctreersf-1.98*sdctreersf), lty=2)
lines(c(1:120), I(meanctreersf+1.98*sdctreersf), lty=2)
abline(h=0, col="red")
box(lty = '1373', col = 'gray')
#dev.off()

#par(mar=c(0.5,0.5,0.5,0.5))


#png("C:/Users/Nk/Documents/Uni/MA/Plots_CI/ctreecforest.png")
plot(c(1:120), meanctreecforest, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="", yaxt='n', xaxt='n')
lines(c(1:120), I(meanctreecforest-1.98*sdctreecforest), lty=2)
lines(c(1:120), I(meanctreecforest+1.98*sdctreecforest), lty=2)
abline(h=0, col="red")
box(lty = '1373', col = 'gray')
#dev.off()



#png("C:/Users/Nk/Documents/Uni/MA/Plots_CI/ctreerpart.png")
plot(c(1:120), meanctreerpart, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="", yaxt='n', xaxt='n')
lines(c(1:120), I(meanctreerpart-1.98*sdctreerpart), lty=2)
lines(c(1:120), I(meanctreerpart+1.98*sdctreerpart), lty=2)
abline(h=0, col="red")
box(lty = '1373', col = 'gray')
#dev.off()

plot(c(0,120), c(-0.04,0.04),  yaxt='n', xaxt='n', frame.plot=F, type='n')
text(60, -0.035, "Ctree", cex=1.2)
text(5, 0, "Ctree", srt=270,  cex=1.2)
lines(x = c(0,60), y = c(-0.04,0), lty=3, col="grey")



# Last line: Cox ----------------------------------------------------------

#par(mar=c(2,2,0.5,0.5))


#png("C:/Users/Nk/Documents/Uni/MA/Plots_CI/coxrsf.png")
plot(c(1:120), meancoxrsf, ylim=c(-0.1, 0.05), type="l", lwd=2, xlab="", ylab="")
lines(c(1:120), I(meancoxrsf-1.98*sdcoxrsf), lty=2)
lines(c(1:120), I(meancoxrsf+1.98*sdcoxrsf), lty=2)
abline(h=0, col="red")
box(lty = '1373', col = 'gray')
#dev.off()


#par(mar=c(2,0.5,0.5,0.5))


#png("C:/Users/Nk/Documents/Uni/MA/Plots_CI/coxcforest.png")
plot(c(1:120), meancoxcforest, ylim=c(-0.1, 0.05), type="l", lwd=2, xlab="", ylab="", yaxt='n')
lines(c(1:120), I(meancoxcforest-1.98*sdcoxcforest), lty=2)
lines(c(1:120), I(meancoxcforest+1.98*sdcoxcforest), lty=2)
abline(h=0, col="red")
box(lty = '1373', col = 'gray')
#dev.off()

#png("C:/Users/Nk/Documents/Uni/MA/Plots_CI/coxrpart.png")
plot(c(1:120), meancoxrpart, ylim=c(-0.1, 0.05), type="l", lwd=2, xlab="", ylab="", yaxt='n')
lines(c(1:120), I(meancoxrpart-1.98*sdcoxrpart), lty=2)
lines(c(1:120), I(meancoxrpart+1.98*sdcoxrpart), lty=2)
abline(h=0, col="red")
box(lty = '1373', col = 'gray')
#dev.off()

#png("C:/Users/Nk/Documents/Uni/MA/Plots_CI/coxctree.png")
plot(c(1:120), meancoxctree, ylim=c(-0.1, 0.05), type="l", lwd=2, xlab="", ylab="", yaxt='n')
lines(c(1:120), I(meancoxctree-1.98*sdcoxctree), lty=2)
lines(c(1:120), I(meancoxctree+1.98*sdcoxctree), lty=2)
abline(h=0, col="red")
box(lty = '1373', col = 'gray')
#dev.off()

plot(c(0,120), c(-0.04,0.04), yaxt='n', xaxt='n', frame.plot=F, type='n')
#text(60, -0.03, "CoxPH")
text(5, 0, "CoxPH", srt=270, cex=1.2)

dev.off()

# Plot differences for CI -------------------------------------------------

# Build difference matrix -------------------------------------------------

rsfcforest<- CIrsf- CIcforest 
rsfctree<- CIrsf- CIctree
rsfrpart<- CIrsf- CIrpart
rsfcox<- CIrsf- CICox

coxrsf<- CICox - CIrsf
coxcforest<- CICox - CIcforest
coxctree<- CICox - CIctree
coxrpart<- CICox - CIrpart

cforestrsf<- CIcforest - CIrsf 
cforestctree<- CIcforest - CIctree
cforestrpart<- CIcforest - CIrpart
cforestcox<- CIcforest - CICox

rpartrsf<- CIrpart - CIrsf
rpartctree<- CIrpart - CIctree
rpartcforest<- CIrpart - CIcforest
rpartcox<- CIrpart - CICox

ctreerpart<- CIctree - CIrpart
ctreersf<- CIctree - CIrsf
ctreecforest<- CIctree - CIcforest
ctreecox<- CIctree - CICox



# Calculate means and sds -------------------------------------------------

meanrsfcforest<- apply(rsfcforest, 1, mean) 
meanrsfctree<- apply(rsfctree, 1, mean)
meanrsfrpart<- apply(rsfrpart, 1, mean)
meanrsfcox<- apply(rsfcox, 1, mean)

meancoxrsf<- apply(coxrsf, 1, mean)
meancoxcforest<- apply(coxcforest, 1, mean)
meancoxctree<- apply(coxctree, 1, mean)
meancoxrpart<- apply(coxrpart, 1, mean)

meancforestrsf<- apply(cforestrsf, 1, mean) 
meancforestctree<- apply(cforestctree, 1, mean)
meancforestrpart<- apply(cforestrpart, 1, mean)
meancforestcox<- apply(cforestcox, 1, mean)

meanrpartrsf<- apply(rpartrsf, 1, mean)
meanrpartctree<- apply(rpartctree, 1, mean)
meanrpartcforest<- apply(rpartcforest, 1, mean)
meanrpartcox<- apply(rpartcox, 1, mean)

meanctreerpart<- apply(ctreerpart, 1, mean)
meanctreersf<- apply(ctreersf, 1, mean)
meanctreecforest<- apply(ctreecforest, 1, mean)
meanctreecox<- apply(ctreecox, 1, mean)


# Calculate standard errors -----------------------------------------------

varrsf<- apply(CIrsf, 1, var)
varctree<- apply(CIctree, 1, var)

sdrsfctree1<- (varrsf +varctree)/100


sdrsfcforest<- apply(rsfcforest, 1, sd) 
sdrsfctree<- apply(rsfctree, 1, sd)
sdrsfrpart<- apply(rsfrpart, 1, sd)
sdrsfcox<- apply(rsfcox, 1, sd)

sdcoxrsf<- apply(coxrsf, 1, sd)
sdcoxcforest<- apply(coxcforest, 1, sd)
sdcoxctree<- apply(coxctree, 1, sd)
sdcoxrpart<- apply(coxrpart, 1, sd)

sdcforestrsf<- apply(cforestrsf, 1, sd) 
sdcforestctree<- apply(cforestctree, 1, sd)
sdcforestrpart<- apply(cforestrpart, 1, sd)
sdcforestcox<- apply(cforestcox, 1, sd)

sdrpartrsf<- apply(rpartrsf, 1, sd)
sdrpartctree<- apply(rpartctree, 1, sd)
sdrpartcforest<- apply(rpartcforest, 1, sd)
sdrpartcox<- apply(rpartcox, 1, sd)

sdctreerpart<- apply(ctreerpart, 1, sd)
sdctreersf<- apply(ctreersf, 1, sd)
sdctreecforest<- apply(ctreecforest, 1, sd)
sdctreecox<- apply(ctreecox, 1, sd)


# Plot differences --------------------------------------------------------
png("C:/Users/Nk/Documents/Uni/MA/Plots_CI/rsfcforest.png")
#par(mfrow=c(1,4))
plot(c(1:120), meanrsfcforest, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="")
lines(c(1:120), I(meanrsfcforest-1.98*sdrsfcforest), lty=2)
lines(c(1:120), I(meanrsfcforest+1.98*sdrsfcforest), lty=2)
abline(h=0, col="red")
dev.off()

png("C:/Users/Nk/Documents/Uni/MA/Plots_CI/rsfcox.png")
plot(c(1:120), meanrsfcox, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="")
lines(c(1:120), I(meanrsfcox-1.98*sdrsfcox), lty=2)
lines(c(1:120), I(meanrsfcox+1.98*sdrsfcox), lty=2)
abline(h=0, col="red")
dev.off()

png("C:/Users/Nk/Documents/Uni/MA/Plots_CI/rsfctree.png")
plot(c(1:120), meanrsfctree, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="")
lines(c(1:120), I(meanrsfctree-1.98*sdrsfctree), lty=2)
lines(c(1:120), I(meanrsfctree+1.98*sdrsfctree), lty=2)
abline(h=0, col="red")
dev.off()

png("C:/Users/Nk/Documents/Uni/MA/Plots_CI/rsfrpart.png")
plot(c(1:120), meanrsfrpart, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="")
lines(c(1:120), I(meanrsfrpart-1.98*sdrsfrpart), lty=2)
lines(c(1:120), I(meanrsfrpart+1.98*sdrsfrpart), lty=2)
abline(h=0, col="red")
dev.off()

#  ------------------------------------------------------------------------

png("C:/Users/Nk/Documents/Uni/MA/Plots_CI/cforestcox.png")
plot(c(1:120), meancforestcox, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="")
lines(c(1:120), I(meancforestcox-1.98*sdcforestcox), lty=2)
lines(c(1:120), I(meancforestcox+1.98*sdcforestcox), lty=2)
abline(h=0, col="red")
dev.off()


png("C:/Users/Nk/Documents/Uni/MA/Plots_CI/cforestrsf.png")
plot(c(1:120), meancforestrsf, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="")
lines(c(1:120), I(meancforestrsf-1.98*sdcforestrsf), lty=2)
lines(c(1:120), I(meancforestrsf+1.98*sdcforestrsf), lty=2)
abline(h=0, col="red")
dev.off()


png("C:/Users/Nk/Documents/Uni/MA/Plots_CI/cforestctree.png")
plot(c(1:120), meancforestctree, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="")
lines(c(1:120), I(meancforestctree-1.98*sdcforestctree), lty=2)
lines(c(1:120), I(meancforestctree+1.98*sdcforestctree), lty=2)
abline(h=0, col="red")
dev.off()


png("C:/Users/Nk/Documents/Uni/MA/Plots_CI/cforestrpart.png")
plot(c(1:120), meancforestrpart, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="")
lines(c(1:120), I(meancforestrpart-1.98*sdcforestrpart), lty=2)
lines(c(1:120), I(meancforestrpart+1.98*sdcforestrpart), lty=2)
abline(h=0, col="red")
dev.off()


#  ------------------------------------------------------------------------



png("C:/Users/Nk/Documents/Uni/MA/Plots_CI/coxrsf.png")
plot(c(1:120), meancoxrsf, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="")
lines(c(1:120), I(meancoxrsf-1.98*sdcoxrsf), lty=2)
lines(c(1:120), I(meancoxrsf+1.98*sdcoxrsf), lty=2)
abline(h=0, col="red")
dev.off()


png("C:/Users/Nk/Documents/Uni/MA/Plots_CI/coxctree.png")
plot(c(1:120), meancoxctree, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="")
lines(c(1:120), I(meancoxctree-1.98*sdcoxctree), lty=2)
lines(c(1:120), I(meancoxctree+1.98*sdcoxctree), lty=2)
abline(h=0, col="red")
dev.off()


png("C:/Users/Nk/Documents/Uni/MA/Plots_CI/coxcforest.png")
plot(c(1:120), meancoxcforest, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="")
lines(c(1:120), I(meancoxcforest-1.98*sdcoxcforest), lty=2)
lines(c(1:120), I(meancoxcforest+1.98*sdcoxcforest), lty=2)
abline(h=0, col="red")
dev.off()

png("C:/Users/Nk/Documents/Uni/MA/Plots_CI/coxrpart.png")
plot(c(1:120), meancoxrpart, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="")
lines(c(1:120), I(meancoxrpart-1.98*sdcoxrpart), lty=2)
lines(c(1:120), I(meancoxrpart+1.98*sdcoxrpart), lty=2)
abline(h=0, col="red")
dev.off()


#  ------------------------------------------------------------------------


png("C:/Users/Nk/Documents/Uni/MA/Plots_CI/rpartcox.png")
plot(c(1:120), meanrpartcox, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="")
lines(c(1:120), I(meanrpartcox-1.98*sdrpartcox), lty=2)
lines(c(1:120), I(meanrpartcox+1.98*sdrpartcox), lty=2)
abline(h=0, col="red")
dev.off()

png("C:/Users/Nk/Documents/Uni/MA/Plots_CI/rpartrsf.png")
plot(c(1:120), meanrpartrsf, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="")
lines(c(1:120), I(meanrpartrsf-1.98*sdrpartrsf), lty=2)
lines(c(1:120), I(meanrpartrsf+1.98*sdrpartrsf), lty=2)
abline(h=0, col="red")
dev.off()

png("C:/Users/Nk/Documents/Uni/MA/Plots_CI/rpartcforest.png")
plot(c(1:120), meanrpartcforest, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="")
lines(c(1:120), I(meanrpartcforest-1.98*sdrpartcforest), lty=2)
lines(c(1:120), I(meanrpartcforest+1.98*sdrpartcforest), lty=2)
abline(h=0, col="red")
dev.off()

png("C:/Users/Nk/Documents/Uni/MA/Plots_CI/rpartctree.png")
plot(c(1:120), meanrpartctree, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="")
lines(c(1:120), I(meanrpartctree-1.98*sdrpartctree), lty=2)
lines(c(1:120), I(meanrpartctree+1.98*sdrpartctree), lty=2)
abline(h=0, col="red")
dev.off()


#  ------------------------------------------------------------------------

png("C:/Users/Nk/Documents/Uni/MA/Plots_CI/ctreecox.png")
plot(c(1:120), meanctreecox, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="")
lines(c(1:120), I(meanctreecox-1.98*sdctreecox), lty=2)
lines(c(1:120), I(meanctreecox+1.98*sdctreecox), lty=2)
abline(h=0, col="red")
dev.off()

png("C:/Users/Nk/Documents/Uni/MA/Plots_CI/ctreecforest.png")
plot(c(1:120), meanctreecforest, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="")
lines(c(1:120), I(meanctreecforest-1.98*sdctreecforest), lty=2)
lines(c(1:120), I(meanctreecforest+1.98*sdctreecforest), lty=2)
abline(h=0, col="red")
dev.off()

png("C:/Users/Nk/Documents/Uni/MA/Plots_CI/ctreersf.png")
plot(c(1:120), meanctreersf, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="")
lines(c(1:120), I(meanctreersf-1.98*sdctreersf), lty=2)
lines(c(1:120), I(meanctreersf+1.98*sdctreersf), lty=2)
abline(h=0, col="red")
dev.off()

png("C:/Users/Nk/Documents/Uni/MA/Plots_CI/ctreerpart.png")
plot(c(1:120), meanctreerpart, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="")
lines(c(1:120), I(meanctreerpart-1.98*sdctreerpart), lty=2)
lines(c(1:120), I(meanctreerpart+1.98*sdctreerpart), lty=2)
abline(h=0, col="red")
dev.off()


# Experiment with layout --------------------------------------------------
dev.off()



#pdf("C:/Users/Nk/Documents/Uni/MA/Plots_CI/allCI.pdf")

#png(paste0("C:/Users/Nk/Documents/Uni/MA/Plots_CI/allCI",pattern,".png"))

pdf(paste0("C:/Users/Nk/Documents/Uni/MA/Plots_CI/allCI",pattern,".pdf"))

pp <- layout(matrix(c(1,0,0,0,0,2,3,0,0,0, 4, 5, 6,0,0, 7, 8, 9, 10, 0, 11, 12, 13, 14, 15), 5, 5, byrow=T))
layout.show(pp)


# First line: RSF ---------------------------------------------------------
par(mar=c(0.5, 0.5, 0.5, 0.5))
par(oma=c(2.5,2.5,0,0))

plot(c(0,120), c(-0.04,0.04), yaxt='n', xaxt='n', frame.plot=F, type='n') 
text(60, -0.03, "RSF", cex=1.2)


# Second line: Ctree ------------------------------------------------------

#par(mar=c(0.5,2,0.5,0.5))

#png("C:/Users/Nk/Documents/Uni/MA/Plots_CI/cforestrsf.png")
plot(c(1:120), meancforestrsf, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="", xaxt='n')
lines(c(1:120), I(meancforestrsf-1.98*sdcforestrsf), lty=2)
lines(c(1:120), I(meancforestrsf+1.98*sdcforestrsf), lty=2)
abline(h=0, col="red")
box(lty = '1373', col = 'gray')

#dev.off()



plot(c(0,120), c(-0.04,0.04),  yaxt='n', xaxt='n', frame.plot=F, type='n')
text(60, -0.03, "Cforest", cex=1.2)
text(5, 0, "Cforest", srt=270, cex=1.2)
lines(x = c(0,60), y = c(-0.04,0), lty=3, col="grey")



# Third line: RPart -------------------------------------------------------

#par(mar=c(0.5,2,0.5,0.5))


#png("C:/Users/Nk/Documents/Uni/MA/Plots_CI/rpartrsf.png")
plot(c(1:120), meanrpartrsf, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="", xaxt='n')
lines(c(1:120), I(meanrpartrsf-1.98*sdrpartrsf), lty=2)
lines(c(1:120), I(meanrpartrsf+1.98*sdrpartrsf), lty=2)
abline(h=0, col="red")
box(lty = '1373', col = 'gray')
#dev.off()

#par(mar=c(0.5,0.5,0.5,0.5))

#png("C:/Users/Nk/Documents/Uni/MA/Plots_CI/rpartcforest.png")
plot(c(1:120), meanrpartcforest, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="", yaxt='n', xaxt='n')
lines(c(1:120), I(meanrpartcforest-1.98*sdrpartcforest), lty=2)
lines(c(1:120), I(meanrpartcforest+1.98*sdrpartcforest), lty=2)
abline(h=0, col="red")
box(lty = '1373', col = 'gray')
#dev.off()

plot(c(0,120), c(-0.04,0.04), text("Cforest"), yaxt='n', xaxt='n', frame.plot=F, type='n')
text(60, -0.03, "Rpart", cex=1.2)
text(5, 0, "Rpart", srt=270, cex=1.2)
lines(x = c(0,60), y = c(-0.04,0), lty=3, col="grey")



# Fourth line: ctree ------------------------------------------------------
#par(mar=c(0.5,2,0.5,0.5))


#png("C:/Users/Nk/Documents/Uni/MA/Plots_CI/ctreersf.png")
plot(c(1:120), meanctreersf, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="", xaxt='n')
lines(c(1:120), I(meanctreersf-1.98*sdctreersf), lty=2)
lines(c(1:120), I(meanctreersf+1.98*sdctreersf), lty=2)
abline(h=0, col="red")
box(lty = '1373', col = 'gray')
#dev.off()

#par(mar=c(0.5,0.5,0.5,0.5))


#png("C:/Users/Nk/Documents/Uni/MA/Plots_CI/ctreecforest.png")
plot(c(1:120), meanctreecforest, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="", yaxt='n', xaxt='n')
lines(c(1:120), I(meanctreecforest-1.98*sdctreecforest), lty=2)
lines(c(1:120), I(meanctreecforest+1.98*sdctreecforest), lty=2)
abline(h=0, col="red")
box(lty = '1373', col = 'gray')
#dev.off()



#png("C:/Users/Nk/Documents/Uni/MA/Plots_CI/ctreerpart.png")
plot(c(1:120), meanctreerpart, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="", yaxt='n', xaxt='n')
lines(c(1:120), I(meanctreerpart-1.98*sdctreerpart), lty=2)
lines(c(1:120), I(meanctreerpart+1.98*sdctreerpart), lty=2)
abline(h=0, col="red")
box(lty = '1373', col = 'gray')
#dev.off()

plot(c(0,120), c(-0.04,0.04),  yaxt='n', xaxt='n', frame.plot=F, type='n')
text(60, -0.03, "Ctree")
text(5, 0, "Ctree", srt=270, cex=1.2)
lines(x = c(0,60), y = c(-0.04,0), lty=3, col="grey")




# Last line: Cox ----------------------------------------------------------

#par(mar=c(2,2,0.5,0.5))


#png("C:/Users/Nk/Documents/Uni/MA/Plots_CI/coxrsf.png")
plot(c(1:120), meancoxrsf, ylim=c(-0.1, 0.05), type="l", lwd=2, xlab="", ylab="")
lines(c(1:120), I(meancoxrsf-1.98*sdcoxrsf), lty=2)
lines(c(1:120), I(meancoxrsf+1.98*sdcoxrsf), lty=2)
abline(h=0, col="red")
box(lty = '1373', col = 'gray')
#dev.off()


#par(mar=c(2,0.5,0.5,0.5))


#png("C:/Users/Nk/Documents/Uni/MA/Plots_CI/coxcforest.png")
plot(c(1:120), meancoxcforest, ylim=c(-0.1, 0.05), type="l", lwd=2, xlab="", ylab="", yaxt='n')
lines(c(1:120), I(meancoxcforest-1.98*sdcoxcforest), lty=2)
lines(c(1:120), I(meancoxcforest+1.98*sdcoxcforest), lty=2)
abline(h=0, col="red")
box(lty = '1373', col = 'gray')
#dev.off()

#png("C:/Users/Nk/Documents/Uni/MA/Plots_CI/coxrpart.png")
plot(c(1:120), meancoxrpart, ylim=c(-0.1, 0.05), type="l", lwd=2, xlab="", ylab="", yaxt='n')
lines(c(1:120), I(meancoxrpart-1.98*sdcoxrpart), lty=2)
lines(c(1:120), I(meancoxrpart+1.98*sdcoxrpart), lty=2)
abline(h=0, col="red")
box(lty = '1373', col = 'gray')
#dev.off()

#png("C:/Users/Nk/Documents/Uni/MA/Plots_CI/coxctree.png")
plot(c(1:120), meancoxctree, ylim=c(-0.1, 0.05), type="l", lwd=2, xlab="", ylab="", yaxt='n')
lines(c(1:120), I(meancoxctree-1.98*sdcoxctree), lty=2)
lines(c(1:120), I(meancoxctree+1.98*sdcoxctree), lty=2)
abline(h=0, col="red")
box(lty = '1373', col = 'gray')
#dev.off()

plot(c(0,120), c(-0.04,0.04), yaxt='n', xaxt='n', frame.plot=F, type='n')
text(5, 0, "CoxPH", srt=270, cex=1.2)

dev.off()





# Experiment with legends -------------------------------------------------

