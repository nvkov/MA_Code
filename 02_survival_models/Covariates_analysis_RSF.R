# Plot Brier score and C-index:

rm(list=ls())

setwd("C:/Users/Nk/Documents/Uni/MA/varimp")
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
load("varimp35.RData")
mean<- sort(apply(varimp,1,mean), decreasing = F)
mean<- apply(varimp,1,mean)

max<- apply(varimp,1,max)
min<- apply(varimp,1,min)

barp<- cbind(mean, max, min)
barp<- barp[order(barp[,1], decreasing = T),]

library(plotrix)

barp(barp[,1], col = "grey70", names.arg = rownames(barp), width=0.15, staxx=T)
points(barp[,2], col="red", pch=19)
points(barp[,3], col="red", pch=19)

