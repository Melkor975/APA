set.seed(222)

##################################################
# VICTOR VALLEJO RIVES - GRUP 11 - EXERCICI 7 L2 #
##################################################





##############
# LLIBRERIES #
##############
library(cclust)
library(MASS)
library(ggplot2)
library(mlbench)
library(Rmixmod)
library(Rcpp)
library(RColorBrewer)


#Funcio que aplica K-means i retorna l'Index de Calinski-Harabasz
do.Index <- function(k) {
  kmeans <- cclust (data.1$x,k,iter.max=100,method="kmeans",dist="euclidean")
  r <- clustIndex(kmeans,data.1$x, index="calinski")
  return (r)
  
}

##########################
# GENERACIO DE LES DADES #
##########################
n <- 1000
k <- 6
sigma2 <- 0.6^2

data.1 <- mlbench.2dnormals (n,k,sd=sqrt(sigma2))

#######################
# DADES SENSE TRACTAR #
#######################
plot(x=data.1$x[,1], y=data.1$x[,2],xlab = "x",ylab = "y")

##################################
# DADES COLOREJADES PELS 6 GRUPS #
##################################
plot(data.1)

#####################
# K-MEANS AMB K = 6 #
#####################
k <- 6
par(mfrow = c(2, 5))
for(i in 1:10){
  kmeans.6 <- cclust (data.1$x,k,iter.max=100,method="kmeans",dist="euclidean")
  
  plot(data.1$x[,1],data.1$x[,2],
       #main = "k-means",
       #sub = paste0("K = ", kmeans.6$ncenters),
       sub = paste0("num.",i),
       xlab = "",
       ylab="",
       
       col =(kmeans.6$cluster+1))
  points(kmeans.6$centers,col="black",cex=2,pch=19)
}

#Ultima execucio per veure-ho en gran
par(mfrow = c(1, 1))
plot(data.1$x[,1],data.1$x[,2],
     main = "k-means",
     sub = paste0("K = ", kmeans.6$ncenters),
     xlab = "",
     ylab="",
     col =(kmeans.6$cluster+1))
points(kmeans.6$centers,col="black",cex=2,pch=19)


#################################
# Index Calinski-Harabasz mitja #
#################################


diferents_k <- 2:10
vector_indexCH <- array(0,dim = (length(diferents_k)))

for (i in diferents_k) {
  vector_indexCH[i-1] <- mean(replicate(20, do.Index(i)))
  print(paste0("k ",i, " CH ", vector_indexCH[i-1]))
}

plot(diferents_k,vector_indexCH,
     type="b",
     main = "Índex CH per a diferents k",
     xlab = "k",
     ylab = " Calinski-Harabasz mitjà",
     ylim=c(0,max(vector_indexCH)) )

#################
# E-M amb k = 6 #
#################



fammodel <- mixmodGaussianModel (family="diagonal", equal.proportions=FALSE)

z <- mixmodCluster (data.frame(data.1$x),models = fammodel, nbCluster = 6)

summary(z)
(z@bestResult@parameters@variance)
#(z@bestResult@likelihood)
#z@bestResult@proba
#z@bestResult@proba[1:5,]
(means <- z@bestResult@parameters@mean)

found.clusters <- z@bestResult@partition

plot(data.1$x[,1],
     data.1$x[,2],
     xlab="x",
     ylab="y",
     main = "E-M amb k = 6",
     col=(found.clusters+1))
points(means,col="black",cex=2,pch=19)



#################################
# GRAFICA DENSITAT DE LES DADES #
#################################

densitat <- kde2d(data.1$x[,1], data.1$x[,2], n=50)
colors <- rev(brewer.pal(11, "RdYlBu"))
plot(data.1$x, xlab="x", ylab="y", pch=19, cex=.4)


contour(densitat, drawlabels=FALSE, nlevels=22, col=colors, add=TRUE)
abline(h=mean(data.1$x[, 2]), v=mean(data.1$x[, 1]), lwd=2)
