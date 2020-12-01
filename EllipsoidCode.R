rm(list=ls())

library(cluster)
library(rgl)

#Load Kinematic Data for Fine Motor Phase: Data is in List format by Participant DSesNP1L

#Calculate ellipsoid hull for dwell time among each Participant
el.cov=array(NA,c(19,3,3))
el.loc=matrix(NA,nrow=19,ncol = 3)
DwellEllipse=matrix(NA,nrow=19,ncol = 9)
for(i in 1:19){
  #Calculate ellipsoidhull for each participant
  el.out=ellipsoidhull(as.matrix(DwellKin[[i]][1:3]))
  #Calculate semi-major axes
  ele=sqrt(el.out$d2)*(sqrt(eigen(el.out$cov)$values))
  #Covariance matrix of ellipsoid
  el.cov[i,,]=el.out$cov
  #p-dimensional location of ellipsoid center
  el.loc[i,]=el.out$loc
  #Store semi-major axes in data frame
  DwellEllipse[i,]=c(ele)
}

#Calculate average covariance and center location
Dwell.sigma=colMeans(el.cov,dims=1)
Dwell.center=colMeans(el.loc)

#Plot average
plot3d( ellipse3d(Dwell.sigma, centre = Old.center), col = "green", alpha = 0.5)


DwellSemiM=data.frame(Dmean=DwellMean,
                         elx=DwellEllipse[,1],
                         ely=DwellEllipse[,2],
                         elz=DwellEllipse[,3])

#Calculate ellipsoid volume
EllVolume=(4/3)*pi*DwellSemiM$elx*DwellSemiM$ely*DwellSemiM$elz

