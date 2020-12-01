#CODE USED TO CALCULATE ELLIPSOID HULL AND ELLIPSOID VOLUME OF INDIVIDUAL PARTICIPANT DATA

rm(list=ls())

library(cluster)
library(rgl)

#Load Kinematic Data for Fine Motor Phase: Data is in List format by Participant 3DKin
load(3DKin.Rda)


#Calculate ellipsoid hull for dwell time among each Participant

#Number of subjects
SubNum=
#Number of Ellipsoid Dimensions
EllDim=3
#pre-allocate memory
el.cov=array(NA,c(SubNum,EllDim,EllDim))
el.loc=matrix(NA,nrow=Subnum,ncol = EllDim)
DwellEllipse=matrix(NA,nrow=Subnum,ncol = EllDim)

for(i in 1:Subnum){
  #Calculate ellipsoidhull for each participant, assumes data is 3 dimensional
  el.out=ellipsoidhull(as.matrix(3DKin[[i]][1:3]))
  #Calculate semi-major axes
  ele=sqrt(el.out$d2)*(sqrt(eigen(el.out$cov)$values))
  #Covariance matrix of ellipsoid
  el.cov[i,,]=el.out$cov
  #p-dimensional location of ellipsoid center
  el.loc[i,]=el.out$loc
  #Store semi-major axes in data frame
  DwellEllipse[i,]=c(ele)
}

#Calculate average covariance and center location of group
Dwell.sigma=colMeans(el.cov,dims=1)
Dwell.center=colMeans(el.loc)

#Plot average 3D ellipsoid
plot3d( ellipse3d(Dwell.sigma, centre = Dwell.center), col = "blue", alpha = 0.5)


DwellSemiM=data.frame(Dmean=DwellMean,
                         elx=DwellEllipse[,1],
                         ely=DwellEllipse[,2],
                         elz=DwellEllipse[,3])

#Calculate ellipsoid volume
EllVolume=(4/3)*pi*DwellSemiM$elx*DwellSemiM$ely*DwellSemiM$elz

