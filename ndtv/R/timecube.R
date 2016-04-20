#  File R/timecube.R in package ndtv, part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 2016 Statnet Commons
#######################################################################

#data("short.stergm.sim")
#compute.animation(short.stergm.sim,mode='MDSJ')
#ndtv:::timePrism(short.stergm.sim,at=c(1,10,20),orientation=c('z','y','x'),displaylabels=TRUE,label.cex=0.5,spline.v=c(3,4,12),scale.y=0.5)

# extract (and possibly compute) network animation coordinates as 3d array
timeCoords<-function(nd,at){
  #check if coordinates have already been computed
  if (!all(c("animation.x.active","animation.y.active") %in% list.vertex.attributes(nd))){
      stop('network does not have animation coordinates, run compute.animation() first')
  }
  
  # TODO:  if at is missing, default to slice.par?
  
  # extract the coords it generated for each slice
  xy<-lapply(seq_len(length(at)),function(t){
    cbind(x=get.vertex.attribute.active(nd,'animation.x',at=at[t]),
          y=get.vertex.attribute.active(nd,'animation.y',at=at[t]))
  })
  # smoosh into 3D coord array
  xy<-do.call(rbind,xy)
  xyz<-cbind(xy,z=unlist(lapply(at,rep,network.size(nd))))
  return(xyz)
}

timePrism<-function(nd,at,
                    spline.v=NULL,
                    spline.col='#55555555',
                    spline.lwd=1,
                    box=TRUE,
                    axis=TRUE,
                    planes=FALSE,
                    plane.col='#FFFFFF99',
                    scale.y=1,
                    angle=20,
                    orientation=c('x','y','z'),
                    ...){
  # TODO compute coords for each slice if missing
  
  requireNamespace('scatterplot3d')

  
  netSize<-network.size(nd)
  #check if coordinates have already been computed
  if (!all(c("animation.x.active","animation.y.active") %in% list.vertex.attributes(nd))){
    if(!missing(at)){
      stop('network does not have animation coordinates, run compute.animation() first')
    } else {
      # if nothing is specified, try dividing time range into beginning, middle, end
      frames<-3
      message("No coordinate information found in network, running compute.animation for ",frames, " time points")
      times <- get.change.times(nd)
      slice.par = list(start = times[1], end = times[length(times)], 
                       interval = (times[length(times)] - times[1])/(frames - 
                                                                       1), aggregate.dur = 0, rule = "latest")
      nd <- compute.animation(nd, slice.par = slice.par)
      at<-seq(from=slice.par$start,to=slice.par$end,by = slice.par$interval)
    }
  }
  
  # TODO:  if at is missing, default to slice.par?
  # extract the coords it generated for each slice
  xyz<-timeCoords(nd,at)
  
  # compute some padding for if we plot the planes
  # assume x and y must have similar range
  plane.pad=(max(xyz[,'x'])-min(xyz[,'x']))*0.025
  
  # swap axes if desired
  xyz<-xyz[,orientation,drop=FALSE]
  
  #figure where ticks will be shown
  x.ticklabs=NULL
  y.ticklabs=NULL
  z.ticklabs=NULL
  if(orientation[1]!='z') x.ticklabs=NA
  if(orientation[2]!='z') y.ticklabs=NA
  if(orientation[3]!='z') z.ticklabs=NA

  # set up the 3d plotting space with axes, etc
  stuff3d<-scatterplot3d::scatterplot3d(xyz,
                         xlab=ifelse(orientation[1]=='z','time',''),
                         ylab=ifelse(orientation[2]=='z','time',''),
                         zlab=ifelse(orientation[3]=='z','time',''),
                         x.ticklabs=x.ticklabs,
                         y.ticklabs=y.ticklabs,
                         z.ticklabs=z.ticklabs,
                         grid=F,
                         box=box,
                         pch=NA,
                         scale.y=scale.y,
                         angle = angle,
                         axis=axis)
  
  # optionally plot x-spline linking selected vertices in v
  if (!is.null(spline.v)){
    # do coordinate projection
    xyproj<-stuff3d$xyz.convert(xyz)
    spline.col<-rep_len(spline.col,length(spline.v))
    for (vindex in seq_len(length(spline.v))){
      xspline(xyproj$x[seq(from=spline.v[vindex],to=length(xyproj$x),by=netSize)],
              xyproj$y[seq(from=spline.v[vindex],to=length(xyproj$y),by=netSize)],
              shape = -0.2,
              lty=2,
              lwd=spline.lwd,
              border=spline.col[vindex])
    }
  }
  
  # plot the momentary networks and their ties
  for (t in at){
    net<-network.collapse(nd,at=t)
      coords<-cbind(x=net%v%'animation.x',y=net%v%'animation.y',z=rep(t,netSize))
      # swap axes if desired
      coords<-coords[,orientation,drop=FALSE]
      if(planes){
      # plot a plane to keep things seperated
      planeCoords<-cbind(x=c(min(xyz[,'x'])-plane.pad,min(xyz[,'x'])-plane.pad,max(xyz[,'x'])+plane.pad,max(xyz[,'x'])+plane.pad),
                         y=c(min(xyz[,'y'])-plane.pad,max(xyz[,'y'])+plane.pad,max(xyz[,'y'])+plane.pad,min(xyz[,'y'])-plane.pad),
                         z=rep(t,4))
      planeCoords<-planeCoords[,orientation,drop=FALSE]
      polygon(stuff3d$xyz.convert(planeCoords),col = plane.col,border='gray')
    }
    # project into the 3d space
    coordsProj<-stuff3d$xyz.convert(coords)
    # plot the network using projected coordiantes
    plot.network(net,coord=cbind(coordsProj$x,coordsProj$y),
                 new=FALSE,jitter=FALSE,...)  
    
    
  }
  invisible(stuff3d)
}


