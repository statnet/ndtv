#  File R/layout_centering.R in package ndtv, part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 2012-2016 Statnet Commons
#######################################################################
# functions to center layouts

# helper function for centering values within range without rescaling
layout.center <- function(coords, xlim,ylim){
  # find max an min of coords
  xrange<-range(coords[,1])
  yrange<-range(coords[,2])
  if(missing(xlim)){
    xlim<-xrange
  } 
  xlimrange<-xlim[2]-xlim[1]
  
  if(missing(ylim)){
    ylim<-yrange
  } 
  ylimrange<-ylim[2]-ylim[1]
  xoffset<-((xlimrange-(xrange[2]-xrange[1]))/2)
  yoffset<-((ylimrange-(yrange[2]-yrange[1]))/2)
  coords[,1]<-(coords[,1]-xrange[1])+(xlim[1]+xoffset)
  coords[,2]<-(coords[,2]-yrange[1])+(ylim[1]+yoffset)
  return(coords)
}


# helper function to normalize coords in range (-1,1)
layout.normalize <-function(coords,keep.aspect.ratio=TRUE){
  # translate
  coords[,1]<-coords[,1]-min(coords[,1],na.rm = TRUE)
  coords[,2]<-coords[,2]-min(coords[,2],na.rm = TRUE)
  # normalize to 1.0
  if (keep.aspect.ratio){
    coords<-coords/(max(coords,na.rm = TRUE)/2)
  } else {
    coords[,1]<-coords[,1]/(max(coords[,1],na.rm = TRUE)/2)
    coords[,2]<-coords[,2]/(max(coords[,2],na.rm = TRUE)/2)
  }
  # shift to center
  coords<-coords-1
  return(coords)
}

# add barycenter function to focus on 'center of gravity' of layout


# add function to center on specific vertex

# function to zoom in on a region of a layout
# accepts a scale factor and center, OR a vector of vertices that should be used to calculate a center
layout.zoomfactor <-function(coords, factor, center=c(0,0),v=NULL){
  
  if (factor==0){
    return(coords)
  }
  if (!is.null(v)){
    center[1]<-mean(coords[v,1])
    center[2]<-mean(coords[v,2])
  }
  # translate coordinates to center
  coords[,1]<-coords[,1]-center[1]
  coords[,2]<-coords[,2]-center[2]
  
  # scale by zoom factor
  coords<-coords*factor
  
  # translate back
  coords[,1]<-coords[,1]+center[1]
  coords[,2]<-coords[,2]+center[2]
  return(coords)
}

