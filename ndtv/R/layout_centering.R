
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

# add barycenter function to focus on 'center of gravity' of layout


# add function to center on specific vertex