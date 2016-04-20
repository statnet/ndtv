#  part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 2012-2016 Statnet Commons
#######################################################################

# function for creating small multiple images of frames of a nD animation

filmstrip <- function(nd, frames=9, slice.par,render.par,mfrow,verbose=FALSE,...){
  if (!is.networkDynamic(nd)){
    stop("filmstrip plots require a networkDynamic object as the first argument")
  }
  
  #TODO: allow specifying frames as vector of timepoints insteadof a number of frames
  oldSlicePar<-NULL
  # check if coordinates have already been computed
  if (!all(c("animation.x.active","animation.y.active") %in% list.vertex.attributes(nd))){
    message("No coordinate information found in network, running compute.animation")
    # figure out the times at which they should be computed
    times <-get.change.times(nd)
    if(missing(slice.par)){
      slice.par=list(start=times[1],end=times[length(times)],interval=(times[length(times)]-times[1])/(frames-1), aggregate.dur=0,rule='latest')
    }
    nd<-compute.animation(nd,slice.par=slice.par,verbose=verbose)
  } else {
    # frames should override slice par unless directl specified
    
    if(missing(slice.par)){
      # figure out times that plots should be done for
      # ideally  want to select time points corresponding to existing slices
      # but slice.par doesn't give us enough flexibility
      slice.par<-nd%n%'slice.par'
      oldSlicePar<-slice.par
      slice.par$interval=(slice.par$end-slice.par$start)/(frames-1)
      nd%n%'slice.par'<-slice.par
    } else {
      message('frames argument has been overridden by slice.par')
    }
  }
  
  

  # set up default render par if missing
  if(missing(render.par)){
    render.par<-list(tween.frames = 1, show.time = TRUE, 
           show.stats = NULL, extraPlotCmds=NULL)
  } else {
      render.par$tween.frames<-1;
  }
  #pass in a secret parameter to tell render.animation not to do the RStudio plot work-around
  render.par$'do_RStudio_plot_hack'<-FALSE
  
  # figure out grid dimensions
  
  if(missing(mfrow)){
    # TODO: should be able to find a nearly-square rect that gets close to right value
    mfrow<-c(ceiling(sqrt(frames)),ceiling(sqrt(frames)))
  }
  # cach the mfrow value so we can restore it
  oldmfrow<-par()$mfrow
  # render the frames out
  render.animation(nd,render.par=render.par,render.cache='none',plot.par=list(bg='white',mfrow=mfrow),verbose=verbose,...)
  par(mfrow=oldmfrow)
  
  # restore the state of slice.par
  nd%n%'slice.par'<-oldSlicePar
  
}