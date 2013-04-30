#  File R/export.movie.R in package ndtv, part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 2012-2013 Statnet Commons
#######################################################################
#functions to generate and export movies
#require(sna)
require(networkDynamic)
require(animation)
#apply a series of network layouts to a networkDynamic object
#store the coordinates as temporal attributes on the network
compute.animation <- function(net, slice.par=NULL, animation.mode="kamadakawai", seed.coords=NULL, layout.par=list(),default.dist=NULL, verbose=TRUE,...){
  #check that we are dealing with the right types of objects
  if (!is.networkDynamic(net)){
    stop("The 'net' argument to compute.animation must be a networkDynamic object")
  }
   
  #figure out what layouts we will be using
  layout.fun <- try(match.fun(paste("network.layout.animate.", animation.mode, sep = "")), silent = TRUE)
  if (class(layout.fun) == "try-error"){
      stop(paste("Error in compute.animation: no network animation layout function for animation.mode ", animation.mode))
  }
  
  # figure out what recentering we will be using
  centering.mode='center' # haven't added argument yet
  center.fun <- try(match.fun(paste("layout.", centering.mode, sep = "")), silent = TRUE)
  if (class(layout.fun) == "try-error"){
    stop(paste("Error in compute.animation: no layout centering function for centering.mode ", centering.mode))
  }
  # figure out the range we will be centering in
  xlim<-c(0,0)
  ylim<-c(0,0)
  
  
  # some stuff so we can modify argument
  xn <- deparse(substitute(net))
  ev <- parent.frame()
  
  #try load from network if it is missing
  if(is.null(slice.par)){
    slice.par <- get.network.attribute(net,"slice.par")
  }
  if(!is.null(slice.par)) {
    # make sure it has appropriate parts
    if(is.null(slice.par$start)){
      stop("the 'slice.par' argument to compute.animation must include a 'start' value")
    }
    if(is.null(slice.par$end)){
      stop("the 'slice.par' argument to compute.animation must include an 'end' value")
    }
    if(is.null(slice.par$interval)){
      stop("the 'slice.par' argument to compute.animation must include an 'interval' value")
    }
    if(is.null(slice.par$aggregate.dur)){
      stop("the 'slice.par' argument to compute.animation must include an 'aggregate.dur' value")
    }
  }
  #if that doesn't work, guess
  if (is.null(slice.par)){
    slice.par <- guessSlicePar(net)
    if(verbose){
      print('No slice.par found, using')
      .print.slice.par(slice.par)
    }
  }
  
  #store the slice.par on network for later use
  set.network.attribute(net,"slice.par", slice.par)
  
  #compute the set of start and end times for each slice
  #TODO: allow alternate "per change specification" or passing in a vector of times
  starts <- seq(from=slice.par$start,to=slice.par$end,by=slice.par$interval)
  ends <- seq(from=slice.par$start+slice.par$aggregate.dur,to=slice.par$end+slice.par$aggregate.dur,by=slice.par$interval)
  #TODO need more initial coord options
  if (is.null(seed.coords)){
    seed.coords <- matrix(data=runif(network.size(net)*2) , ncol=2)
  }
  
  # recenter the seed coords
  seed.coords<-center.fun(seed.coords,xlim=xlim,ylim=ylim)
    
  #delete any existing coords attached to the network
  delete.vertex.attribute(net,"animation.x.active")
  delete.vertex.attribute(net,"animation.y.active")
  
  coords <-seed.coords
  
  #extract crossections and apply layouts, must be in sequence to allow chaining
  for ( s in 1:length(starts)){
    #debug, print out the coordinte range
    xrange <-c(min(coords[,1]),max(coords[,1]))
    yrange <-c(min(coords[,2]),max(coords[,2]))
    
    if(verbose){
      print(paste("Calculating layout for network slice from time ",starts[s],"to",ends[s]))
    }
    #only compute the layout involving active nodes and edges
    activev <- is.active(net,starts[s],ends[s], rule=slice.par$rule,v=seq_len(network.size(net)))
    slice <- network.collapse(net,starts[s],ends[s], rule=slice.par$rule)
    

    if (length(slice) > 0 & network.size(slice)>0){
      #only update those coords that were calced
      newCoords <-coords[activev,] # maybe this assignment necessary to force a copy before passing to C?
      newCoords  <- layout.fun(slice,dist.mat=NULL, default.dist=default.dist, seed.coords=newCoords,layout.par=layout.par,verbose=verbose)
      # recenter the new coords
      newCoords<-center.fun(newCoords,xlim,ylim)
      coords[activev,] <- newCoords
      net <- activate.vertex.attribute(net,prefix="animation.x",onset=starts[s],terminus=ends[s],value=newCoords[,1],v=which(activev))
      net <- activate.vertex.attribute(net,prefix="animation.y",onset=starts[s],terminus=ends[s],value=newCoords[,2],v=which(activev))
    }
    
  }
  if(exists(xn, envir=ev))
    on.exit(assign(xn, net, pos=ev))
  return(invisible(net))
}

#go through the sets of coordinates attached to the network
#compute interpolation frames, and actually draw it out
#optionally save it directly to a file
render.animation <- function(net, render.par=list(tween.frames=10,show.time=TRUE,show.stats=NULL,extraPlotCmds=NULL),verbose=TRUE,label,displaylabels=!missing(label),xlab,...){
  if (!is.network(net)){
    stop("render.animation requires the first argument to be a network object")
  }
  
  #check if coordinates have already been computed
  if (!all(c("animation.x.active","animation.y.active") %in% list.vertex.attributes(net))){
    net <- compute.animation(net,verbose=verbose)
  }
  
  # temporary hard-coded param to work around plot issue in rstudio
  externalDevice<-FALSE
  if (!is.function(options()$device)){
    if (options("device")$device=="RStudioGD"){
      print("RStudio's graphics device is not well supported by ndtv, attempting to open another type of plot window")
      # try to open a new platform-appropriate plot window
      if (.Platform$OS.type=='windows'){
        windows()
      } else if(length(grep(R.version$platform,pattern='apple'))>0)  # is it mac?
      {
        quartz()
      } else {  # must be unix
        x11()
      }
      externalDevice<-TRUE
    }
  }
  
  #figure out what the slicing parameters were
  slice.par <- get.network.attribute(net,"slice.par")
  if (is.null(slice.par)){
    stop("render.animation can not locate the 'slice.par' list of parameters in the input network object")
  }
  
  # check render.par params
  if (is.null(render.par)){
    stop("render.animaion is missing the 'render.par' argument (a list of rendering parameters).")
  }
  if (is.null(render.par$tween.frames)){
    render.par$tween.frames<-10 
  }
  if (is.null(render.par$show.time)){
    render.par$show.time<-TRUE
  }
  
  #check graphics params
  if(missing(xlab)){
    xlab=NULL
  }
  
  #TODO: how are we doing interpolation?
  interp.fun<-coord.interp.smoothstep
  #interp.fun<-coord.interp.linear
  
  starts <- seq(from=slice.par$start,to=slice.par$end,by=slice.par$interval)
  ends <- seq(from=slice.par$start+slice.par$aggregate.dur,to=slice.par$end+slice.par$aggregate.dur,by=slice.par$interval)
  
  #TODO: check that there are at least two frames
  
  #print some summary info as a starting frame?
  #compute some starting coords  
  slice <- network.collapse(net,starts[1],ends[1]) 
  activev <- is.active(net,starts[1],ends[1],v=seq_len(network.size(net)))
  
  
  
  #compute coordinate ranges to know how to scale plots
  xmin <- aggregate.vertex.attribute.active(net,"animation.x",min)
  xmax <- aggregate.vertex.attribute.active(net,"animation.x",max)
  ymin <- aggregate.vertex.attribute.active(net,"animation.y",min)
  ymax <- aggregate.vertex.attribute.active(net,"animation.y",max)
  if (!exists('xlim')){
    xlim<-c(xmin,xmax)
  }
  if(!exists('ylim')){
    ylim<-c(ymin,ymax)
  }
  
  
  #if the first slice is empty, just start from zeros
  # TODO: start from initial coords?
  coords<-matrix(0,ncol=2,nrow=network.size(net))
  if (length(slice)>0 & network.size(slice)>0){ 
    #coords[activev,1] <-get.vertex.attribute.active(slice,"animation.x",onset=starts[1],terminus=ends[1])
    #coords[activev,2] <-get.vertex.attribute.active(slice,"animation.y",onset=starts[1],terminus=ends[1])
    coords[activev,1] <-get.vertex.attribute(slice,"animation.x")
    coords[activev,2] <-get.vertex.attribute(slice,"animation.y")
    #need to update plot params with slice-specific values
    if(missing(label)){
      slice.label <- network.vertex.names(slice)
    } else {
      slice.label <- label
    }
    if(render.par$show.time){
      xlab <- paste("t=",starts[1],"-",ends[1],sep='')
    }
    if(!is.null(render.par$show.stats) && render.par$show.stats!=FALSE){
      # evaluate a eqn string giving the stats formual
      stats <- eval(parse(text=paste("summary.statistics.network(network.extract(net,onset=",starts[1],", terminus=",ends[1],")",render.par$show.stats,")",sep=''))) 
      xlab <- paste(xlab,paste(rbind(names(stats),stats),collapse=":"))
    }
    
    
    plot.network(slice,coord=coords[activev,],
                 label=slice.label,displaylabels=(!missing(label) | displaylabels),xlim=xlim,ylim=ylim,xlab=xlab,jitter=FALSE,...)
    # check if user has passed in extra plotting commands that need to be rendered
    if (!is.null(render.par$extraPlotCmds)){
      eval(render.par$extraPlotCmds)
    }
  }# end slice > 0 block
    
  coords2 <- coords
  oopts <- ani.options(interval = 0.1,ani.type="jpeg",ani.dev="jpeg")
  #oopts <- ani.options(interval = 0.1,...)
  ani.record(reset=TRUE)
  #move through frames to render them out
  for(s in 1:length(starts)){
    if (verbose){print(paste("rendering",render.par$tween.frames,"frames for slice",s-1))}
    slice <- network.collapse(net,starts[s],ends[s])
    activev <- is.active(net,starts[s],ends[s],v=seq_len(network.size(net)))
   
    #TODO: draw new slices for intermediate tween frames?
    #skip any empty networks
    if (length(slice)>0 & network.size(slice)>0){
      if(missing(label)){
        slice.label <- network.vertex.names(slice)
      } else {
        slice.label <-label
      }
      
      #show the time on the plot
      if(render.par$show.time){
        xlab <- paste("t=",starts[s],"-",ends[s],sep='')
      }
      
      #show stats as title of the plot
      if(!is.null(render.par$show.stats) && render.par$show.stats!=FALSE){
        # evaluate a eqn string giving the stats formual
        stats <- eval(parse(text=paste("summary.statistics.network(network.extract(net,onset=",starts[s],", terminus=",ends[s],")",render.par$show.stats,") ",sep='')))
        xlab <- paste(xlab,paste(rbind(names(stats),stats),collapse=":"))
      }
   
      for(t in 1:render.par$tween.frames){
        #coords2[activev,1]<-get.vertex.attribute.active(slice,"animation.x",onset=starts[s],terminus=ends[s])
        #coords2[activev,2]<-get.vertex.attribute.active(slice,"animation.y",onset=starts[s],terminus=ends[s])
        coords2[activev,1]<-get.vertex.attribute(slice,"animation.x")
        coords2[activev,2]<-get.vertex.attribute(slice,"animation.y")
       # tweenCoords <- coords + ((coords2-coords)*(t/render.par$tween.frames))
        tweenCoords <- interp.fun(coords,coords2,t,render.par$tween.frames)
         #TODO:what if we want to include innactive nodes
        plot.network(slice,coord=tweenCoords[activev,],
                 label=slice.label,displaylabels=(!missing(label) | displaylabels),xlim=xlim,ylim=ylim,xlab=xlab,jitter=FALSE,...) 
        # check if user has passed in extra plotting commands that need to be rendered
        if (!is.null(render.par$extraPlotCmds)){
          eval(render.par$extraPlotCmds)
        }
        ani.record();
      }
      coords<-coords2;
    } else { # end slice > 0 block
      # draw some blank frames while time passes
      if(render.par$show.time){
        xlab <- paste("t=",starts[s],"-",ends[s],sep='')
      }

      singlenet <-network.initialize(1)
      for(t in 1:render.par$tween.frames){
        plot.network(singlenet,
                   vertex.cex=0,xlim=xlim,ylim=ylim,xlab=xlab)
        # check if user has passed in extra plotting commands that need to be rendered
        if (!is.null(render.par$extraPlotCmds)){
          eval(render.par$extraPlotCmds)
        }
        ani.record();
      }
    } # end empty network block
  }
  
  # turn off external device if using one
  if (externalDevice){
    dev.off()
  }
  
}
    
#common function called to construct the distance matrix for mds-based layouts
layout.distance <-function(net,default.dist=NULL,weight.attr=NULL,weight.dist=FALSE){
  if (is.null(default.dist)){
    default.dist=sqrt(network.size(net))
  }
  raw<-as.matrix.network.adjacency(net,attrname=weight.attr,expand.bipartite=TRUE)
  # if the attribute is similairity (bigger=closer, need to invert values)
  if(!is.null(weight.attr) & !weight.dist){
    matmax<-max(raw)
    matmin<-min(raw[raw>0])
    raw[]<-vapply(raw,function(x){ifelse(x>0,(matmax-x)+matmin,0)},numeric(1))
  }
  #TODO: give option to use pmin?  is there a fast version of pmean?
  dg <- geodist(pmax(raw,t(raw)),inf.replace=default.dist,ignore.eval=is.null(weight.attr))$gdist
  return(dg)
}

#function to generate a symetrix matrix using an arbirary funciton SLOW
matrix.symmetrize <- function(mat,fun){
  for (i in seq_len(nrow(mat))){
    j<- i+1;
    while(j <=ncol(mat)){
      value <- fun(c(mat[i,j],mat[j,i]))
      mat[i,j] <- value
      mat[j,i] <- value
      j <- j+1;
    }
  }  
  return(mat)
}

guessSlicePar <- function(nd){
  times <- get.change.times(nd)
  if (length(times)==0){
    warning("network does not appear to have any dynamic information. Using start=0 end=1")
    slice.par<-list(start=0,end=0,interval=1, aggregate.dur=1,rule="any")
    return(slice.par)
  }
  # ignore inf values
  times[times==Inf]<-NA
  times[times==-Inf]<-NA
  
  # TODO: should try to guess if it is discrete or cont
  # TODO: should try to pick no more than 100 samples
  slice.par<-list(start=min(times,na.rm=T),end=max(times,na.rm=T),interval=1, aggregate.dur=1,rule="any")
  return(slice.par)
}

# Rough draft of a primitive internal command for plotting labels on edges
.plotEdgeLabel<-function(net,coords, eid,label,...){
  # figure out the coords from eid
  v1<-net$mel[[eid]]$outl
  v2<-net$mel[[eid]]$inl
  v1coords<-coords[v1,]
  v2coords<-coords[v2,]
  textCoords<-v1coords+((v2coords-v1coords)/2)
  # assumes that line is straight
  text(textCoords[1],textCoords[2],labels=label,...)
}

.plotEdgeLabels <-function(net,coords,attrname,...){
  # get the values from the net
  vals <- get.edge.value(net,attrname)
  # if there are any edges / values
  if (length(vals) > 0){
    # get head ant tail vertex ids
    v1<-sapply(net$mel,'[[','outl')
    v2<-sapply(net$mel,'[[','inl')
    # get coords of those vertices
    v1coords<-coords[v1,,drop=FALSE]
    v2coords<-coords[v2,,drop=FALSE]
    # compute a point inbetween vertices (won't be on edge if edge is curved)
    textCoords<-v1coords+((v2coords-v1coords)/2)
    # draw the labels
    text(textCoords[,1],textCoords[,2],labels=vals,...)
  }
}

.plotActiveEdgeLabels <-function(net,at,eids=seq_len(network.edgecount(net)),attrname,...){
  # get the values from the net
  vals <- get.edge.value.active(net,attrname,at=at)
  x<-get.vertex.attribute.active(net,'animation.x',onset=at,terminus=at)
  y<-get.vertex.attribute.active(net,'animation.y',onset=at,terminus=at) 
  coords<-cbind(x,y)
  for (e in eids){
    .plotEdgeLabel(net,coords,e,label=vals[e],...)
  }
}

# helper function to pretty print slice.par object
.print.slice.par <-function(sp){
  cat("slice parameters:\n")
  cat(paste("  start:",sp$start,'\n  end:',sp$end,'\n  interval:',sp$interval,'\n  aggregate.dur:',sp$aggregate.dur,'\n  rule:',sp$rule,"\n\n",sep=''))
}


