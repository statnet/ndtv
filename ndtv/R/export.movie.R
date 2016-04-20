#  File R/export.movie.R in package ndtv, part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 2012-2016 Statnet Commons
#######################################################################
#functions to generate and export movies
#apply a series of network layouts to a networkDynamic object
#store the coordinates as temporal attributes on the network
compute.animation <- function(net, slice.par=NULL, animation.mode="kamadakawai", seed.coords=NULL, layout.par=list(),default.dist=NULL, weight.attr=NULL,weight.dist=FALSE, chain.direction=c('forward','reverse'), verbose=TRUE,...){
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
  
  chain.direction<-match.arg(chain.direction)
  
  
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
    if(is.null(slice.par$rule)){
      stop("the 'slice.par' argument to compute.animation must include a 'rule' value")
    }
  }
  #if that doesn't work, guess
  if (is.null(slice.par)){
    slice.par <- guessSlicePar(net)
    if(verbose){
      message('No slice.par found, using')
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
  if (chain.direction=='reverse'){
    sliceIndices <-length(starts):1
  } else {  # assume we will be doing it forward
    sliceIndices <-1:length(starts)
  }
  for ( s in sliceIndices){
    #debug, print out the coordinte range
    xrange <-c(min(coords[,1]),max(coords[,1]))
    yrange <-c(min(coords[,2]),max(coords[,2]))
    
    if(verbose){
      message(paste("Calculating layout for network slice from time ",starts[s],"to",ends[s]))
    }
    #only compute the layout involving active nodes and edges
    activev <- is.active(net,starts[s],ends[s], rule=if(slice.par$rule!='all'){'any'},v=seq_len(network.size(net)))
    slice <- network.collapse(net,starts[s],ends[s], rule=slice.par$rule,rm.time.info=is.null(weight.attr))
    

    if (length(slice) > 0 & network.size(slice)>0){
      #only update those coords that were calced
      newCoords <-coords[activev,,drop=FALSE] # maybe this assignment necessary to force a copy before passing to C?
      # if a weight attribute is defined, precompute a dist.matrix
      dist.mat=NULL
      if (!is.null(weight.attr)){
        dist.mat<-layout.distance(slice,default.dist=default.dist,weight.attr=weight.attr,weight.dist=weight.dist)
      }
      newCoords  <- layout.fun(slice,dist.mat=dist.mat, default.dist=default.dist, seed.coords=newCoords,layout.par=layout.par,verbose=verbose)
      # recenter the new coords
      newCoords<-center.fun(newCoords,xlim,ylim)
      coords[activev,] <- newCoords
      net <- activate.vertex.attribute(net,prefix="animation.x",onset=starts[s],terminus=ends[s],value=newCoords[,1],v=which(activev))
      net <- activate.vertex.attribute(net,prefix="animation.y",onset=starts[s],terminus=ends[s],value=newCoords[,2],v=which(activev))
    } 
      # TODO: should we still store coords at the same position as previous frame?
      
    
    
  }
  if(exists(xn, envir=ev))
    on.exit(assign(xn, net, pos=ev))
  return(invisible(net))
}

#go through the sets of coordinates attached to the network
#compute interpolation frames, and actually draw it out
#optionally save it directly to a file
render.animation <- function(net, render.par=list(tween.frames=10,show.time=TRUE,show.stats=NULL,extraPlotCmds=NULL,initial.coords=0),plot.par=list(bg='white'),ani.options=list(interval=0.1),render.cache=c('plot.list','none'), verbose=TRUE,...){
  if (!is.network(net)){
    stop("render.animation requires the first argument to be a network object")
  }
  
  
  # check render.par params
  if (is.null(render.par)){
    stop("render.animation is missing the 'render.par' argument (a list of rendering parameters).")
  }
  if (is.null(render.par$tween.frames)){
    render.par$tween.frames<-10 
  }
  if (is.null(render.par$show.time)){
    render.par$show.time<-TRUE
  }
  if (is.null(render.par$initial.coords)){
    render.par$initial.coords<-matrix(0,ncol=2,nrow=network.size(net))
  }
  
  #check if coordinates have already been computed
  if (!all(c("animation.x.active","animation.y.active") %in% list.vertex.attributes(net))){
    net <- compute.animation(net,verbose=verbose)
  }
  
  
  # temporary hard-coded param to work around plot issue in RStudio
  externalDevice<-FALSE
  doRStudioHack<-TRUE
  if(!is.null(render.par$'do_RStudio_plot_hack')){
    doRStudioHack<-render.par$'do_RStudio_plot_hack'
  }
  if (!is.function(options()$device)){
    if (names(dev.cur())=="RStudioGD" & doRStudioHack){
      message("RStudio's graphics device is not well supported by ndtv, attempting to open another type of plot window")
      # check if user allready specified something with R_DEFAULT_DEVICE
      defaultDev<-Sys.getenv('R_DEFAULT_DEVICE')
      if (identical(defaultDev,'')){
        # try to open a new platform-appropriate plot window
        if (.Platform$OS.type=='windows'){
          #windows()
          defaultDev<-'windows'
        } else if(length(grep(R.version$platform,pattern='apple'))>0)  # is it mac?
        {
          #quartz()
          defaultDev= 'quartz'
        } else {  # must be unix
          #x11()
          defaultDev='x11'
        }
        Sys.setenv(R_DEFAULT_DEVICE=defaultDev)
      }
      dev.new(noRStudioGD = TRUE)
      externalDevice<-TRUE
      # don't forget to set the R_DEFAULT_DEVICE back to '' or R CMD Check will balk
      Sys.setenv(R_DEFAULT_DEVICE='')
    }
  }
  
  # make sure background color is not transparent unless set that way explicitly
  if (par("bg")=="transparent" & is.null(plot.par$'bg')){
    plot.par$'bg'<-'white'
  }
  # set high-level plot attributes (bg color, margins, etc)
  # and cache initial graphics par settings
  origPar<-par(plot.par) 
  
  # set animation options
  oopts <- ani.options(ani.options)
  
  #figure out what the slicing parameters were
  slice.par <- get.network.attribute(net,"slice.par")
  if (is.null(slice.par)){
    stop("render.animation can not locate the 'slice.par' list of parameters in the input network object")
  }
  
  
  # check plot caching params
  render.cache<-match.arg(render.cache)
  
  
  # cache plotting arguments 
  plot_params<-list(...)
  
  # define some defaults for ploting args
  # label defaults to vertex names
  if(is.null(plot_params$label)){
    plot_params$label<-function(slice){network.vertex.names(slice)}
  }
  # xlab defaults to time
  if(is.null(plot_params$xlab) & render.par$show.time){
    plot_params$xlab <- function(onset,terminus){ifelse(onset==terminus,paste("t=",onset,sep=''),paste("t=",onset,"-",terminus,sep=''))}
  }
  # but if show stats, use that instead 
  # TODO: deprecate show.stats in favor of passing in directly for evaluation?
  if(!is.null(render.par$show.stats) && render.par$show.stats!=FALSE){
    # evaluate a eqn string giving the stats formual
    # TODO: this requires that tergm be loaded! give informative warning if not
    if(render.par$show.time){
      # include the time string in the summary
      plot_params$xlab <- eval(parse(text=paste("function(slice,onset,terminus){stats<-ergm::summary.statistics.network(slice",render.par$show.stats,")\n return(paste('t=',onset,'-',terminus,' ',paste(rbind(names(stats),stats),collapse=':'),sep='')) }",sep='')))
    } else {
      plot_params$xlab <- eval(parse(text=paste("function(slice){stats<-ergm::summary.statistics.network(slice",render.par$show.stats,")\n return(paste(rbind(names(stats),stats),collapse=':')) }",sep='')))
    }
  }
  
  #disable jitter by default because it messes things up
  if(is.null(plot_params$jitter)){
    plot_params$jitter<-FALSE
  }
  
  #TODO: how are we doing interpolation?
  interp.fun<-coord.interp.smoothstep
  #interp.fun<-coord.interp.linear
  
  # compute lists of times that networks will be collapsed
  starts <- seq(from=slice.par$start,to=slice.par$end,by=slice.par$interval)
  ends <- seq(from=slice.par$start+slice.par$aggregate.dur,to=slice.par$end+slice.par$aggregate.dur,by=slice.par$interval)

  #compute coordinate ranges to know how to scale plots
  xmin <- min(aggregate.vertex.attribute.active(net,"animation.x",min),na.rm=TRUE)
  xmax <- max(aggregate.vertex.attribute.active(net,"animation.x",max),na.rm=TRUE)
  ymin <- min(aggregate.vertex.attribute.active(net,"animation.y",min),na.rm=TRUE)
  ymax <- max(aggregate.vertex.attribute.active(net,"animation.y",max),na.rm=TRUE)
  if (is.null(plot_params$xlim)){
    # deal with Inf or NA
    if(is.na(xmin) | is.infinite(xmin)){
      xmin<--1
    } 
    if(is.na(xmax) | is.infinite(xmax)){
      xmax<-1
    }
    # deal with case of only one coord, so no range
    if(xmin==xmax){
      
      xmax<-xmin+1
      xmin<-xmin-1
    }
    plot_params$xlim<-c(xmin,xmax)
  }
  if(is.null(plot_params$ylim)){
    # deal with Inf or NA
    if(is.na(ymin) | is.infinite(ymin)){
      ymin<--1
    } 
    if(is.na(ymax) | is.infinite(ymax)){
      ymax<-1
    }
    # deal with case of only one coord, so no range
    if(ymin==ymax){
      ymax<-ymin+1
      ymin<-ymin-1
    }
    plot_params$ylim<-c(ymin,ymax)
  }
  
  #set up default coords.  If not specified, default will be zero
  if(is.numeric(render.par$initial.coords)){
    coords<-matrix(render.par$initial.coords,ncol=2,nrow=network.size(net))
  }
  
  #compute some starting coords  
  slice <- network.collapse(net,starts[1],ends[1],rule=slice.par$rule,rm.time.info=FALSE) 
  activev <- is.active(net,starts[1],ends[1],v=seq_len(network.size(net)),rule=if(slice.par$rule!='all'){'any'})
  
  # start from the coords of the first slice
  if (length(slice)>0 & network.size(slice)>0){ 
    coords[activev,1] <-get.vertex.attribute(slice,"animation.x")
    coords[activev,2] <-get.vertex.attribute(slice,"animation.y")
    #need to update plot params with slice-specific values
  }# end slice > 0 block
    
  coords2 <- coords
  
  if (render.cache=='plot.list'){
    ani.record(reset=TRUE)
  }
  #move through frames to render them out
  for(s in 1:length(starts)){
    if (verbose){message(paste("rendering",render.par$tween.frames,"frames for slice",s-1))}
    slice <- network.collapse(net,starts[s],ends[s],rule=slice.par$rule,rm.time.info=FALSE)
    activev <- is.active(net,starts[s],ends[s],v=seq_len(network.size(net)),rule=if(slice.par$rule!='all'){'any'})
   
    #TODO: draw new slices for intermediate tween frames?
    #skip any empty networks
    if (length(slice)>0 & network.size(slice)>0){
      #need to update plot params with slice-specific values
      evald_params<-.evaluate_plot_params(plot_params=plot_params,net=net,slice=slice,s=s,onset=starts[s],terminus=ends[s])
      
   
      for(t in 1:render.par$tween.frames){
        #coords2[activev,1]<-get.vertex.attribute.active(slice,"animation.x",onset=starts[s],terminus=ends[s])
        #coords2[activev,2]<-get.vertex.attribute.active(slice,"animation.y",onset=starts[s],terminus=ends[s])
        coords2[activev,1]<-get.vertex.attribute(slice,"animation.x")
        coords2[activev,2]<-get.vertex.attribute(slice,"animation.y")
       # tweenCoords <- coords + ((coords2-coords)*(t/render.par$tween.frames))
        tweenCoords <- interp.fun(coords,coords2,t,render.par$tween.frames)
        
         #TODO:what if we want to include innactive nodes
        # set up arguments
        plot_args<-list(x=slice,coord=tweenCoords[activev,,drop=FALSE])
        plot_args<-c(plot_args,evald_params)
        # cll the plotting function with appropriate args
        do.call(plot.network, plot_args) 
        # check if user has passed in extra plotting commands that need to be rendered
        if (!is.null(render.par$extraPlotCmds)){
          eval(render.par$extraPlotCmds)
        }
        if (render.cache=='plot.list'){
          ani.record()
        }
      }
      coords<-coords2;
    } else { # end slice > 0 block
      # empty network causes plot problems
      # draw some blank frames while time passes
      evald_params<-.evaluate_plot_params(plot_params=plot_params,net=net,slice=slice,s=s,onset=starts[s],terminus=ends[s])
      if(render.par$show.time){
        xlab<-evald_params$xlab
      } else {
        xlab<-NULL
      }

      singlenet <-network.initialize(1)
      for(t in 1:render.par$tween.frames){
        plot.network(singlenet,
                   vertex.cex=0,xlab=xlab)
        # check if user has passed in extra plotting commands that need to be rendered
        if (!is.null(render.par$extraPlotCmds)){
          eval(render.par$extraPlotCmds)
        }
        if (render.cache=='plot.list'){
          ani.record()
        }
      }
    } # end empty network block
  }
  
  # reset the graphics params back
  par(origPar)
  # turn off external device if using one
  if (externalDevice){
    dev.off()
  }
  
}


# function to evaluate graphics parameters defined as functions
# net is the original (uncollapsed) network
# slice is collapsed network
# s is the slice number
# onset is the start time of the slice
# terminus is the end time of the slice
# functions will be evaluated in the parent frame
.evaluate_plot_params <- function(plot_params,net,slice,s,onset,terminus){
  if (length(plot_params)<1){
    return(plot_params)
  }
  # figure out which plot params are functions
  fun_params<-which(sapply(plot_params,is.function))
  # loop over all the functinons..
  for (fun_index in fun_params){
    # get the names of the funtions arguments
    argnames<-names(as.list(args(plot_params[[fun_index]])))
    argnames <- argnames[-length(argnames)] # trim off last element
    # construct an argument list by mapping of values to function params
    args<-list()
    for (arg in argnames){
      if (arg=='net'){
        args<-c(args,list(net=net))
      } else if (arg=='slice'){
        args<-c(args,list(slice=slice))
      } else if (arg=='s'){
        args<-c(args,list(s=s))
      } else if (arg=='onset'){
        args<-c(args,list(onset=onset))
      } else if (arg=='terminus'){
        args<-c(args,list(terminus=terminus))
      } else {
        #stop('unknown argument name "',arg,'" in function provided for ',names(fun_params)[[fun_index]],' graphic parameter:',deparse(plot_params[[fun_index]]))
        # copy in the original argument corresponding to the argument name
        args[[arg]]<-as.list(args(plot_params[[fun_index]]))[[arg]]
      }
    }
    # replace the function on the list with the results of its evaluation
    val<-do.call(plot_params[[fun_index]],args=args)
    # make sure we dont' clobber the list element by setting it to NULL
    if(!is.null(val)){
      plot_params[[fun_index]]<-val
    } else {
      # we can't leave it as a function, so what do we set it to?
      plot_params[[fun_index]]<-numeric(0)
    }
  }
  # return the modified list of plot params
  return(plot_params)
}
    
#common function called to construct the distance matrix for mds-based layouts
layout.distance <-function(net,default.dist=NULL,weight.attr=NULL,weight.dist=FALSE){
  
  if (is.null(default.dist)){
    default.dist=sqrt(network.size(net))
  } else {
    if(!is.numeric(default.dist) | length(default.dist)>1){
      stop('default.dist must be a numeric value of length 1')
    }
  }
  # if there are no edges, don't worry about the edge value
  if (network.edgecount(net)<1){
    weight.attr=NULL
  }
  raw<-as.matrix.network.adjacency(net,attrname=weight.attr,expand.bipartite=TRUE)
  # if the attribute is similairity (bigger=closer, need to invert values)
  if(!is.null(weight.attr) & !weight.dist){
    matmax<-max(raw)
    matmin<-min(raw[raw>0])
    raw[]<-vapply(raw,function(x){ifelse(x>0,(matmax-x)+matmin,0)},numeric(1))
  }
  # if it is a distance matrix, symmatrize with pmin (shortest path), if it is similarity matrix, symmatrize with pmax, (most similar)
  if (is.directed(net)){
    if (weight.dist){
      # replace any 0 values with their transpose (to avoid zeroing the min)
      raw[raw==0]<-t(raw)[raw==0]
      # now take min  
      raw<-pmin(raw,t(raw))
    } else {
      raw<-pmax(raw,t(raw))
    }
  }
  dg <- geodist(raw,inf.replace=default.dist,ignore.eval=is.null(weight.attr))$gdist
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
    slice.par<-list(start=0,end=0,interval=1, aggregate.dur=1,rule="latest")
    return(slice.par)
  }
  # ignore inf values
  times[times==Inf]<-NA
  times[times==-Inf]<-NA
  
  # TODO: should try to guess if it is discrete or cont
  # TODO: should try to pick no more than 100 samples
  slice.par<-list(start=min(times,na.rm=T),end=max(times,na.rm=T),interval=1, aggregate.dur=1,rule="latest")
  return(slice.par)
}


# helper function to pretty print slice.par object
.print.slice.par <-function(sp){
  cat("slice parameters:\n")
  cat(paste("  start:",sp$start,'\n  end:',sp$end,'\n  interval:',sp$interval,'\n  aggregate.dur:',sp$aggregate.dur,'\n  rule:',sp$rule,"\n\n",sep=''))
}


