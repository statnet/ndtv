#  File R/proximity.timeline.R in package ndtv, part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 2012-2016 Statnet Commons
#######################################################################

#TODO: when the network contains isolates, the values for default.dist tend to dominate the network, provide option to not include isolates
#TODO:allow for missing networks and inactive vertices
#TODO: add ability to specifiy values for labels, colors for labels, etc.
#TODO: add ability to specify attributes for weights

proximity.timeline<-function(nd,start = NULL, end = NULL, time.increment = NULL, 
                             onsets=NULL, termini=NULL, rule='earliest', default.dist=NULL,
                             vertex.col='#55555555',label=network.vertex.names(nd),
                             labels.at=NULL,label.cex=1,vertex.cex=2,
                             splines=-.2,
                             render.edges=FALSE,
                             grid=!render.edges,
                             edge.col='#00000055',
                             edge.lwd=4,
                             mode=c('isoMDS','sammon','cmdscale','gvNeato','MDSJ'),
                             coords=NULL,
                             draw.inactive=NULL, spline.style=c('default','inactive.ghost','inactive.gaps','inactive.ignore','color.attribute'),
                             chain.direction=c('forward','reverse'),
                             verbose=TRUE,...){
  if (!is.networkDynamic(nd)) {
    stop("proximity.timeline requires that the first argument be a networkDynamic object")
  }
  mode<-match.arg(mode)
  if(!is.null(draw.inactive)){
    stop("the 'draw.inactive' argument has been deprecated. Use 'spline.style' instead.")
  }
  spline.style<-match.arg(spline.style)
  chain.direction<-match.arg(chain.direction)
  
  if (!is.null(onsets) && (!is.vector(onsets) || !is.numeric(onsets))) 
    stop("Onset times must be a numeric vector \n")
  if (!is.null(termini) && (!is.vector(termini) || !is.numeric(termini))) 
    stop("termini must be a numeric vector \n")
  if (!is.null(time.increment) && !is.numeric(time.increment)) 
    stop("time.increment must be a non-negative numeric value\n")
  net.obs.period <- nd %n% "net.obs.period"
  if (!is.null(net.obs.period) & is.null(onsets) & is.null(termini)) {
    if (is.null(start)) {
      start <- min(unlist(net.obs.period$observations))
    }
    if (is.null(end)) {
      end <- max(unlist(net.obs.period$observations))
    }
    if (is.null(time.increment) && is.numeric(net.obs.period$time.increment)) {
      time.increment <- net.obs.period$time.increment
    }
  }
  if (is.null(time.increment)) {
    time.increment <- 1
  }
  if (!is.null(start) & !is.null(end) & !is.null(time.increment)) {
    if (!is.null(onsets) | !is.null(termini)) {
      stop("onsets & termini cannot be specified with start & end arguments\n")
    }
    if (is.infinite(start) | is.infinite(end)) {
      stop("start and end values must be finite")
    }
    onsets <- seq(from = start, to = end - time.increment, 
                  by = time.increment)
    termini <- seq(from = start + time.increment, to = end, 
                   by = time.increment)
  }
  else if (!is.null(onsets) & !is.null(termini)) {
    if (length(onsets) != length(termini)) {
      stop("onsets and termini must have the same number of elements")
    }
    if (any(onsets > termini)) {
      stop("Onset times must precede terminus times\n")
    }
  }
  else {
    # guess using observed change times
    onsets<-get.change.times(nd)
    termini<-onsets
  }
  if (verbose){
    message('collapsing slice networks ...')
  }
  slices<-get.networks(nd,retain.all.vertices=TRUE,onsets=onsets,termini=termini,rule=rule,...)
  
  # determine which direction the slices should be evaluated
  if (chain.direction=='reverse'){
    computeSequence<-seq.int(from=length(slices),to=1)
  }else{
    computeSequence<-seq_along(slices)
  }
  
  # check if passing in coordinates
  if (!is.null(coords)){
    if(!inherits(coords,'matrix') | !is.numeric(coords)){
      stop("the 'coords' argument must be a numeric matrix with appropriate dimensions")
    }
    # check appropriate dimensions
    if(nrow(coords)!=network.size(nd)){
      stop("number of rows (",nrow(coords),") in 'coords' cooordinate matrix is not equal to network size")
    }
    if(ncol(coords)!=length(slices)){
      stop("number of columns (",ncol(coords),") in 'coords' cooordinate matrix is not equal to number of time bins (",length(slices),")")
    }
    if(verbose){
      message("using passed coordinate matrix ...")
    }
    # copy the coords and repeat last column so that splines will end correctly
    ycoords<-cbind(coords,coords[,ncol(coords),drop=FALSE])
  } else {  # coordinates not passed in, so compute them
    ycoords<-matrix(0,nrow=network.size(nd),ncol=length(slices)+1)
  
    # set up initial starting coords
    # have to jitter to make sure no coords are the same
    prev_ycoord<-matrix(jitter(cmdscale(layout.distance(slices[[1]],default.dist=default.dist),k=1)),ncol=1)
    if (verbose){
      message('computing vertex positions using 1D ',mode, ' layout ...')
    }

    for (s in computeSequence){
      if (verbose){
        message('  computing positions for slice ',s)
      }
      slice<-slices[[s]]
      onset<-onsets[s]
      terminus<-termini[s]
      if(network.size(slice)>0 & network.edgecount(slice)>0){
        mat<-layout.distance(slice,default.dist=default.dist)
        # use the appropriate function as specified in mode
        ycoords[,s]<-switch(mode,
          sammon=MASS::sammon(mat,y=prev_ycoord,k=1,trace=FALSE,tol=1e-9)$points,
          isoMDS=MASS::isoMDS(mat,y=prev_ycoord,k=1,maxit=500,tol=1e-9,trace=FALSE)$points,
          #smacofSym=smacof::smacofSym(mat,ndim=1,init=prev_ycoord,metric=TRUE)$conf,
          cmdscale=cmdscale(mat,k=1),
          gvNeato=network.layout.animate.Graphviz(net = slice,dist.mat = mat,seed.coords = cbind(0,prev_ycoord),layout.par = list(gv.engine='neato',gv.args='-Gdim=2',gv.len.mode='ndtv.distance.matrix'))[,2],
          MDSJ=network.layout.animate.MDSJ(net=slice,dist.mat = mat,seed.coords = prev_ycoord,layout.par=list(dimensions=1),verbose=verbose)
        )       
        prev_ycoord<-ycoords[,s,drop=FALSE]
      } else {
        # I would like to leave out this segment, or draw it dotted
        # but for now, just copy previous values
        if (s>1){
          ycoords[,s]<-ycoords[,s-1]
        }
      }
    } # end slice setup loop
    #copy the last coord to bound the last spline
    ycoords[,ncol(ycoords)]<-ycoords[,ncol(ycoords)-1]
  }
  # check if there are any empty vertex spells
  hasGaps<-any(sapply(computeSequence,function(s){
    network.size.active(nd,onset=onsets[s],terminus=termini[s])<network.size(nd)
  }))
  

  
  #---- BEGIN RENDERING PROCESS ----
  
  # create a new plot
  plot(NA,NA,xlim=c(onsets[1],termini[length(termini)]),ylim=range(ycoords,na.rm=TRUE),xlab='time',ylab=paste('approx. distance among vertices of',substitute(nd)))
  
  
  # ---- VERTEX RENDERING -----
  
  # adjust spline style if mode was default and gaps found
  if (spline.style=='default' & hasGaps){
    spline.style<-'inactive.ghost'
  }
  
  # label processing
  label<-plotArgs.network(nd,argName = 'label',argValue=label)
  label.cex<-plotArgs.network(nd,argName='label.cex',argValue=label.cex)
  
  # expand vertex.cex (if it is an  attribute name, use that, otherwise replicate)
  vertex.cex<-plotArgs.network(nd,argName = 'vertex.cex',argValue=vertex.cex)
  
  # determine if vertex colors will be dynamic or not
  vattrnames<-list.vertex.attributes(nd)
  if (is.character(vertex.col) & (length(vertex.col) == 1)){ # TODO: what if the network only has one vertex?!!!
      # is it an attribute name?
     # if it is dynamic the name won't match the .active version, so check later
      if (vertex.col%in%vattrnames) { 
        # static colors
        vertex.col<-plotArgs.network(nd,argName='vertex.col',argValue=vertex.col)
      } else if( paste(vertex.col,'active',sep='.')%in%vattrnames) {
        # dynamic clors but non function
        # if we are not doing dynamic colors spline mode, this will be a problem
        if (spline.style!='color.attribute' & !is.color(vertex.col)){
          stop("Dynamic color attributes can only be used with spline.style='color.attribute'")
        }
        # we will be doing dynamic colors, so deal with later
        if (verbose){
          message('assuming dynamic colors')
        }
      } else { # assume it is (or should be) a vector of colors and replicate to appropriate length
        vertex.col<-rep(as.color(vertex.col),length=network.size(nd))
      }
  } else if (is.function(vertex.col)){
    # if we are coloring segments ignore it, because will expand it later
    if (spline.style!='color.attribute'){
       stop("setting the vertex.col as a function only works when coloring spline segments by attributes")
      }
  } else { # color is not an attribute, so just replicate the color strings for vertices
    vertex.col<-rep(as.color(vertex.col),length=network.size(nd))
  }
  
  
  if (verbose){
    message('rendering splines for each vertex ...')
  }
  # loop over vertices, drawing splines
  for (v in seq_len(nrow(ycoords))){
    
    if (spline.style=='inactive.ghost'){
      # 'ghost' in light background line to connect active spells
      xspline(c(onsets,termini[length(termini)]),ycoords[v,],shape=splines,border='#55555555',lwd=1,repEnds=TRUE,lty=3)
    }  
    
    if (spline.style%in%c('inactive.ghost','inactive.gaps','color.attribute')){ # should we render as continuous splines or in vertex activity chunks
      actives<-list() # to hold determine the activespells we will actually render
      segmentColors<-character(0)
      
      if (spline.style=='color.attribute'){
        # we will use vertex attribute activity chunks to determine breaks instead of vertex activity
        # for now, hard-coded to color
        
        # grab the color spells at each slice
        sliceColors<-sapply(seq_along(slices), function(s){
          getSegmentColor(nd,slices[[s]],v,s,onset=onsets[s],termini[s],vertex.col=vertex.col)
        })
        # figure out contiguous sets of values
        changes<-which(c(sliceColors,Inf) != c(Inf, sliceColors))
        actives<-lapply(seq_len(length(changes)-1),function(index){
          c(changes[index],changes[index+1]-1)
        })
        segmentColors<-sliceColors[changes]
        
        
      } else {  # all other segment drawing modes
        
        # render spells in segments to avoid slices where vertex is inactive
        # figure out contiguous set of slices
        
        lastactive<- 0
        startSlice<-NA
        endSlice<-NA
        for (s in seq_along(slices)){
          if(is.active(nd,v=v,onset=onsets[s],terminus=termini[s])){
            
            if(s-lastactive >= 1 & is.na(startSlice)){
              # record the value of the start
              startSlice<-s
            } 
            endSlice<-s
            lastactive<-s
            # if its the last slice, record the range
            if(s==length(slices)){
              actives[[length(actives)+1]]<-c(startSlice,endSlice)
            }
          } else { # v not active
            if(lastactive>0 && s-lastactive == 1){
              # record a spell
              actives[[length(actives)+1]]<-c(startSlice,endSlice)
              startSlice<-NA
              startSlice<-NA
            }
          }
        }
        
        
      } # end segment finding
      
      # now render the chunks
      for(r in seq_len(length(actives))){
        sliceIndices<-seq(from=actives[[r]][1],to=actives[[r]][2])
        # if onset==terminus, can't draw a spline because not enough control points
        duration <-termini[sliceIndices[1]]-onsets[sliceIndices[1]]
        
        # determine the color for the vertex segment to be drawn if it hasn't been set
        if( length(segmentColors)==0){
          vCol<-getSegmentColor(nd,slice=slices[[sliceIndices[1]]],v,s=r,onset=onsets[sliceIndices[1]],termini[max(sliceIndices)],vertex.col=vertex.col)
        } else {
          vCol<-segmentColors[r]
        }
          
        xControlP<-onsets[sliceIndices]
        yControlP<-ycoords[v,sliceIndices]
        
        # smoothness controlled by shape param, mapped from splines
        # =0 straight segments
        # >0 approx control points
        # <0 pass through control points
        # because we will draw the spline with repEnds=FALSE, we need to add aditional 
        # control points at each end (but the spline will now be drawn to them)
        if(r==1){
          # replicate the first value 
          xControlP<-c(xControlP[1],xControlP)
          yControlP<-c(yControlP[1],yControlP)
          #TODO: if inf value, should extend off chart by -duration?
        } else { # this isn't the first spell
          # use add the coords one step previous as control pointxControlP<-c(xControlP[1],xControlP)
          xControlP<-c(onsets[sliceIndices[1]-1],xControlP)
          yControlP<-c(ycoords[v,sliceIndices[1]-1],yControlP)
        }
        if(r==length(actives)){
          # extend one more unit with the same values
          xControlP<-c(xControlP,onsets[max(sliceIndices)]+duration,onsets[max(sliceIndices)]+duration*2)
          yControlP<-c(yControlP,yControlP[length(yControlP)],yControlP[length(yControlP)])
        } else { # this is not the last spell
          # use a control point from the next segment so they will match
          xControlP<-c(xControlP,onsets[max(sliceIndices)]+duration,onsets[max(sliceIndices)]+duration*2)
          yControlP<-c(yControlP,ycoords[v,max(sliceIndices)+1],ycoords[v,max(sliceIndices)+2])
        }
        xspline(xControlP,yControlP,shape=splines,border=vCol,lwd=vertex.cex[v],repEnds=FALSE)
       
      }
      
    } else {
      # don't worry about the gaps, just draw one spline
      # smoothness controlled by shape param, mapped from splines
      # =0 straight segments
      # >0 approx control points
      # <0 pass through control points
      xspline(c(onsets,termini[length(termini)]),ycoords[v,],shape=splines,border=vertex.col[v],lwd=vertex.cex[v],repEnds=TRUE)
    }
  } # end loop over v
  
  # draw some sepration lines at time points
  if(grid){
    for (s in seq_along(slices)){
      col<-par()$bg
      if(col=='transparent'){
        col<-'white'
      }
      lines(c(onsets[s],onsets[s]),range(ycoords,na.rm=TRUE),col=col)
    }
  }
  
  # do vertex labels if requested
  if(!is.null(labels.at)){
    for (t in seq_along(labels.at)){
      # find the closest slice start
      ycoord<-ycoords[,max(which(onsets<=labels.at[t]))]
      text(rep(labels.at[t],length(label)),ycoord,labels=label,cex=label.cex)
      
      
    }
  }
  
  # ---- EDGE RENDERING -----
  

  if(render.edges){
    if (verbose){
      message('rendering segments for each edge ...')
    }
    # loop over the time periods
    for (s in seq_along(slices)){
      # get all of the edges active for that time period from the slice
      eids<-valid.eids(slices[[s]])
      # iff there are any edges to plot...
      if(length(eids)>0){
        edge.col=plotArgs.network(slices[[s]],'edge.col',edge.col,edgetouse =eids)
        edge.lwd=plotArgs.network(slices[[s]],'edge.lwd',edge.lwd,edgetouse =eids)
        #eids<- goodIds[is.active(nd,e = goodIds,onset=onsets[s],terminus=termini[s])]
        for(w in seq_along(eids)){
          e<-eids[w]
          # get endpoints
          i<-slices[[s]]$mel[[e]]$inl
          j<-slices[[s]]$mel[[e]]$outl
          # plot a line segment
          lines(onsets[c(s,s)],ycoords[c(i,j),s],
                col = edge.col[w],
                lwd = edge.lwd[w])
        }
      }
    }
  }
  # invisibly return the y coords for possible later use, triming off the last
  # column that was added to make the splines work out
  invisible(ycoords[,1:(ncol(ycoords)-1)])
}

# convert a 2 column matrix of spells into a list of 2-element vectors
splMatrixToList<-function(spls){
  lapply(1:nrow(spls),function(r){spls[r,]})
}

# determine the appropriate colors for the vertex segment
getSegmentColor<-function(net,slice,v,s,onset,terminus,vertex.col){
  vCol<- ''
  if (is.character(vertex.col) & length(vertex.col)==1){
    vCol<-get.vertex.attribute.active(net,vertex.col,at=onset)[v]
    vCol <-ifelse(is.na(vCol),'#FFFFFF00',vCol)
    vCol<-as.color(vCol)
  } else if (is.function(vertex.col)){
    # get the names of the funtions arguments
    argnames<-names(as.list(args(vertex.col)))
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
        stop('unknown argument name "',arg,'" in function provided for vertex.col graphic parameter:',deparse(vertex.col))
      }
    }
    vCol<-do.call(vertex.col,args=args)[v]
  } else {
    vCol<-vertex.col[v]
  }
  return(vCol)
}
