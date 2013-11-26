

#TODO: change structure so that x axis (and params) are based on time rather than slice index
#TODO: when the network contains isolates, the values for default.dist tend to dominate the network, provide option to not include isolates
#TODO: compute attributes on a per-slice pasis so that sizes and colors for a vertex can change over time
#TODO:allow for missing networks and inactive vertices
#TODO: add ability to specifiy values for labels, colors for labels, etc.
#TODO: add ability to specify alternate algorithms (isoMDS, sammon, cmdscale)
#TODO: add ability to specify attributes for weights

proximity.timeline<-function(nd,start = NULL, end = NULL, time.increment = NULL, 
                             onsets=NULL, termini=NULL, default.dist=NULL,
                             vertex.col='#55555555',labels.at=NULL,vertex.cex=2,
                             splines=-.2,grid=TRUE,mode=c('isoMDS','sammon','cmdscale'),
                             draw.inactive=c('default','ghost','no','yes'),...){
  if (!is.networkDynamic(nd)) {
    stop("proximity.timeline requires that the first argument be a networkDynamic object")
  }
  mode<-match.arg(mode)
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
  slices<-get.networks(nd,retain.all.vertices=TRUE,onsets=onsets,termini=termini,...) 
  ycoords<-matrix(0,nrow=network.size(nd),ncol=length(slices)+1)
  # set up initial starting coords
  # have to jitter to make sure no coords are the same
  prev_ycoord<-matrix(jitter(cmdscale(layout.distance(slices[[1]],default.dist=default.dist),k=1)),ncol=1)
  hasGaps<-FALSE # for tracking if there are any empty spells, useful later
  for (s in seq_along(slices)){
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
        cmdscale=cmdscale(mat,k=1)
      )       
      prev_ycoord<-ycoords[,s,drop=FALSE]
    } else {
      # I would like to leave out this segment, or draw it dotted
      # but for now, just copy previous values
      if (s>1){
        ycoords[,s]<-ycoords[,s-1]
      }
    }
    if (network.size.active(nd,onset=onset,terminus=terminus)<network.size(nd)){
      hasGaps<-TRUE
    }
  } # end slice setup loop
  #copy the last coord to bound the last spline
  ycoords[,ncol(ycoords)]<-ycoords[,ncol(ycoords)-1]
  
  # TODO: add label processing
  
  # if vertex.cex is an attribute name, use that
  if (is.character(vertex.cex) && (length(vertex.cex == 1))) {
    temp <- vertex.cex
    vertex.cex <- rep(get.vertex.attribute(nd, vertex.cex), 
                      length = network.size(nd))
    if (all(is.na(vertex.cex))) 
      stop("Attribute '", temp, "' had illegal missing values for vertex.cex or was not present in proximity.timeline.")
  } else {
    vertex.cex<-rep(vertex.cex,network.size(nd))
  }
  
  # if vertex.col is an attribute name, use that
  if (is.character(vertex.col) && (length(vertex.col) == 1)) {
    temp <- vertex.col
    vertex.col <- rep(get.vertex.attribute(slice, vertex.col), 
                      length = network.size(nd))
    if (all(is.na(vertex.col))) 
      vertex.col <- rep(temp, length = network.size(nd))
    else {
      if (!all(is.color(vertex.col), na.rm = TRUE)) 
        vertex.col <- as.color(vertex.col)
    }
  }
  # figure out draw.inactive setting
  draw.inactive<-match.arg(draw.inactive)
  if (draw.inactive=='default' & hasGaps){
    draw.inactive<-'ghost'
  }
  # create a new plot
  plot(NA,NA,xlim=c(onsets[1],termini[length(termini)]),ylim=range(ycoords,na.rm=TRUE),xlab='time',ylab=paste('approx. distance among vertices of',substitute(nd)))
  
  # loop over vertices, drawing splines
  for (v in seq_len(nrow(mat))){
    
    if (draw.inactive=='ghost'){
      # 'ghost' in light background line to connect active spells
      xspline(c(onsets,termini[length(termini)]),ycoords[v,],shape=splines,border='#55555555',lwd=1,repEnds=TRUE,lty=3)
    }  
    
    if (draw.inactive%in%c('ghost','no')){ # should we render as continuous splines or in vertex activity chunks
      # render spells in segments to avoid slices where vertex is inactive
      # figure out contiguous set of slices
      
      actives<-list()
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
      for(r in seq_len(length(actives))){
        sliceIndicies<-seq(from=actives[[r]][1],to=actives[[r]][2])
        # if onset==terminus, can't draw a spline because not enough control points
        duration <-termini[sliceIndicies[1]]-onsets[sliceIndicies[1]]
        if(length(sliceIndicies)<2){
          lines(c(onsets[sliceIndicies],onsets[sliceIndicies]+duration),
                c(ycoords[v,sliceIndicies],ycoords[v,sliceIndicies]),col=vertex.col[v],lwd=vertex.cex[v])
        } else {
          # smoothness controlled by shape param, mapped from splines
          # =0 straight segments
          # >0 approx control points
          # <0 pass through control points
          xspline(c(onsets[sliceIndicies],onsets[max(sliceIndicies)]+duration),ycoords[v,c(sliceIndicies,max(sliceIndicies))],shape=splines,border=vertex.col[v],lwd=vertex.cex[v],repEnds=TRUE)
        }
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
  
  # do labels if requeted
  if(!is.null(labels.at)){
    for (i in seq_along(labels.at)){
      labels<-network.vertex.names(nd)
      # find the closest slice start
      ycoord<-ycoords[,max(which(onsets<=labels.at[i]))]
      text(rep(labels.at[i],length(labels)),ycoord,labels=labels)
    }
  }
}

