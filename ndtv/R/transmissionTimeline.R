#  File R/transmissionTimeline.R in package ndtv, part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 2016 Statnet Commons
#######################################################################


transmissionTimeline<-function(x,time.attr,
                          label,
                          displaylabels = !missing(label),
                          label.cex = 0.7,
                          label.col = 1,
                          vertex.col = 2,
                          vertex.border=1,
                          vertex.lwd=1,
                          vertex.sides = 50,
                          vertex.cex = 1,
                          jitter=FALSE,
                          edge.col = 'gray',
                          edge.lty = 1,
                          edge.lwd = 1,
                          xlab='time',
                          ylab='generation',
                          ...){
  
  if(missing(time.attr)){
    if('transmat'%in%class(x)){
      time.attr<-'at'
    } else if ('tPath'%in%class(x)){
      requireNamespace('tsna')
      # this loads the tsna namespace so that as.network.tPath will be called below
      time.attr<-'tdist'
    } else {
      stop('time.attr argument must be given')
    }
  }
 
 net<-as.network(x)
   
 if (!time.attr%in%list.vertex.attributes(net)){ # or is it an attribute name?
    stop("time.attr ",time.attr,' is not a vertex attribute of the network')
  }
  el<-as.edgelist(net)
  times<-get.vertex.attribute(net,time.attr)
  coords<-matrix(0,nrow=network.size(net),ncol=2)
  yBin<-0
  if('tPath'%in%class(x)){  # not using is.tpath, because have not decided on dependency structure yet
    # assume tPath for now
    yBin<-net%v%'gsteps'
    coords<-cbind(times,yBin)
  } else {
    # find roots
    v<-setdiff(unique(el[,1]),unique(el[,2]))
    
    visited<-integer(0)
    while(length(v)>0){
      visited<-c(visited,v[1])
      coords[v[1],]<-c(times[v[1]],yBin)
      kids<-el[el[,1]==v[1],2] # look up kids on the edgelist
      # in case network was not actually a tree, make sure we don't loop forever
      if(any(kids%in%visited)){
        stop('vertex was revisited: network does not appear to be a tree')
      }
      v<-c(v[-1],kids)
      yBin<-yBin+1
    }
  }
  #op <- par(no.readonly = TRUE)
  if(jitter){
    # only consider the coordinates with finite values when computing jitter
    coords[is.finite(coords)]<-jitter(coords[is.finite(coords)])
  }
  # set up the plotting window
  plot(coords,pch=NA,xlab=xlab,ylab=ylab,...)
  
  # expand the various plot parameters using network defaults
  
  vertex.col<- plotArgs.network(net,'vertex.col',vertex.col)
  vertex.border<-plotArgs.network(net,'vertex.border',vertex.border)
  vertex.lwd<-plotArgs.network(net,'vertex.lwd',vertex.lwd)
  vertex.sides<-plotArgs.network(net,'vertex.sides',vertex.sides)
  vertex.cex<-plotArgs.network(net,'vertex.cex',vertex.cex)
  # remap vertex.sides to a vertex pch approximation
  vertex.pch <- sapply(vertex.sides,function(sides){
    switch (as.character(sides),
            '3' = 24,
            '4' = 22,
            '50' = 21)})
  
  edge.col<-plotArgs.network(net,'edge.col',edge.col)
  edge.lty <- plotArgs.network(net,'edge.lty',edge.lty)
  edge.lwd <- plotArgs.network(net,'edge.lwd',edge.lwd)
  if(missing(label)){
    label<-network.vertex.names(net)
  }
  label <-plotArgs.network(net,'labels',label)
  label.col<-plotArgs.network(net,'label.col',label.col)
  label.pos<-4
  label.cex<-plotArgs.network(net,'label.cex',label.cex)
  
  # plot the 'edges'
  if(nrow(el)>0){
    lapply(1:nrow(el),function(e){
      lines(c(coords[el[e,1],1],
              coords[el[e,2],1]), # xcoords
            c(coords[el[e,1],2],
              coords[el[e,2],2]),# ycoords
            col=edge.col[e],
            lwd=edge.lwd[e],
            lty=edge.lty[e]
      )
    })
  }
  
  # plot the vertices
  if(network.size(net)>0){
    points(coords[,1],coords[,2],bg=vertex.col,pch=vertex.pch,col=vertex.border,lwd=vertex.lwd,cex=vertex.cex)
    
    # plot labels
    if(displaylabels){
      text(coords[,1],coords[,2],labels = label,col=label.col,pos=label.pos,cex=label.cex)
    }
  }
  #par(op)
  invisible(coords)
}