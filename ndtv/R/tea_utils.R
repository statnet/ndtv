#  File R/tea_utils.R in package ndtv, part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 2012-2016 Statnet Commons
#######################################################################


#plots spells of a network as a timeline for diagnostics
timeline <-function(x,v=seq_len(network.size(x)), 
                    e=seq_along(x$mel),
                    plot.vertex.spells=TRUE, 
                    plot.edge.spells=TRUE, 
                    slice.par=NULL,
                    displaylabels=TRUE,
                    e.label=TRUE,
                    e.label.col='purple',
                    edge.lwd=1,
                    v.label,
                    v.label.col='blue',
                    vertex.cex=1,
                    cex,
                    adj=0,
                    edge.col=rgb(.5,.2,.2,.5),
                    vertex.col=rgb(.2,.2,.5,.5),
                    xlab,
                    ylab,
                    xlim,
                    ylim,
                    ...){
  if (!is.network(x)){
    stop('x must be a network object in timeline plot function')
  }
  # record the name of hte network before we mess with it
  network.name<-deparse(substitute(x))
  # if xlim is set, do an appropriate temporal subsetting of the network
  #if (!missing(xlim)){
  #  x <-network.extract(x,onset=xlim[1],terminus=xlim[2])
  #}
  
  if (plot.edge.spells & network.edgecount(x)>0){
    # create a dataframe of edge spells with onset, terminus, tail, head, edge.id
    tel <- as.data.frame(x,e=e,as.spellList=TRUE)[c(1:4,8)]
    if(!missing(xlim)){
      # if the xlim was set, need to exclude rows which are wholy outside of bounds
	    tel <- tel[sapply(seq.int(nrow(tel)),function(r){spells.overlap(xlim,tel[r,1:2])}),]
    } 
  } else {
    # not going to be drawing edges, so create a dummy dataframe
    tel<-as.data.frame(matrix(nrow=0,ncol=8))
  }
  # get representation of all vertex spells as a data frame
	tvl <- get.vertex.activity(x,v=v,as.spellList=TRUE)[1:3]
  # if the xlim was set, need to exclude rows which are wholy outside of bounds
  if (!missing(xlim)){
    tvl <-tvl[sapply(seq.int(nrow(tvl)),function(r){spells.overlap(xlim,tvl[r,1:2])}),]
  }
	
  # shrink tel to show only edges invovling desired vids
  # NOTE we do not shrink v to show only active vertices so that we won't hide edges 
  #  that accidentally connect inactive vertices
	v.rows <- which(tvl$vertex.id%in%v)
	e.rows <- intersect(which(tel$tail%in%v),which(tel$head%in%v))
  
  # because of multiple spells per element
  # create mapping for tel rows to plot rows by edge.id
  e.plot.rows.eid<-unique(tel$edge.id[e.rows])
  
  # also shrink e to avoid edges that have been eliminated
  e <-e[e%in%e.plot.rows.eid]
  
  # ditto for vertices
  v.plot.rows<-unique(tvl$vertex.id[v.rows])
	
	rows <- length(e.plot.rows.eid)*plot.edge.spells
  
  
  # deal with possible info rows and time range
  times<-unique(c(tel$onset[e.rows],tel$terminis[e.rows],tvl$onset[v.rows],tvl$terminus[v.rows]))
  if (length(times)==0){
    times<-0
  }
  # remove possible infs
  times<-times[!is.infinite(times)]
  
  xmin<-min(times)
  xmax<-max(times)
  min.inf.replace=xmin-1
  max.inf.replace=xmax+1
  tel$onset[tel$onset==-Inf] <- min.inf.replace
  tel$terminus[tel$terminus==Inf] <- max.inf.replace
  tvl$terminus[tvl$terminus==Inf] <- max.inf.replace
  tvl$onset[tvl$onset==-Inf] <- min.inf.replace
  
  # figure plot x range in not set
  if(missing(xlim)){
    xlim<-c(xmin,xmax)
  }
  
  # figure text for y-axis
  if(missing(ylab)){
    ylab <- paste(ifelse(plot.edge.spells,'edge',''),
                  ifelse(plot.edge.spells & plot.vertex.spells,'and',''),
                  ifelse(plot.vertex.spells,'vertex','')
                  ,'spells of network ',network.name)
 } 
  if(missing(xlab)){
    xlab<-'time'
  }
 
  if(missing(ylim)){
    ylim<-c(1,(rows*plot.edge.spells)+(length(v.plot.rows)*plot.vertex.spells))
  }
  # do the plot window and range               
	plot(NULL,NULL, xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,yaxt='n',...)
		
  if(missing(cex)){
    cex<-0.5
  }
  
	if (plot.edge.spells & network.edgecount(x)>0){
	  # do potential attribute matching for colors, etc 
    
    if(missing(e.label) ){
      e.label<-TRUE
    }
    e.label<-plotArgs.network(x,argName = 'edge.label',argValue = e.label,edgetouse = e)
    e.label.col<-plotArgs.network(x,argName = 'edge.label.col',argValue=e.label.col,edgetouse=e)
    edge.col<-plotArgs.network(x,argName = 'edge.col',argValue=edge.col,edgetouse=e)
    edge.lwd<-plotArgs.network(x,argName = 'edge.lwd',argValue=edge.lwd,edgetouse=e)
    # loop over all the edge spells to be drawn
		for(i in seq_len(length(e.rows))){
		  eid<-tel$edge.id[e.rows[i]]# this eid
      eid.index<-which(e==eid)  # where along the index of e is this eid located
			y<-which(e.plot.rows.eid==eid) # what is the vertical index on y-axis where eid is drawn
			lines(c(tel$onset[e.rows[i]],tel$terminus[e.rows[i]]),c(y,y),col=edge.col[eid.index],lwd=edge.lwd[eid.index],...)
			if (displaylabels){
        # compute start position for spell in case xlim is viewing part of graph
        label.start<-max(tel$onset[e.rows[i]],xlim[1])
				#text(tel$onset[e.rows[e]],y,labels=e.label[e.rows[e]],col="purple",cex=cex,...)
				text(label.start,y,labels=e.label[eid.index],col=e.label.col[eid.index],cex=cex,adj=adj,...)
			}
		}
		
	}
	#now plot nodes
	if (plot.vertex.spells){
	  # do potential attribute matching for colors, etc
    if(missing(v.label)){
      v.label<-network.vertex.names(x)
    }
    # possibly expand / lookup vertex labels
    v.label<-plotArgs.network(x,argName='label',argValue = v.label)
    # possibly expand / look up vertex colors for lines
    vertex.col<-plotArgs.network(x,argName='vertex.col',argValue=vertex.col)
    # possibly expand / lookup colors for vertex labels
    v.label.col<-plotArgs.network(x,argName='label.col',argValue=v.label.col)
    # possibly expand / lookup vertex.cex (line width)
    vertex.cex <- plotArgs.network(x,argName='vertex.cex',argValue=vertex.cex)
    
		for (r in seq_len(length(v.rows))){
      y<-which(v.plot.rows==tvl$vertex.id[v.rows[r]])+rows
      vid<-tvl$vertex.id[v.rows[r]]
			lines(c(tvl$onset[v.rows[r]],tvl$terminus[v.rows[r]]),c(y,y),col=vertex.col[vid],lwd=vertex.cex[vid],...)
			if (displaylabels){
			  # so labels will still draw when viwing only part of the network
        label.start <-max(tvl$onset[v.rows[r]],xlim[1]) 
				text(label.start,y,labels=v.label[vid],col=v.label.col[vid],cex=cex,adj=adj,...)
			}
		}
		rows <- length(v.rows)
	}
  
  # optionally plot slice bounds
  if(!is.null(slice.par)){
    .plot.slice.par(slice.par,(length(e.plot.rows.eid*plot.edge.spells)+(length(v.plot.rows)*plot.vertex.spells))+1,xlim)
  }
	
}


# helper function for timeline() to show slices bins on plot
.plot.slice.par<-function(slice.par,nrows,xlim){
  #check inputs
  if(is.null(slice.par$start)){
    stop("slice.par parameter list is missing a 'start' component to give the start of the sliceing period")
  }
  if(is.null(slice.par$end)){
    stop("slice.par parameter list is missing an 'end' component to give the end of the sliceing period")
  }
  if(is.null(slice.par$interval)){
    stop("slice.par parameter list is missing an 'interval' component to give the time between slices")
  }
  if(is.null(slice.par$aggregate.dur)){
    stop("slice.par parameter list is missing an 'aggregate.dur' component to give the aggregation duration for each slice")
  }
  s<-slice.par$start
  while(s<slice.par$end){
    # make it so durations of 0 are still visable
    minDur<-(xlim[2]-xlim[1])/500
    dur<-max(slice.par$aggregate.dur,minDur)
    # plot a rect corresponding to each slice
    # each slice gets three rects of increasing transparency to indicate right-openess of interval
    rect(s,-2,s+minDur,nrows+2,col=rgb(.1,.1,.1,.2),border=NA)
    rect(s+minDur,-2,s+dur-(minDur*2),nrows+2,col=rgb(.1,.1,.1,.1),border=NA)
    rect(s+dur-(minDur*2),-2,s+dur-minDur,nrows+2,col=rgb(.1,.1,.1,.07),border=NA)
    rect(s+dur-minDur,-2,s+dur,nrows+2,col=rgb(.1,.1,.1,.025),border=NA)
    s<-s+slice.par$interval
  }
}


#applies a function to all defined values in range, 
#TODO: should take range operators
aggregate.vertex.attribute.active<-function(net,attrname,fun){
  return(fun(unlist(lapply(lapply(net$val,"[[",paste(attrname,".active",sep='')),"[[",1))))
}
