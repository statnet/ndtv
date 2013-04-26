#  File R/tea_utils.R in package ndtv, part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 2012-2013 Statnet Commons
#######################################################################


#plots spells of a network as a timeline for diagnostics
timeline <-function(x,v=seq_len(network.size(x)), e=seq_along(x$mel),plot.vertex.spells=TRUE, plot.edge.spells=TRUE, slice.par=NULL,displaylabels=TRUE,e.label,v.label,cex,edge.col=rgb(.5,.2,.2,.5),vertex.col=rgb(.2,.2,.5,.5),xlab,ylab,xlim,...){
  if (!is.network(x)){
    stop('x must be a network object in timeline plot function')
  }
  
	tel <- get.edge.activity(x,e=e,as.spellList=TRUE)[c(1:4,8)]
	tvl <- get.vertex.activity(x,v=v,as.spellList=TRUE)[1:3]
	
  # shrink tel to show only edges invovling desired vids
	v.rows <- which(tvl$vertex.id%in%v)
	e.rows <- union(which(tel$tail%in%v),which(tel$head%in%v))
  
  # because of multiple spells per element
  # create mapping for tel rows to plot rows by edge.id
  e.plot.rows<-unique(tel$edge.id[e.rows])
  
  # ditto for vertices
  v.plot.rows<-unique(tvl$vertex.id[v.rows])
	
	rows <- length(e.plot.rows)*plot.edge.spells
  
  
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
                  ,'spells of network ',deparse(substitute(x)))
 } 
  if(missing(xlab)){
    xlab<-'time'
  }
                 
	plot(NULL,NULL, xlim=xlim,ylim=c(1,(rows*plot.edge.spells)+(length(v.plot.rows)*plot.vertex.spells)),xlab=xlab,ylab=ylab,yaxt='n',...)
		
  if(missing(cex)){
    cex<-0.5
  }
  
	if (plot.edge.spells){
	  # do potential attribute matching for colors, etc 
    
    if(missing(e.label)){
      e.label<-tel$edge.id
    }
		for(e in seq_len(length(e.rows))){
			y<-which(e.plot.rows==tel$edge.id[e.rows[e]])
			lines(c(tel$onset[e.rows[e]],tel$terminus[e.rows[e]]),c(y,y),col=edge.col,...)
			if (displaylabels){
				text(tel$onset[e.rows[e]],y,labels=e.label[e.rows[e]],col="purple",cex=cex,...)
			}
		}
		
	}
	#now plot nodes
	if (plot.vertex.spells){
	  # do potential attribute matching for colors, etc
    if(missing(v.label)){
      v.label<-network.vertex.names(x)
    }
		for (r in seq_len(length(v.rows))){
      y<-which(v.plot.rows==tvl$vertex.id[v.rows[r]])+rows+1
			lines(c(tvl$onset[v.rows[r]],tvl$terminus[v.rows[r]]),c(y,y),col=vertex.col,...)
			if (displaylabels){
				text(tvl$onset[v.rows[r]],y,labels=v.label[tvl$vertex.id[v.rows[r]]],col="blue",cex=cex,...)
			}
		}
		rows <- length(v.rows)
	}
  
  # optionally plot slice bounds
  if(!is.null(slice.par)){
    .plot.slice.par(slice.par,(length(e.plot.rows*plot.edge.spells)+(length(v.plot.rows)*plot.vertex.spells))+1)
  }
	
}


# helper function for timeline() to show slices bins on plot
.plot.slice.par<-function(slice.par,nrows){
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
    dur<-max(slice.par$aggregate.dur,(slice.par$end-slice.par$start)/500)
    # plot a rect corresponding to each slice
    rect(s,-2,s+dur,nrows+2,col=rgb(.1,.1,.1,.1),border=NA)
    s<-s+slice.par$interval
  }
}


#applies a function to all defined values in range, 
#TODO: should take range operators
aggregate.vertex.attribute.active<-function(net,attrname,fun){
  return(fun(unlist(lapply(lapply(net$val,"[[",paste(attrname,".active",sep='')),"[[",1))))
}
