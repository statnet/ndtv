#  File R/export.dot.R in package ndtv, part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 2012-2013 Statnet Commons
#######################################################################
require(network)

# x is a network to export
# coords is an optinal square matrix of coordinates
# all.dyads, if true, causes edges to be written out for all dyads in the network instead of just where edges exit
# use.names  if true, includes vertex names as labels
# attrname  if non-null, it is the name of an edge attribute giving the weight


export.dot <-
function(x,file="",coords=NULL,all.dyads=FALSE,vert.attrs=NULL,edge.attrs=NULL){
  if (!all.dyads){ #the easy way
   edges <- as.matrix(x,matrix.type="edgelist")
  } else {
    #the harder way
    matrix <- as.matrix(x,matrix.type="adjacency")
    matrix <- replace(matrix,matrix==0,all.dyads)
    edges <- as.matrix(as.network(matrix),matrix.type="edgelist")
  }
	cat("digraph g {\n",file=file)
  cat("graph[];\n",file=file, append=TRUE)
  cat("node[];\n",file=file, append=TRUE)
  vAttrStrings<-rep('',length(network.size(x)))
  if (!is.null(vert.attrs)){
    for(attrname in vert.attrs){
      vals<-get.vertex.attribute(x,attrname)
      vAttrStrings<-paste(vAttrStrings,attrname,'="',vals,'", ',sep='')
    }
  }
  #make sure we pick up isolates
  if (is.null(coords)){
    cat(paste(1:network.size(x)," [",vAttrStrings,"];\n",sep=""),file=file, append=TRUE)
	} else {
    for (n in 1:network.size(x)){
      cat(paste(n," [pos=\"",coords[n,1],",",coords[n,2],"\"",labels[n],"];\n",sep=""),file=file,append=TRUE)
    }
	}
	cat("edge [];\n",file=file, append=TRUE)
  eAttrStrings<-rep('',length=nrow(edges))
  if(!is.null(edge.attrs)){
    for (attrname in edge.attrs){
      vals<-get.edge.attribute(x,attrname=attrname)
      eAttrStrings<-paste(eAttrStrings,attrname,'="',vals,'", ',sep='')
    }
  }
	for(e in 1:nrow(edges)){
		cat(paste(edges[e,1],"->",edges[e,2],"[",eAttrStrings[e],"];\n"),file=file, append=TRUE)
	}	
	cat("}",file=file, append=TRUE)	
}
