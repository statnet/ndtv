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
export.dot <-
function(x,file="",coords=NULL,all.dyads=FALSE){
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
  #make sure we pick up isolates
  if (is.null(coords)){
    cat(1:network.size(x),sep=";\n",file=file, append=TRUE)
	} else {
    for (n in 1:network.size(x)){
      cat(paste(n," [pos=\"",coords[n,1],",",coords[n,2],"\"];\n",sep=""),file=file,append=TRUE)
    }
	}
	cat("edge [];\n",file=file, append=TRUE)
	for(e in 1:nrow(edges)){
		cat(paste(edges[e,1],"->",edges[e,2],";\n"),file=file, append=TRUE)
	}	
	cat("}",file=file, append=TRUE)	
}
