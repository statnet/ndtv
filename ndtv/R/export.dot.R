#  File R/export.dot.R in package ndtv, part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 2012-2016 Statnet Commons
#######################################################################

require(network)

# x is a network to export
# coords is an optinal square matrix of coordinates
# all.dyads, if true, causes edges to be written out for all dyads in the network instead of just where edges exit
# use.names  if true, includes vertex names as labels
# attrname  if non-null, it is the name of an edge attribute giving the weight


export.dot <-
function(x,file="",coords=NULL,all.dyads=FALSE,vert.attrs=NULL,edge.attrs=NULL){
  
  if (is.logical(all.dyads) && !all.dyads){ #the easy way
    # use only existing edges in the network
   edges <- as.matrix(x,matrix.type="edgelist")
  } else if (is.numeric(all.dyads)){
    if (length(all.dyads)==1){
      #the harder way. need to print out all the dyads, replaces zeros with the value of all.dyads
      matrix <- as.matrix(x,matrix.type="adjacency",as.sna.edgelist = FALSE)
      matrix <- replace(matrix,matrix==0,all.dyads)
    } else {
      # assume it is an adjacency matrix of desired distances
      matrix<-as.matrix(all.dyads)
    }
      edges <- as.matrix(as.network(matrix,matrix.type = 'adjacency',ignore.eval = FALSE,names.eval='len',loops = has.loops(x),directed = is.directed(x)),matrix.type="edgelist",attrname = 'len')
     # give a warning that all edge attributes other than length will be ignored
     if (!is.null(edge.attrs)){
       warning('edge attributes ',edge.attrs,' were ignored in dot file output because all.dyads matrix was used')
       edge.attrs<-NULL
     }
  } else {
    stop('argument all.dyads must be FALSE, numeric length 1, or a matrix with dimension equal to the size of the network')
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
      cat(paste(n," [pos=\"",coords[n,1],",",coords[n,2],"\"",vAttrStrings,"];\n",sep=""),file=file,append=TRUE)
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
  if (ncol(edges)==2){
    # print edges with possible attribute strings
  	for(e in seq_len(nrow(edges))){
  		cat(paste(edges[e,1],"->",edges[e,2],"[",eAttrStrings[e],"];\n"),file=file, append=TRUE)
  	}	
  } else if (ncol(edges)==3){
    # assume we are probably doing all dyads and the 3rd column is the desired edge length
    for(e in seq_len(nrow(edges))){
      cat(paste(edges[e,1]," -> ",edges[e,2],' [len="',edges[e,3],'"];\n',sep=''),file=file, append=TRUE)
    }	
  }
	cat("}",file=file, append=TRUE)	
}


# TODO: possibly replace this functions and make things faster using RGraphviz package:
#http://www.bioconductor.org/packages/release/bioc/html/Rgraphviz.html

#output of Graphviz -Tplain format looks like this:

# graph 1 7.9819 8.5204
# node 1 3.1943 3.3635 0.75 0.5 1 solid ellipse black lightgrey
# node 2 4.159 3.9107 0.75 0.5 2 solid ellipse black lightgrey
# node 3 4.8287 2.8863 0.75 0.5 3 solid ellipse black lightgrey
# node 4 3.7991 5.0937 0.75 0.5 4 solid ellipse black lightgrey
# edge 2 1 4 3.8696 3.7465 3.7875 3.7 3.6966 3.6484 3.6087 3.5986 solid black
# edge 3 2 4 4.6767 3.1188 4.5911 3.2497 4.4825 3.4159 4.3868 3.5622 solid black
# edge 4 2 4 3.8737 4.8486 3.9221 4.6895 3.9862 4.4787 4.0414 4.2973 solid black
# stop

# documented here: http://www.graphviz.org/content/output-formats#dplain

# this function parses the output above and returns a two-column coordinate matrix
parseCoordsFromGraphvizPlain<-function(text,dim=2){
  coords<-NULL
  # grab all the rows that start with 'node'
  nodeLines<-grep("^node",text,value=TRUE)
  # check for 0-vertex net
  if (length(nodeLines)>0){
    # split on spaces and grab the 3rd and 4th elements
    nodeLines<-lapply(nodeLines,function(line){strsplit(line,split=" ")[[1]][2+seq.int(dim)]})
    # reformat into matrix
    coords<-matrix(as.numeric(unlist(nodeLines)),ncol=dim,byrow=TRUE)
  } else {
    coords<-matrix(0,nrow=0,ncol=dim)
  }
  
  if(is.null(coords)){
    stop('Unable to parse vertex coordinates from Graphviz plain-formated input')
  }
  return(coords)
}


