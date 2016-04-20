#  File R/animate.layouts.R in package ndtv, part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 2012-2016 Statnet Commons
#######################################################################
#methods and outlines for ndtv package

#--- animation appropriate layouts

#TODO: good default value for default.dist

network.layout.animate.kamadakawai <-function(net, dist.mat=NULL, default.dist=NULL,seed.coords=NULL, layout.par=list(),verbose=FALSE){
  #if seed.coord already set in layoutpar, overide
  if (is.null(dist.mat)){
    dist.mat <- layout.distance(net,default.dist=default.dist)
  }
  layout.par$seed.coord <- seed.coords
  layout.par$elen <-dist.mat
  coords <- network.layout.kamadakawai(net,layout.par=layout.par)
  return(coords)
}

# MDSJ java library is from http://www.inf.uni-konstanz.de/algo/software/mdsj/ 
# License info: Creative Commons by-nc-sa http://www.inf.uni-konstanz.de/algo/software/mdsj/#license
# Auther christian.pich@gmx.de is OK with including in this GPL package. He wrote:
# If your package is non-commercial, I see no problem in including MDSJ in your implementation.
# Please feel free to contact me regarding technical MDS issues 

network.layout.animate.MDSJ <-function(net, dist.mat=NULL, default.dist=NULL,seed.coords=NULL, layout.par=list(max_iter=50,dimensions=2),verbose=TRUE){
  # check that mdsj library can be located
  mdsj.path <- check.mdsj()
  
  # if not, give warning, and use KK instead so examples don't break
  if(is.null(mdsj.path)){
    warning("Unable to locate the MDSJ library, using KamadaKawai layout instead")
    return(network.layout.animate.kamadakawai(net, dist.mat=dist.mat, default.dist=default.dist,seed.coords=seed.coords, layout.par=list(),verbose=TRUE))
  }
  
  n <- network.size(net)
  max_iter <- 50 #layout.par$max_iter
  dim<-2  # default to two dimeions
  if (!is.null(layout.par$dimensions)){  # if dimensions specified in layout par, grab 'em
    dim<-as.numeric(layout.par$dimensions)
  }
  #if seed.coord already set in layoutpar, overide
  if (is.null(dist.mat)){
    dist.mat <- layout.distance(net,default.dist=default.dist)
  }
  if(!isSymmetric(dist.mat)){
    warning("Input distance matrix is not symmetric, will give unpredictable results")
  }
  filename <- tempfile("matrix",fileext=".txt")
  coord.file <- tempfile("coords",fileext=".txt")
  write.matrix(dist.mat,file=filename)
  if (!is.null(seed.coords)){
    # limit seeds coords to the specified number of dimensions
    if(ncol(seed.coords)!=dim){
      warning('Dimensions of seed coord matrix does not match dimensions specified in layout.par ',dim,'. Only the first ',dim,' columns will be used.')
    }
    write.table(seed.coords[,seq.int(dim)],file=coord.file,col.names=FALSE,row.names=FALSE)
  } else {
    coord.file <- ""
  }
  sep<-':'
  if (.Platform$OS.type=='windows'){
    sep<-';'
  }
  verbosity<-0  # make a verbose flag for java
  if(verbose){
    verbosity<-1
  }
 
  #command = paste("java -jar",mdsjpath,"-e-2",filename)
  #TODO java classpath is platform dependent, need to modify?  
  #/home/skyebend/SNA_health:/home/skyebend/SNA_health/mdsj.jar MDSJWrapper  
  command = paste("java -cp ",paste(mdsj.path,file.path(mdsj.path,"mdsj.jar"),sep=sep),"MDSJWrapper", n,dim,verbosity,max_iter,filename,coord.file)
  #print(command)
  output <- system(command,intern=TRUE)
  if(verbose){
    print(output[1:(max(1,(length(output)-nrow(dist.mat))))])
  }
  unlink(filename)
  if (!is.null(seed.coords)){
    unlink(coord.file)
  }
  #NEED TO CHECK FOR ERROR
  #only grab the last n lines
  coords <- NULL
  if(length(output)>=n){
    coords <- matrix(data=as.numeric(unlist(strsplit(output[(length(output)-nrow(dist.mat)+1):length(output)]," "))),ncol=dim,byrow=TRUE)
  } else {
    # something must be wrong, so print the output (unless it would already have been printed)
    if (!verbose){print(output)}
    error=stop("Unable to parse coordinates returned MDSJ java code")
  }
  return(coords)
}


network.layout.animate.Graphviz <-function(net, dist.mat=NULL, default.dist=NULL,seed.coords=NULL,layout.par=list(),verbose=TRUE){
  
  gv.installed <- check.graphviz()
  # if not, give warning, and use KK instead so examples don't break
  if(!gv.installed){
    warning("Unable to locate the Graphviz library, using KamadaKawai layout instead")
    return(network.layout.animate.kamadakawai(net, dist.mat=dist.mat, default.dist=default.dist,seed.coords=seed.coords, layout.par=list(),verbose=TRUE))
  }
  
  #check ags from layout.par
  if (!is.null(layout.par)){
    if(!is.list(layout.par)){
      stop("layout.par argument must be a list of appropriate layout parameters")
    }
   gv.engine<-layout.par$gv.engine
   gv.engine<-match.arg(gv.engine,c('neato','dot','fdp','circo','osage','sfdp','twopi'))
    
   if(is.null(layout.par$gv.args)){
     gv.args<-''
   } else {
     gv.args<-layout.par$gv.args
   }
   
   # need to determine if we will be including weights from the distance matrix, the edge attributes
   # or neither
   gv.len.mode<-layout.par$gv.len.mode
   if (is.null(gv.len.mode)){
     gv.len.mode<-'gv.edge.len'
   }else{
     gv.len.mode<-match.arg(gv.len.mode,c('gv.edge.len','ndtv.distance.matrix'))
   }
   
   # possible list of edge attributes from the network to include
   gv.edge.attrs<-layout.par$gv.edge.attrs
    
  } else {
    # set to defaults
    gv.engine<-'neato'
    gv.len.mode<-'gv.edge.len'
    gv.edge.attrs=NULL
  }
  
  
  
  
  n <- network.size(net)
  #if seed.coord already set in layoutpar, overide
  if (!is.null(dist.mat) & gv.len.mode=='gv.edge.len'){
    warning("distance matrix was ignored because gv.len.mode=='gv.edge.len'")
  }
  
  if (is.null(default.dist)){
    default.dist<-FALSE
  }

  filename <- tempfile("network",fileext=".dot")
  
  if (gv.len.mode=='ndtv.distance.matrix'){
    if (is.null(dist.mat)){
      stop("dist.mat must not be null to use the gv.len.mode=='ndtv.distance.matrix' option")
    }
    # pass it the distance matrix as len, ignore edge attributes
    export.dot(net,filename,coords=seed.coords,all.dyads=dist.mat, edge.attrs=gv.edge.attrs) 
  } else if (!is.null(default.dist)) {
    # pass in the default dist to create distance matrix as len, ignore edge attributes        
    export.dot(net,filename,coords=seed.coords,all.dyads=default.dist, edge.attrs=gv.edge.attrs) 
  } else {  #(gv.len.mode=='gv.edge.len')
    #include edge attributes (especially 'len' ) if defined so that gv can uses them
    export.dot(net,filename,coords=seed.coords,edge.attrs = gv.edge.attrs)
  }
  #TODO: should be able to pipe .dot contents directly into GV and skip disk IO ?
  command <- paste(gv.engine,"-Tplain",gv.args,filename)
  #print(command)
  output <- system(command,intern=TRUE)
  unlink(filename)
 
  #NEED TO CHECK FOR ERROR
  coords <- NULL
  if(length(output)>n){
    coords <- parseCoordsFromGraphvizPlain(output)
  } else {
    if (!verbose){print(output)}
    stop("Unable to parse coordinates returned from Graphviz")
  }
  return(coords)
}


network.layout.animate.MDS <- function(nw, layout.par){
  #TODO: write a wrapper for MDS
}

network.layout.animate.smacofMDS <- function(nw, layout.par){
  #TODO: write a wrapper for smacofMDS
}

#use a stored attribute of the network as the layout param
network.layout.animate.useAttribute <- function(net, dist.mat=NULL, default.dist=NULL,seed.coords=NULL,layout.par=list(x='x',y='y'),verbose=TRUE){
  #TODO: write a wrapper for user-stored layout coords
  # check that coord attribute name args are given
  if (is.list(layout.par) & length(layout.par)>0){
    if (is.null(layout.par$x) | is.null(layout.par$y)){
      stop("useAttribute layout must have layout.par list argument with elements named 'x' and 'y' giving the names of the vertex attributes containing the x and y coordinates to use")
    }
    xattr <- layout.par$x
    yattr <-layout.par$y
  } else {
    # default to x and y
    xattr <- 'x'
    yattr <- 'y'
  }
  
  # check that they correspond to a valid attribute and the attribute is numeric
  if (!xattr%in%list.vertex.attributes(net) ){
    stop(paste("useAttribute layout is unable to find vertex attribute",xattr,"containing x coordinate information"))
  }
  if (!yattr%in%list.vertex.attributes(net) ){
    stop(paste("useAttribute layout is unable to find vertex attribute",yattr,"containing y coordinate information"))
  }
  xpos <- get.vertex.attribute(net,xattr)
  ypos <- get.vertex.attribute(net,yattr)
  
  if (!is.numeric(xpos) | !is.numeric(ypos)){
    stop("attribute values for 'useAttribute' layout coordinates must be numeric and defined for each time step")
  }
  
  # todo: check for missing?
  coords<-cbind(xpos,ypos)
  return(coords)
  
}

