#  File R/installers.R in package ndtv, part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 20123-2013 Statnet Commons
#######################################################################
#functions to install, configure, and check various 3rd party components of ndtv

check.java <-function(){
  if (.Platform$OS.type == "unix" | .Platform$OS.type == "windows"){
    if (Sys.which("java")!=''){
      #system("java -version",intern=TRUE)   somehow this always prints, so not using
      return(TRUE)
    } else {
      warning(paste("Unable to locate Java on this machine. Please visit http://java.com for install instructions"))
    }
  }  else {
    warning(paste("not sure how to check for java on ",.Platform$OS.type,"implement me"))
  }
  warning("unable to locate Java")
  return(FALSE)
}

check.mdsj <-function(){
  #check that java is installed and working
  java <-check.java()
  #TODO: assumes mdsj is in package, which assumes package dir is writable, need to add alternate
  if(java){
    mdsj.path <- file.path(path.package('ndtv'),'exec/mdsj.jar')
    if(!file.exists(mdsj.path)){
      warning("The MDSJ java library is not installed, please run ndtv:::install.mdsj()")
    } else {
      return(file.path(path.package('ndtv'),'exec/'))
    }
  } else {
    warning("The MDSJ library can only run if Java is installed on the system")
  }
  return(NULL)
}

install.mdsj <-function(){
  #print non-comercial use warning
  #TODO: assumes mdsj is in package, which assumes package dir is writable, need to add alternate
 cat("MDSJ is a free Java library for Multidimensional Scaling (MDS).\n It is a free, non-graphical, self-contained, lightweight implementation of basic MDS algorithms and intended to be used both as a standalone application and as a building block in Java based data analysis and visualization software. \n Algorithmics Group. MDSJ: Java Library for Multidimensional Scaling (Version 0.2). Available at http://www.inf.uni-konstanz.de/algo/software/mdsj/. University of Konstanz, 2009.  USE RESTRICTIONS: Creative Commons License 'by-nc-sa' 3.0." )
 browseURL("http://www.inf.uni-konstanz.de/algo/software/mdsj/")
}

check.graphviz <-function(){
  if (.Platform$OS.type == "unix" | .Platform$OS.type == "windows"){
    if (Sys.which("neato")!=''){
      return(system("neato -V"))
    } else {
      warning("The Graphviz neato utility does not appear to be installed on this system")
    }
  } else {
    warning(paste("not sure how to check for Graphviz on ",.Platform$OS.type,"implement me"))
    
  }
  return(FALSE)
}

install.graphviz <-function(){
  cat("Please visit the Graphviz website and install the libraries on your machine: http://www.graphviz.org/Download.php")
  browseURL("http://www.graphviz.org/Download.php")
}

check.ffmpeg <- function(){
   if (.Platform$OS.type == "unix" || .Platform$OS.type == "windows"){
    if (Sys.which("ffmpeg")!=''){
      return(Sys.which("ffmpeg"))
    } else {
      warning("The ffmpeg video utility does not appear to be installed on the system, or the path is not set correctly. Please run ndtv:::install.ffmpeg() for more information ")
    }
  } else {
    warning(paste("not sure how to check for ffmpeg video utility on ",.Platform$OS.type,"implement me"))
    
  }
   return(FALSE)
}

install.ffmpeg <-function(){
  if (.Platform$OS.type == "windows"){
    cat("To export movies, please install ffmpeg for windows from http://ffmpeg.zeranoe.com/builds/\n type ?install.ffmpeg for more detailed instructions")
    browseURL("http://ffmpeg.zeranoe.com/builds/")
    
  } else if (.Platform$OS.type == "unix") {
    cat("To export movies, please install ffmpeg using your system's package manager.\n Type ?install.ffmpeg for more detailed instructions.")
  } else {
    cat("To determine if ffmpeg can be installed on your system, please visit http://ffmpeg.org")
    browseURL("http://ffmpeg.org")
  }
}