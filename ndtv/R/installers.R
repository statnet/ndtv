#  File R/installers.R in package ndtv, part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 20123-2016 Statnet Commons
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

check.mdsj <-function(ask=TRUE){
  #check that java is installed and working
  java <-check.java()
  #TODO: assumes mdsj is in package, which assumes package dir is writable, need to add alternate
  if(java){
    mdsj.path <- file.path(path.package('ndtv'),'java/mdsj.jar')
    mdsj.dir <- file.path(path.package('ndtv'),'java/')
    if(!file.exists(mdsj.path)){
      message("The MDSJ Java library does not appear to be installed. The ndtv package can use MDSJ to provide a fast accurate layout algorithm. It can be downloaded from http://algo.uni-konstanz.de/software/mdsj/")
      # ask user if the want to download
      #  how will this work during automated test?
      # "In non-interactive use the result is as if the response was RETURN and the value is ""."
      if (ask){
        n <- readline("Do you want to automatically download and install the MDSJ Java library? (y/N): ")
      } else {
        # if not ask, just try to install
        n<-'Yes'
      }
      if (n%in%c('y','Y',"Yes","yes")){
        install.mdsj(mdsj.dir)
        if(!file.exists(mdsj.path)){
          warning('MDSJ install failed.')
        } else {
          return(mdsj.dir)
        }
      } else {
        message("The MDSJ library was not installed.")
        return(NULL)
      }
      
      
    } else {
      return(mdsj.dir)
    }
  } else {
    warning("The MDSJ library can only run if Java is installed on the system")
  }
  return(NULL)
}

install.mdsj <-function(install.path){
  
 
 # test if install path is writeable
 if(file.access(install.path,mode=2)!=0){
   stop('Unable to install MDSJ because the package path ',install.path,'does not appear to have write permissions')
 }
 # download file
 mdsjURL<-'http://algo.uni-konstanz.de/software/mdsj/mdsj.jar'
 message('installing MDSJ to directory ',install.path)
 download.file(url=mdsjURL,destfile=file.path(install.path,'mdsj.jar'),mode='wb')
 if (file.exists(file.path(install.path,'mdsj.jar'))){
   #print non-comercial use warning
   message("MDSJ is a free Java library for Multidimensional Scaling (MDS).\n It is a free, non-graphical, self-contained, lightweight implementation of basic MDS algorithms and intended to be used both as a standalone application and as a building block in Java based data analysis and visualization software. \n\n CITATION: Algorithmics Group. MDSJ: Java Library for Multidimensional Scaling (Version 0.2). Available at http://algo.uni-konstanz.de/software/mdsj/. University of Konstanz, 2009. \n\n USE RESTRICTIONS: Creative Commons License 'by-nc-sa' 3.0.\n" )
 }
 
}

check.graphviz <-function(){
  if (.Platform$OS.type == "unix" | .Platform$OS.type == "windows"){
    if (Sys.which("neato")!=''){
      return(TRUE)
    } else {
      warning("The Graphviz neato utility does not appear to be installed on this system. See ?install.graphviz for more information")
    }
  } else {
    warning(paste("not sure how to check for Graphviz on ",.Platform$OS.type,"implement me"))
    
  }
  return(FALSE)
}

install.graphviz <-function(){
  message("Please visit the Graphviz website and install the libraries on your machine: http://www.graphviz.org/Download.php  See ?install.graphviz for more information")
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
    message("To export movies, please install ffmpeg for windows from http://ffmpeg.zeranoe.com/builds/\n type ?install.ffmpeg for more detailed instructions")
    browseURL("http://ffmpeg.zeranoe.com/builds/")
    
  } else if (.Platform$OS.type == "unix") {
    message("To export movies, please install ffmpeg using your system's package manager.\n Type ?install.ffmpeg for more detailed instructions.")
  } else {
    message("To determine if ffmpeg can be installed on your system, please visit http://ffmpeg.org")
    browseURL("http://ffmpeg.org")
  }
}