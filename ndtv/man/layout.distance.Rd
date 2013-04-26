%  File man/layout.distance.Rd in package ndtv, part of the Statnet suite
%  of packages for network analysis, http://statnet.org .
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) at
%  http://statnet.org/attribution
%
%  Copyright 2012-2013 Statnet Commons
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{layout.distance}
\alias{layout.distance}
\title{
Provides a default way to convert a network into a set of euclidian distances suitable for MDS-style layout optimization.
}
\description{
Computes a geodesic path distance matrix for a network after symmetrizing, replacing Inf values with \code{default.dist} 
}
\usage{
layout.distance(net, default.dist = NULL,  weight.attr = NULL,
                 weight.dist = FALSE)
}
\arguments{
  \item{net}{
The network that the distance matrix should be computed for
}
  \item{default.dist}{
An (optional) value to be used to replace undefined values created by isolates and disconnected components. 
}
  \item{weight.attr}{character, (optional) the name of an edge attribute of \code{net} containing numeric values to use for edge distances.}
  
  \item{weight.dist}{logical, should the edge values given by \code{weight.attr} be interpreted as distances (larger values should place vertices farther apart) ? Default (\code{FALSE}) assumes values are similarities (larger values means stronger connection means virtices closer together ).
  }
  
}
\details{
If no \code{default.dist} is provided the value \code{sqrt(network.size(net))} will be used. If input is similarity, it will be recoded/reversed to distances by subtracting each non-zero value from the max  value of the matrix and adding the min value of the matrix. Input matrix will then be symmatrized to max value of i-j relation. 
}
\value{
A distance matrix assumed to be appropriate for the network.
}

\author{
Skye Bender-deMoll
}

\examples{
test<-network.initialize(4)
add.edges(test,tail=1:2,head=2:3)
# in adjacency matrix form
as.matrix(test)
# as matrix of geodesic distances
layout.distance(test,1.5)

}

