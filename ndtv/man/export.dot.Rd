%  File man/export.dot.Rd in package ndtv, part of the Statnet suite
%  of packages for network analysis, http://statnet.org .
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) at
%  http://statnet.org/attribution
%
%  Copyright 2012-2013 Statnet Commons
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{export.dot}
\alias{export.dot}
\title{
Export a network file as Graphviz .dot formatted text file. 
}
\description{
A crude exporter for saving out a network in the Graphviz .dot format. \url{http://graphviz.org/content/dot-language}
}
\usage{
export.dot(x, file = "", coords = NULL, all.dyads = FALSE)
}

\arguments{
  \item{x}{ The network object to be exported
}
  \item{file}{ The file name where network should be saved
}
  \item{coords}{ Optional node coordinates to include
}
  \item{all.dyads}{ If true, entries are written out for all possible dyads in the network, necessary for some use cases. 
}
}
\details{
A crude exporter for saving out a network in the Graphviz .dot format. \url{http://graphviz.org/content/dot-language}
}
\value{
Returns nothing but creates a file in .dot format: \url{http://graphviz.org/content/dot-language}
}
\references{
\url{http://graphviz.org/content/dot-language}
}
\author{
Skye Bender-deMoll
}
\note{
This is still a partial implementation focusing on edges, edgewights, and node coordinates in order to pass the information to graphViz to use it as an external layout engine rather than a renderer. 
}


\examples{
 library(network)
 net <- network.initialize(5)
 net[1,] <-1
 net[2,3] <-2
 export.dot(net,file="testNet.dot")
}

\keyword{ IO }
