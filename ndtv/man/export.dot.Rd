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
export.dot(x, file = "", coords = NULL, all.dyads = FALSE, 
            vert.attrs = NULL, edge.attrs = NULL)
}

\arguments{
  \item{x}{ The network object to be exported
}
  \item{file}{ The file name where network should be saved
}
  \item{coords}{ Optional node coordinates to include
}
  \item{all.dyads}{ FALSE, a numeric value, or a symetric matrix of distances providing the desired lengths for all dyads..  If numeric,  entries are written out for all possible dyads in the network, and the numeric value will be used to fill in the values for all the dyads in the matrix not linked by an edge (see \code{default.dist} param to \code{\link{layout.distance}}). This is necessary for some uses cases, but will generate dramatically larger files and slower performance. For the matrix and numeric cases, the values will be written as Graphviz 'len' edge attributes, and the values of \code{edge.attrs} will be ignored.  
}
  \item{vert.attrs}{optional character vector listing the names of any vertex attributes of the network that should be included as attributes of the nodes in the Graphviz dot file. (e.g. 'label', 'width')}
  \item{edge.attrs}{optional character vector listing the names of any edge attributes of the network that should be included as attributes of edges in the Graphviz dot file. (e.g. 'weight','penwidth')}
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
This is still a partial implementation focusing on edges, edge wights, and node coordinates in order to pass the information to graphViz to use it as an external layout engine rather than a renderer. 
}


\examples{
 library(network)
 net <- network.initialize(5)
 net[1,] <-1
 net[2,3] <-2
 export.dot(net,file="testNet.dot")
}

\keyword{ IO }
