%  File man/export.pajek.net.Rd in package ndtv, part of the Statnet suite
%  of packages for network analysis, http://statnet.org .
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) at
%  http://statnet.org/attribution
%
%  Copyright 2012-2016 Statnet Commons
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{export.pajek.net}
\alias{export.pajek.net}
\title{
Export a network file as a Pajek .net formatted text file.
}
\description{
A basic tool for exporting a network as a Pajek \url{http://pajek.imfm.si/doku.php?id=pajek} .net format text file.  Does not yet encode attributes, layout information or timing info.
}
\usage{
export.pajek.net(net, filename)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{net}{a network object
}
  \item{filename}{ the file where the network object should be saved
}
}
\details{
.net is basically an edgelist format with sections for vertices, arcs and edges.  Vertex attributes for 'label', coordinates named 'x','y','z', 'color' as 'ic' (inner color), 'shape' as a shape value  will be written in the appropriate Pajek format.  An edge attribute of 'weight' will be written as the edge value, 'width' as 'w' and 'color' as 'c'.    See \code{\link[network]{read.paj}} for reading pajek files (time info supported) 
}
\value{
A file is written out containing the vertex and edge data.
}
\references{
Pajek software: \url{http://pajek.imfm.si/doku.php?id=pajek}

Pajek file format documentation: \url{http://vlado.fmf.uni-lj.si/pub/networks/pajek/svganim/1.10.7.1/PajekToSvgAnim.pdf}
}
\author{
Skye Bender-deMoll
}
\note{
This is a very minimal implementation, mostly used for testing layout algorithms.  Timing information is not yet supported.
}

\examples{
data('toy_epi_sim')
toy_epi_sim\%v\%'color'<-'blue'
export.pajek.net(toy_epi_sim,filename='toy_epi_sim.net')

# clean up file afterwards (just for testing)
file.remove('toy_epi_sim.net')
}



\keyword{ IO }
