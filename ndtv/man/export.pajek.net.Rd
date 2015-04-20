%  File man/export.pajek.net.Rd in package ndtv, part of the Statnet suite
%  of packages for network analysis, http://statnet.org .
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) at
%  http://statnet.org/attribution
%
%  Copyright 2012-2013 Statnet Commons
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{export.pajek.net}
\alias{export.pajek.net}
\title{
Export a network file as a Pajek .net formatted text file.
}
\description{
A basic tool for exporting a network as a Pajek \url{http://pajek.imfm.si/doku.php?id=pajek} .net format text file. 
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
.net is basically an edgelist format with sections for vertices, arcs and edges. 
}
\value{
Nothing
}
\references{
\url{http://pajek.imfm.si/doku.php?id=pajek}
}
\author{
Skye Bender-deMoll
}
\note{
This is a very minimal implementation, mostly used for testing layout algorithms
}




\keyword{ IO }
