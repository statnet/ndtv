%  File man/network.layout.animate.Rd in package ndtv, part of the Statnet suite
%  of packages for network analysis, http://statnet.org .
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) at
%  http://statnet.org/attribution
%
%  Copyright 2012-2013 Statnet Commons
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{network.layout.animate.kamadakawai}
\alias{network.layout.animate.kamadakawai}
\alias{network.layout.animate.MDSJ}
\alias{network.layout.animate.useAttribute}
\title{
Sequentially-stable network layout algorithms suitable for generating network animations.
}
\description{
The \code{network.layout.animate.*} layouts are often adaptations or wrappers for existing static layout algorithms with some appropriate presets.  They all accept the coordinates of the `previous' layout as an argument so that they can try to construct a suitably smooth sequence of node positions.
}
\usage{
network.layout.animate.kamadakawai(net, dist.mat = NULL, default.dist = NULL, 
                        seed.coords = NULL, layout.par = list(),verbose=FALSE)

network.layout.animate.MDSJ(net, dist.mat = NULL, default.dist = NULL, 
                seed.coords = NULL, layout.par=list(max_iter=50),verbose=TRUE)
                
network.layout.animate.useAttribute(net, dist.mat = NULL, default.dist = NULL,
     seed.coords = NULL,  layout.par = list(x = "x", y = "y"), verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{net}{ The network (or temporal sub-network) that will be used to determine vertex positions. 
   
}
  \item{dist.mat}{ A (usually optional) matrix of distances between vertices that should be used to define node positions.  This is important to provide if network edge weights need special handling - for example to be fliped from similarities to distances, symmetrized, etc. 
}
  \item{default.dist}{ A default distance value which a layout may use to fill in for undefined dyads to space out isolates and disconnected components. 
}
  \item{seed.coords}{A two-column by n-vertex matrix of starting coordinates for the layout to use, usually the coordinates of the previous layout. 
}
  \item{layout.par}{A list of named layout parameters specific to the algorithm. 
}

  \item{verbose}{Print more information about the layout process}
}
\details{
These layouts are generally called by \code{\link{compute.animation}} on a sequence of extracted networks, with each layout fed the output of the previous layout

Usually if the dist.mat is not included, one will be calculated using the \code{\link{layout.distance}} function which will compute the geodesic path length distance between nodes after symmetrizing the network and replacing Inf values with either sqrt(network.size) or the passed in \code{default.dist}

\strong{KamadaKawai}

The KamadaKawai option provides resonably good dynamically stable layouts. It computes a symmetric geodesic distance matrix from the input network (replacing infinite values with \code{default.dist}, and seeds the initial coordinates for each slice with the results of the previous slice in an attempt to find solutions that are as close as possible to the previous positions.  However, it performs poorly on large networks and is not as stable as MDSJ. 
See \code{\link{network.layout.kamadakawai}} for more details about the implementation and parameters

\strong{MDSJ}

The MDSJ layout uses the MDSJ Java library written by Christian Pich, Algorithmics Group, Department of Computer & Information Science, University of Konstanz, Germany \url{http://www.inf.uni-konstanz.de/algo/software/mdsj/}, 2009. The library does Multidimensional Scaling (MDS) of the distance matrix using SMACOF optimization.  Because MDSJ is released under a creative commons by-nc-sa license it is not distributed with the ndtv package, but an installer is included.

When the MDSJ layout is called it checks for working Java installation, and then checks if MDSJ is installed. If not, it prompts the user and (optionally) downloads and installs MDSJ. If MDSJ is not installed, it falls back to calling the KamadaKawai layout instead.  

MDSJ is quite fast for larger networks, but relatively less efficient for smaller ones because of the overhead of \code{system} calls and Java start up for each layout.  The \code{verbose} option prints more information on the Java process.  The \code{max_iter} parameter sets the maximum of minimization steps the algorithm can try. In cases where it seems like the layout has not completely finished, this can be set higher.

\strong{useAttribute}

The \code{useAttribute} layout makes it possible to define vertex positions using a dynamic vertex attributes to provide the x and y coordinates for each time step.  The names of the attributes to be used are passed in via the \code{layout.par} argument.  For example \code{layout.par = list(x = "myX", y = "myY")} The attribute must have values defined for each time point that the network plotted. 


}
\value{
A two-column by n-vertex matrix of coordinates. 
}
\references{
Algorithmics Group. MDSJ: Java Library for Multidimensional Scaling (Version 0.2). Available at \url{http://www.inf.uni-konstanz.de/algo/software/mdsj/}. University of Konstanz, 2009.

Kamada, T. and Kawai, S. (1989). An Algorithm for Drawing General Undirected Graphs. Information Processing Letters, 31(1):7-15.

}
\author{
Skye Bender-deMoll
}
\note{
The MDSJ algorithm can only be used for non-comercial projects as it is available under the terms and conditions of the Creative Commons License "by-nc-sa" 3.0. \url{http://creativecommons.org/licenses/by-nc-sa/3.0/}
}



\seealso{
See Also \code{\link{network.layout.kamadakawai}},\code{\link{layout.distance}},\code{\link{compute.animation}}
}


