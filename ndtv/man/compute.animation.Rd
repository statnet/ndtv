%  File man/compute.animation.Rd in package ndtv, part of the Statnet suite
%  of packages for network analysis, http://statnet.org .
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) at
%  http://statnet.org/attribution
%
%  Copyright 2012-2016 Statnet Commons
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{compute.animation}
\alias{compute.animation}
\alias{slice.par}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute a sequence of vertex layouts over time suitable for rendering an animation. 
}
\description{
Steps through a networkDynamic object and applies layout algorithms at specified intervals, storing the calculated coordinates in the network for later use by the render.animation function. Generally the layout are done in a sequence with each using the previously calculated positions as initial seed coordinates in order to smooth out the resulting movie. Not all network layout algorithms give good results. 
}
\usage{
compute.animation(net, slice.par = NULL, animation.mode = "kamadakawai", 
                  seed.coords = NULL, layout.par = list(), 
                  default.dist = NULL, weight.attr = NULL, weight.dist=FALSE,
                  chain.direction=c('forward','reverse'),
                  verbose = TRUE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{net}{
 A networkDynamic network object describing the temporal evolution of a network.
}
  \item{slice.par}{
  A list of parameters which specify the time steps and aggregation that should be used when moving through the network. Example: 
  \code{slice.par=list(start=0,end=100,interval=1,
  aggregate.dur=1,rule='latest')}
  
  The parameters are:
  \itemize{
  \item \code{start}  The time point at which the sequence of layouts should begin
  \item \code{end}  The time point at which the sequence of layouts should finish 
  \item \code{interval}   The amount of time between successive layouts
  \item \code{aggregate.dur}   The duration of time over which the network should be aggregated to derive the network for each layout
  \item \code{rule}  The aggregation rule to be used when collapsing the network.
  }
  
  
  
  
}
  \item{animation.mode}{
The name of the network animation layout to be used. These layouts are name network.layout.animate.something but will be matched using the final part of the name.  Current useful values are:
  \itemize{
  \item \code{\link{network.layout.animate.kamadakawai}} essentially wrapper for the Kamadakawai layout included in the network package.
  \item \code{\link{network.layout.animate.MDSJ}} a wrapper to do a Stress Majorization optimized MDS layout using the Multi Dimensional Scaling for Java package. Note, due to license restrictions, this algorithm is for non-commercial use only)
  \item \code{\link{network.layout.animate.useAttribute}} applies coordinates stored in a user-generated dynamic network attribute
  \item \code{\link{network.layout.animate.Graphviz}} a wrapper for the Graphviz software library --if the library is installed on your system.
  }
}
  \item{seed.coords}{
(optional) an array of initial positions to be used for the very first layout in the sequence
}
  \item{layout.par}{
  A list of parameters to be passed to the layout algorithm.
}
  \item{default.dist}{
  The default distance to be used to separate nodes (or disconnected network components). Default to \code{sqrt(network.size(net))}. See \code{\link{layout.distance}}. 
}
  \item{weight.attr}{charater providing the name of a (possibly dynamic) numeric edge attribute defining weights for the edges in each time slice. The values \code{activity.duration} or \code{activity.count} can be used to weight edges by the duration or count of the edge's activity spells in the time slice. 
  }
  \item{weight.dist}{logical, defaults to \code{FALSE}, meaning that the edge weight values provided by \code{weight.attr} will be treated as similarities (larger values means closer). A value of \code{TRUE} means that weights should be intrepreted as distances. See \code{\link{layout.distance}} for more information. 
  
  }
  \item{chain.direction}{
  a value of \code{'forward'} indicates the chain of layouts should be computes in forward temporal order. A value \code{'reverse'} runs the chain backwards. For some layouts, reverse-chaining means that isolated vertices are more likely to have positions close to the partners they will be tied to.
  }
  \item{verbose}{
  If true, additional information about the layout process and progress will be returned to console. 
}
  \item{...}{
  possible additional arguments to be passed to sub processes
  }
}
\details{
This function is under active development so implementation and parameters will continue to change.
}
\value{
 Invisibly returns original network argument (which is also modified in-place), with the addition of a network variable \code{slice.par} storing the slice parameters used, and dynamic node attributes \code{animation.x} and \code{animation.y} storing the coordinates calculated for each time point. 
}
\references{
See docs for specific layout functions. 

Bender-deMoll, S., Morris, M. and Moody, J. (2008) Prototype Packages for Managing and Animating Longitudinal Network Data: dynamicnetwork and rSoNIA \emph{Journal of Statistical Software} 24:7.

Krivitsky P and Handcock M (2012).
\emph{Fit, Simulate and Diagnose Models for Network Evoluation based on Exponential-Family Random Graph Models}.
Version 3.0-999.  Project home page at \url{http://statnet.org}, \url{https://cran.r-project.org/package=tergm}.

Butts CT (2008). network: A Package for Managing Relational Data in R.  \emph{Journal of Statistical Software}, 24(2). \url{https://doi.org/10.18637/jss.v024.i02}. 

Skye Bender-deMoll and McFarland, Daniel A. (2006)  The Art and Science of Dynamic Network Visualization. \emph{Journal of Social Structure. Volume 7, Number 2} \url{https://www.cmu.edu/joss/content/articles/volume7/deMollMcFarland/}
}
\author{
Skye Bender-deMoll, and the statnet team.
}


\seealso{
 See also \code{\link{layout.distance}}, \code{\link{render.animation}}, \code{\link{network.layout.animate.MDSJ}},\link{ndtv}, package vignette (\code{vignette('ndtv')}) for examples.
}


