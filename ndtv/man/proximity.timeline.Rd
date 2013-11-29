%  File man/proximity.timeline.Rd in package ndtv, part of the Statnet suite
%  of packages for network analysis, http://statnet.org .
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) at
%  http://statnet.org/attribution
%
%  Copyright 2012-2013 Statnet Commons
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{proximity.timeline}
\alias{proximity.timeline}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot a chart of a networkDynamic object in which vertices trace out paths in time, positioned vertically so that their proximity corresponds to their relative geodesic distance at the sampled time points. 
}
\description{
This a DRAFT version of the function, parameters are likely to change. Creates a 'phase plot' of vertex proximities against time, with the ability to size and color the lines corresponding to each vertex with arguments similar to \code{\link{plot.network}}
}
\usage{
proximity.timeline(nd,  start = NULL, end = NULL, time.increment = NULL,
  onsets = NULL, termini = NULL, default.dist = NULL, vertex.col = "#55555555", 
  labels.at = NULL, vertex.cex = 2, splines = -0.2, grid = TRUE, 
  mode=c('isoMDS','sammon','cmdscale'),
  draw.inactive=c('default','ghost','no','yes'), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nd}{
a \code{networkDynamic} object to be plotted. 
}
  \item{start}{optional numeric value giving the time to start the network sampling to be passed to \code{link{get.networks}}}
  \item{end}{optional numeric value giving the time to end the network sampling to be passed to \code{link{get.networks}}}
  \item{time.increment}{optional numeric value to increment network sampling to be passed to \code{link{get.networks}}}
  \item{onsets}{optional numeric vector of sampling onset time points to be passed to \code{link{get.networks}}}
  \item{termini}{optional numeric vector of sampling terminus time points to be passed to \code{link{get.networks}}}
  \item{default.dist}{numeric default distance parameter to space apart isolates and disconnected components (see \code{\link{layout.distance}})
}
  \item{vertex.col}{
either a character color value, a vector of values of length equal to the size of the network, or the name of a vertex attribute containing color values to be used to color each of the vertices splines. Note that partially transparent colors work much better than opaque colors, as it is easier to see when lines overlap.
}
  \item{labels.at}{ numeric value or vector of values specifying the time at which labels should be plotted on the splines. 
}
  \item{vertex.cex}{either a numeric value, a vector of values of length equal to the size of the network, or the name of a vertex attribute containing numeric values to be used to scale the width of the lines for each vertex. 
}
  \item{splines}{ numeric. value controls how tightly the splines meet their control points. A value of 0 draws straight lines and sharp corners, values less than zero cause the spline to pass through the control point, values greater than zero will approximate the point. See the \code{shape} argument of \code{\link{xspline}}.
}
  \item{grid}{ logical. if \code{TRUE}, vertical lines in the background color will be drawn at the beginning of each time slice to make it easier to determine where on the splines the positions are actually set. 
}
 \item{mode}{name of MDS algorithm to be used.  Currently one of \code{\link{isoMDS}},\code{\link{sammon}},\code{\link{cmdscale}}}
 \item{draw.inactive}{options to control how vertices with inactive spells should be drawn: \code{'yes'} ignores activity spells and draws an unbroken spline for each vertex (fastest). \code{'no'} leaves gaps in the splines when vertices are inactive, \code{'ghost'} draws faint gray dotted lines under the spline so they appear in the gaps, \code{'default'} does \code{'yes'} if there are no gaps in encountered, otherwise \code{'ghost'}.}
  \item{\dots}{
arguments to be passed to \code{\link{network.collapse}} (via \code{\link{get.networks}}) to control how the network should be aggregated during slicing}
}
\details{
The passed network dynamic object is sliced up into a series of networks. It loops over the networks, converting each to a distance matrix based on geodesic path distance with \code{layout.distance}. The distances are fed into an MDS algorithm (specified by \code{mode}) that lays them out \strong{in one dimension}: essentially trying to position them along a vertical line. The sequence of 1D optimizations are arranged along a timeline, and a spline is drawn for each vertex connecting its positions at each time point.  The idea is that closely-linked clusters form bands of lines that move together through the plot.  

Currently, \code{mode='sammon'} tends to produce much equally spaced lines, making it easier to follow individual vertices, but harder to see clusters, \code{mode='isoMDS'} does a better job with clusters, but in some layouts converges too soon and just produces straight lines, \code{mode='cmdscale'} does a great job with clusters, but is highly unstable (coordinates will reshuffle dramatically on nearlly identical networks).
}
\value{
produces a plot
}
\references{
Some inspirational examples here: \url{http://skyeome.net/wordpress/?p=604}
}
\author{
skyebend@uw.edu
}
\note{
This is still very much a work in progress, the 1D optimization are not very stable, especially for cmdscale
}

\examples{
data(McFarland_cls33_10_16_96)
proximity.timeline(cls33_10_16_96,
   onsets=seq(0,45,0.5),
   termini=seq(2.5,47.5,0.5),
   vertex.cex=(cls33_10_16_96\%v\%'type'=='instructor')*4+1,
   labels.at=16)
}

