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
This a DRAFT version of the function, parameters are likely to change. Creates a 'phase plot' chart of vertex geodesic distance proximities overtime time, with the ability to size and color the lines corresponding to each vertex with arguments similar to \code{\link{plot.network}}
}
\usage{
proximity.timeline(nd,  start = NULL, end = NULL, time.increment = NULL,
  onsets = NULL, termini = NULL, default.dist = NULL, vertex.col = "#55555555", 
  label = network.vertex.names(nd), labels.at = NULL, vertex.cex = 2, 
  splines = -0.2, grid = TRUE, 
  mode=c('isoMDS','sammon','cmdscale','gvNeato','MDSJ'),
  draw.inactive=NULL, 
  spline.style=c('default','inactive.ghost','inactive.gaps',
                'inactive.ignore','color.attribute'),
  chain.direction=c('forward','reverse'),
  verbose=TRUE, ...)
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
  \item{default.dist}{numeric default distance parameter to space apart isolates and disconnected components. Usually defaults to square root of network size (see \code{\link{layout.distance}})
}
  \item{vertex.col}{
either a character color value, a vector of values of length equal to the size of the network, or the name of a vertex attribute containing color values to be used to color each of the vertices splines. Note that partially transparent colors work much better than opaque colors, as it is easier to see when lines overlap. When used with \code{spline.style='color.attribute'}, \code{vertex.col} can be a function with a special limited set of arguments (see Details of \code{\link{render.animation}}) which will be evaluated \emph{at the onset} of each segment.
}
  \item{labels.at}{ numeric value or vector of values specifying the time(s) at which vertex labels should be plotted on the splines. If \code{NULL} (default), labels will not be drawn.
}
  \item{label}{character vector of labels for vertices or name of vertex attribute to be expanded. Default is network.vertex.names.  Labels only drawn if \code{labels.at} argument has a value.
  
  }
  \item{vertex.cex}{either a numeric value, a vector of values of length equal to the size of the network, or the name of a vertex attribute containing numeric values to be used to scale the width of the lines (\code{lwd}) for each vertex. 
}
  \item{splines}{ numeric. value controls how tightly the splines meet their control points. A value of 0 draws straight lines and sharp corners, values less than zero cause the spline to pass through the control point, values greater than zero will approximate the point. See the \code{shape} argument of \code{\link{xspline}}.
}
  \item{grid}{ logical. if \code{TRUE}, vertical lines in the background color will be drawn at the beginning of each time slice to make it easier to determine where on the splines the positions are actually set. 
}
 \item{mode}{name of MDS algorithm to be used.  Currently one of \code{\link{isoMDS}},\code{\link{sammon}},\code{\link{cmdscale}}}
 \item{draw.inactive}{DEPRECATED. see \code{spline.style}}
 \item{spline.style}{options to control how vertices with inactive spells or changing attribute values should be drawn: 
  \itemize{
    \item \code{'inactive.ignore'} ignores activity spells and draws an unbroken spline for each vertex (fastest). 
    \item \code{'inactive.gaps'} leaves gaps in the splines when vertices are inactive 
    \item \code{'inactive.ghost'} draws faint gray dotted lines under the spline so they appear in the gaps 
    \item \code{'default'} does \code{'inactive.ignore'} if there are no gaps in encountered, otherwise \code{'inactive.ghost'} 
    \item \code{'color.attribute'} uses the activity spells of the vertex color TEA (indicated by the \code{vertex.col} argument) to break the splines in to color segments -- ignoring the the vertices activity spells}
  }
 \item{chain.direction}{value of \code{'forward'} means that the slice layouts should be computed in temporal order, with each layout initialized with the coordinates from the previous. A value of \code{'reverse'} causes layouts to be computed in reverse temporal order (for some layouts, this will cause less spline crossing as vertices will tend to be closer to their final state).  }
 \item{verbose}{logical, default is TRUE, in which case status messages about the computations are printed to the console, at some speed cost}
  \item{\dots}{
arguments to be passed to \code{\link{network.collapse}} (via \code{\link{get.networks}}) to control how the network should be aggregated during slicing}
}
\details{
The passed network dynamic object is sliced up into a series of networks. It loops over the networks, converting each to a distance matrix based on geodesic path distance with \code{layout.distance}. The distances are fed into an MDS algorithm (specified by \code{mode}) that lays them out \strong{in one dimension}: essentially trying to position them along a vertical line. The sequence of 1D layouts are arranged along a timeline, and a spline is drawn for each vertex connecting its positions at each time point.  The idea is that closely-linked clusters form bands of lines that move together through the plot.  

Currently, 
\itemize{
\item \code{mode='sammon'} tends to produce much equally spaced lines, making it easier to follow individual vertices, but harder to see clusters 
\item \code{mode='isoMDS'} does a better job with clusters, but in some layouts converges too soon and just produces straight lines, 
\item \code{mode='cmdscale'} does a great job with clusters, but is highly unstable (coordinates will reshuffle dramatically on nearly identical networks). 
\item \code{mode='gvNeato'} tries to do a 1D Graphviz neato layout (experimental) \code{\link{network.layout.animate.Graphviz}}. 
\item \code{mode='MDSJ'} tries a 1D \code{\link{network.layout.animate.MDSJ}} layout.
}

For most of the layouts it is necessary to manually adjust the default dist parameter to find a value that sufficently groups together linked clusters and spaces out isolates. 


Note for RStudio users: the spline rendering seems to be much slower on RStudio's graphics device than on other graphics devices such as \code{x11()}.
}
\value{
Produces a plot with horizontal splines corresponding the vertices of the network and vertical proximities approximately proportional to geodesic distance.
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
\seealso{
See also \code{\link{timeline}} for plotting spells of vertices and edges without proximity positioning.
}

\examples{
data(McFarland_cls33_10_16_96)
proximity.timeline(cls33_10_16_96,
   onsets=seq(0,45,0.5),
   termini=seq(2.5,47.5,0.5),
   vertex.cex=(cls33_10_16_96\%v\%'type'=='instructor')*4+1,
   labels.at=16)

data(toy_epi_sim)

# render a timeline with vertices colored by infection status
proximity.timeline(toy_epi_sim,vertex.col = 'ndtvcol',
                  spline.style='color.attribute',
                  mode='sammon',default.dist=20,
                  chain.direction='reverse')
}

