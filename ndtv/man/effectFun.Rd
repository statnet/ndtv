%  File man/effectFun.Rd in package ndtv, part of the Statnet suite
%  of packages for network analysis, http://statnet.org .
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) at
%  http://statnet.org/attribution
%
%  Copyright 2012-2016 Statnet Commons
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{effectFun}
\alias{effectFun}
\alias{effects}
\alias{effect.edgeAgeColor}
\alias{effect.vertexAgeColor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
functions to manipulate graphic attributes of network for 'special effects'
}
\description{
Functions that can return appropriate graphic attributes (i.e. color interpolation) based on properties of the network (ages of edges,etc) 
}
\usage{
effectFun(name, ...)
effect.edgeAgeColor (net, onset, fade.dur, 
                     start.color = "#000000FF", end.color = "#00000000", 
                     na.color = "#CCCCCC55")
effect.vertexAgeColor (net, onset, fade.dur, start.color = "#000000FF", 
                      end.color = "#00000000", na.color = "#CCCCCC55")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{
 the short name of the effect function to be returned. i.e \code{'edgeAgeColor'} will return the \code{effect.edgeAgeColor} function
}
  \item{\dots}{
additional arguments to be passed in to effect functions
}
\item{net}{a network object to be evaluated}
\item{onset}{the time at which the network should be evaluated}
\item{fade.dur}{(effect property) numeric value giving the color duration of the interpolation}
\item{start.color}{(effect property) color name for color value to be used at start of interpolation}
\item{end.color}{(effect property) color name for color value to be used at start of interpolation}
\item{na.color}{(effect property) default color name for color value to be used for edge/vertices that are not currently active}
}
\details{
The special effects functions can be called directly for use as graphic parameters with standard network plots, or via  \code{effectFun} which will return the effect in a functional form so that it can evaluated/substituted at each time point as plot control function to \code{\link{render.animation}}

\code{effect.edgeAgeColor} calculates the edge of each edge in \code{net} at the time \code{onset} and uses the value to return a color interpolated between \code{start.color} and \code{end.color} by comparing the time of each edge to the \code{fade.dur} parameter

\code{effect.vertexAgeColor} does the same, but for vertices. 

Users can also define functions to be called in this way
}
\value{
\code{effectFun} returns the function named by its first argument, with any arguments matching in ... substituted.
}

\author{
skyebend@uw.edu
}

\examples{
library(ndtv)
data('short.stergm.sim')
# render a plot with edges colored by age at time 24
# edges labeld with age
plot(short.stergm.sim,edge.col=effect.edgeAgeColor(short.stergm.sim,
                                                   fade.dur=25,
                                                   start.color = 'red',
                                                   end.color='blue', 
                                                   onset=24),
     edge.label=edges.age.at(short.stergm.sim,24),
     edge.lwd=5)
     
\dontrun{
# render an animation where edges are colored dynamically by their age
# starting out red and fading to blue
compute.animation(short.stergm.sim,slice.par = list(start=0,
                                                   end=25,
                                                   interval=1, a
                                                   ggregate.dur=5,
                                                   rule='latest'))
render.animation(short.stergm.sim,edge.col=effectFun('edgeAgeColor',
                                                      fade.dur=5,
                                                      start.color = 'red',
                                                      end.color='blue'))
}     

}