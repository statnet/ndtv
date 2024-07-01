%  File man/filmstrip.Rd in package ndtv, part of the Statnet suite
%  of packages for network analysis, http://statnet.org .
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) at
%  http://statnet.org/attribution
%
%  Copyright 2012-2016 Statnet Commons
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{filmstrip}
\alias{filmstrip}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a `small multiples` plot of a networkDynamic object.
}
\description{
Plots several frames of a network animation of a \code{networkDynamic} object in a single static image as a way to provide a quick visual summary of the dynamics of the network.
}
\usage{
filmstrip(nd, frames = 9, slice.par, render.par, mfrow, verbose = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nd}{
networkDynamic object to be plotted
}
  \item{frames}{
integer number of frames to extract and render
}
  \item{slice.par}{
optional list of parameters to control binning of network, overrides \code{frames} argument.  See \code{\link{compute.animation}}
}
  \item{render.par}{
optional list of parameters to control rendering of network. See \code{\link{render.animation}}
}
  \item{mfrow}{
   optional two-element numeric vector giving the number of rows and columns for the layout grid. See \code{\link{par}}.
}
  \item{verbose}{
    boolean,(defaults to FALSE) verbose argument to be passed to \code{\link{compute.animation}}\code{\link{render.animation}}/
  }
  \item{\dots}{
additional arguments to be passed to \code{\link[network]{plot.network}} via \code{\link{render.animation}}
}
}
\details{
If the \code{\link[networkDynamic]{networkDynamic}} object does not already have animation coordinates, calls \code{\link{compute.animation}} to calculate coordinates for the appropriate number of frames.  The \code{frames} argument determines the number of evenly-spaced network slices to be rendered by \code{\link{render.animation}} (with the normal plot recording disabled) as images on the final plot grid.  Note that if the layout has coordinates pre-computed by \code{compute.animation}, the slices selected by the \code{frames} argument may not align exactly with the previously compute slices.  Passing in a \code{slice.par} argument will overide \code{frames} to determine exactly which slices will be rendered. 

The layout of plot grid can be changed via the \code{mfrow} argument. 


}
\value{
Generates plots on the active graphics device.
}

\author{
skyebend
}
\note{
This is a DRAFT version of the function.  
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also \code{\link{compute.animation}}, \code{\link{render.animation}}.
}
\examples{
data(stergm.sim.1)
filmstrip(stergm.sim.1,displaylabels=FALSE)
# print an overall title for the main plot
title('stergm.sim.1 at 9 time points')

# adjust margins of individual plots to make more room
par(mar=c(1,1,1,1))
filmstrip(stergm.sim.1)
}

