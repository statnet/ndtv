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
additional arguments to be passed to \code{\link{plot.network}} via \code{\link{render.animation}}
}
}
\details{
If the networkDynamic object does not already have animation coordinates, calls \code{\link{compute.animation}} to calculate coordinates for the appropriate number of frames.  The frames argument determines the number of evenly-spaced network slices to be rendered by \code{\link{render.animation}} (with the normal plot recording disabled) as squares on the final plot grid.  The layout of plot grid can be changed via the \code{mfrow} argument. 
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
filmstrip(stergm.sim.1)
}

