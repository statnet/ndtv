\name{layout.center}
\alias{layout.center}
\alias{layout.normalize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Functions to center and normalize the coordinates of a network plot within a window.
}
\description{
The \code{layout.center} function takes a matrix of coordinates and an x- and y-coordinate range and centers the input coordinates within the range.

The \code{layout.normalize} function takes a matrix of coordinates and rescales them to the range (-1,1). If \code{keep.aspect.ratio=FALSE}, x- and y-coords are rescaled independently. 
}
\usage{
layout.center(coords, xlim, ylim)
layout.normalize(coords, keep.aspect.ratio = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{coords}{
two column numeric matrix of coordinates.
}
  \item{xlim}{
two element numeric vector giving min and max of x axis
}
  \item{ylim}{
  two element numeric vector giving min and max of y axis
}
 \item{keep.aspect.ratio}{
 boolean, if FALSE, x- and y-axis will be rescaled indpendently 
 }

}
\details{
These functions are used internally, but can also be called by the user when manipulating coordinates for layouts, especially when the coordinate ranges for a sequence of layouts do not match up well.  
TODO: add barycenter function, and center on vertex function
}
\value{
 The input two column numeric matrix of coordinates with positions transformed.
}

\author{
skyebend
}

\examples{
data(McFarland_cls33_10_16_96)
coords<-plot(cls33_10_16_96)

# center layout coords with 100 unit area
layout.center(coords,xlim=c(0,100),ylim=c(0,100))

# rescale layout coords to unit interval
layout.normalize(coords)
}
