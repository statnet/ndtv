\name{layout.center}
\alias{layout.center}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Functions to center the coordinates of a network plot within a window.
}
\description{
The \code{layout.center} function takes a matrix of coordinates and an x- and y-coordinate range and centers the input coordinates within the range.
}
\usage{
layout.center(coords, xlim, ylim)
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

}
\details{
TODO: add barycenter function, and center on vertex function
}
\value{
 The input two column numeric matrix of coordinates with positions transformed.
}

\author{
skyebend
}
