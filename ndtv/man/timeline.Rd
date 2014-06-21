%  part of the Statnet suite
%  of packages for network analysis, http://statnet.org .
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) at
%  http://statnet.org/attribution
%
%  Copyright 2012-2013 Statnet Commons
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{timeline}
\alias{timeline}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot a timeline for the edge and vertex spells of a network
}
\description{
Produces a `phase plot' or timeline showing the durations of the activity spells in a networkDynamic object. Spells are traced out horizontally, with all the activity for each element (vertex or edge) in a single row. 
}
\usage{
timeline(x, v = seq_len(network.size(x)), e = seq_along(x$mel), 
        plot.vertex.spells = TRUE, plot.edge.spells = TRUE,
        slice.par = NULL,
        displaylabels = TRUE, e.label, v.label, cex, 
        edge.col = rgb(0.5, 0.2, 0.2, 0.5), 
        vertex.col = rgb(0.2, 0.2, 0.5, 0.5), 
        xlab, ylab, xlim, ylim ...)
}

\arguments{
  \item{x}{
 a \code{networkDynamic} object that will have its spells plotted.
}
  \item{v}{
numeric vector of vertex ids to include
}
  \item{e}{
numeric vector of edge ids to include
}
  \item{plot.vertex.spells}{
logical, should vertex spells be plotted?
}
  \item{plot.edge.spells}{
logical, should edge spells be plotted?
}
\item{slice.par}{
(optional) `slice.par' list giving network binning parameters. If included, rectangles corresponding to each bin will to be plotted over the spells. The rectangles will be drawn with slightly darker left edge more transparent right edge to evoke the effect of a right-open interval. 
}
  \item{displaylabels}{
 logical, should labels be drawn for each spell
}
  \item{e.label}{
character vector of edge labels. Default is edge.id (TODO: should allow attribute value)
}
  \item{v.label}{
character vector of vertex labels. Default is vertex.names (TODO: should allow attribute value)
}
  \item{cex}{
 text size for labels (see \code{\link{plot.default}})
}
  \item{edge.col}{
 color to be used to draw lines for edge spells
}
  \item{vertex.col}{
color to be used to draw lines for vertex spells
}
  \item{xlab}{
x-axis label for plot
}
  \item{ylab}{
 y-axis label for plot
}
  \item{xlim}{
two-element numeric vector giving the x-range (time bounds) of the plot to show. Defaults to (non-infinite) max and min time of network
}
 \item{ylim}{ two-element numeric vector giving the y-range (effectively the number of entities) of plot to show. Defaults to an appropriate mapping of the number of entities to the availible plot size.} 
  \item{\dots}{
additional arguments to be passed to plot subroutines.  See \code{\link{plot.default}},\code{\link{lines}},\code{\link{text}}.
}
}

\details{
When the \code{v} argument is included, edges involving vertices not in \code{v} are excluded (but the reverse is not true for the \code{e} argument). Additional plotting arguments can be passed in to modify drawing.  For example, \code{lwd} to set line with, \code{lty} for line style. Vertices and edges that are never active are not included on the plot.
}
\value{
A plot is produced.
}

\author{
skyebend@uw.edu
}
\note{
not fully implemented, would be nice to be able to pass network attribute names for properties..
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also \code{\link{plot.network}}.
}
\examples{
  data(stergm.sim.1)
  timeline(stergm.sim.1)
}

