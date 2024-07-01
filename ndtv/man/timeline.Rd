%  part of the Statnet suite
%  of packages for network analysis, http://statnet.org .
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) at
%  http://statnet.org/attribution
%
%  Copyright 2012-2016 Statnet Commons
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
        displaylabels = TRUE, e.label=TRUE, e.label.col='purple',
        edge.lwd=1,
        v.label, v.label.col='blue',
        vertex.cex=1, cex, adj=0,
        edge.col = rgb(0.5, 0.2, 0.2, 0.5), 
        vertex.col = rgb(0.2, 0.2, 0.5, 0.5), 
        xlab, ylab, xlim, ylim, ...)
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
(optional) `slice.par' list giving network binning parameters. If included, rectangles corresponding to each bin will  be plotted over the spells to indicate which spell will land in bins. The bins will be drawn with slightly darker left edge more transparent right edge to evoke the effect of a right-open interval. 
}
  \item{displaylabels}{
 logical, should labels be drawn for each spell
}
  \item{e.label}{
character vector of edge labels or edge attribute name. Default is edge.id 
}
  \item{e.label.col}{
  color name or character vector of colors for edge labels (or name of edge attribute to provide them)  
  }
  \item{v.label}{
character vector of vertex labels or vertex attribute name. Default is \code{network.vertex.names}
}
  \item{v.label.col}{
  color name or character vector of colors for vertex labels (or name of vertex attribute to provide them)  
  }
  \item{vertex.cex}{
  numeric scaling factor, vector of numeric scaling factors or attribute name.  Translated width of line (\code{lwd}) corresponding to each vertex. 
  }
  \item{edge.lwd}{
    numeric scale factor, numeric vector, or character edge attribute name providing a numeric value for the width of the lines corresponding to each edge.  Note that this does not behave exactly as \code{edge.lwd} in \code{\link[network]{plot.network}} as it does not perform scaling based on attribute values. 
  }
  \item{cex}{
  text size scaling for both vertex and edge labels (see \code{\link{plot.default}})
  }
  \item{adj}{text justification parameter (see \code{\link{par}}) for both vertex and edge labels.  Labels are positioned relative to onset of spell.}
  \item{edge.col}{
   color to be used to draw lines for edge spells, or vector of color names corresponding to edges, or name of edge attribute.
}
  \item{vertex.col}{
color to be used to draw lines for vertex spells, or vector of color names corresponding to vertices, or name of vertex attribute.
}
  \item{xlab}{
x-axis label for plot
}
  \item{ylab}{
 y-axis label for plot
}
  \item{xlim}{
two-element numeric vector giving the x-range (time bounds) of the plot to show. Defaults to (non-infinite) max and min time of network.
}
 \item{ylim}{ two-element numeric vector giving the y-range (effectively the number of entities) of plot to show. Defaults to an appropriate mapping of the number of entities to the available plot size.} 
  \item{\dots}{
additional arguments to be passed to plot subroutines.  See \code{\link{plot.default}},\code{\link{lines}},\code{\link{text}}.
}
}

\details{
When the \code{v} argument is included, edges involving vertices not in \code{v} are excluded (but the reverse is not true for the \code{e} argument).  If \code{xlim} range is provided and the spells corresponding to a vertex or an edge lie entirely outside its bounds they will not be shown. 

Many of the arguments correspond to arguments in \code{\link[network]{plot.network}} but are translated to the timeline plot context.  For example, \code{vertex.cex} actually controls the \code{lwd} (line width) of the lines corresponding to vertex spells.  The arguments are expanded using \code{\link[network]{plotArgs.network}} so that they should give the expansion behavior and attribute look up as \code{\link[network]{plot.network}}

Additional plotting arguments can be passed in to modify drawing.  For example, \code{lty} for line style. Vertices and edges that are never active are not included on the plot.
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
See also \code{\link[network]{plot.network}}.
}
\examples{
  data(stergm.sim.1)
  timeline(stergm.sim.1)
  
  # color vertices by priorates, don't show edges
  timeline(stergm.sim.1,vertex.col='priorates',plot.edge.spells=FALSE)
  
  # show only relationships among a few vertices
  timeline(stergm.sim.1,v=1:8)
  
  # zoom in on a region of time
  timeline(stergm.sim.1,xlim=c(20,40))
  
  # label vertices with numbers
  # and label edges by the tail and head vertices they link
  timeline(stergm.sim.1,xlim=c(0,5),v.label=1:network.size(stergm.sim.1),
      e.label=sapply(stergm.sim.1$mel,function(e){paste(e$inl,e$outl,sep='->')}) )
      
  # show only edge spells, hi-lite edge id 20
  set.edge.attribute(stergm.sim.1,'my_color','gray')
  set.edge.attribute(stergm.sim.1,'my_color','red',e=20)
  timeline(stergm.sim.1,edge.col='my_color',plot.vertex.spells=FALSE)
  
  # show binning over the edges
  timeline(stergm.sim.1,slice.par=list(start=0,
                                       end=100,
                                       interval=10, aggregate.dur=5,
                                       rule='latest'),
                      plot.vertex.spells=FALSE)
}

