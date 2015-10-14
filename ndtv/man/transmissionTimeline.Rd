\name{transmissionTimeline}
\alias{transmissionTimeline}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plots network diffusion/transmission tree with generation time vs. clock/model time
}
\description{
Plots view of a network with positions determined by the timing and generation depth (previously calculated) in a transmission tree.  The horizontal axis is model time, and the vertical axis is the number of steps from the root of the tree.
}
\usage{
transmissionTimeline(x, time.attr, 
                     label, 
                     displaylabels = !missing(label), 
                     label.cex = 0.7, 
                     label.col = 1, 
                     vertex.col = 2, 
                     vertex.border = 1, 
                     vertex.lwd = 1, 
                     vertex.sides = 50, 
                     vertex.cex = 1, 
                     edge.col = "gray", 
                     edge.lty = 1, 
                     edge.lwd = 1, 
                     xlab = "time", 
                     ylab = "generation", 
                     ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
an object than can be coearced into a \code{\link{network}}.  The network must be a tree
}
  \item{time.attr}{
name of a vertex attribute containing the transmission/infection/diffusion time for each vertex
}
  \item{label}{
a vector of vertex labels, if desired; defaults to the vertex labels returned by network.vertex.names. If label has one element and it matches with a vertex attribute name, the value of the attribute will be used. Note that labels may be set but hidden by the displaylabels argument.
}
  \item{displaylabels}{
boolean; should vertex labels be displayed?
}
  \item{label.cex}{
character expansion factor for label text.
}
  \item{label.col}{
color for vertex labels; may be given as a vector or a vertex attribute name, if labels are to be of different colors.
}
  \item{vertex.col}{
color for vertices; may be given as a vector or a vertex attribute name, if vertices are to be of different colors.
}
  \item{vertex.border}{
border color for vertices; may be given as a vector or a vertex attribute name, if vertex borders are to be of different colors.
}
  \item{vertex.lwd}{
line width of vertex borders; may be given as a vector or a vertex attribute name, if vertex borders are to have different line widths.
}
  \item{vertex.sides}{
number of polygon sides for vertices; may be given as a vector or a vertex attribute name, if vertices are to be of different types. NOTE: only values of 3,4,and 50 (circle) are used as they are translated to \code{pch} plot symbols.
}
  \item{vertex.cex}{
expansion factor for vertices; may be given as a vector or a vertex attribute name, if vertices are to be of different sizes.
}
  \item{edge.col}{
color for edges; may be given as a vector, adjacency matrix, or edge attribute name, if edges are to be of different colors.
}
  \item{edge.lty}{
line type for edge borders; may be given as a vector, adjacency matrix, or edge attribute name, if edge borders are to have different line types.
}
  \item{edge.lwd}{
line width scale for edges;  May be given as a vector or edge attribute name, if edges are to have different line widths.
}
  \item{xlab}{
y-xais plot label
}
  \item{ylab}{
x-axis plot label
}
  \item{\dots}{
 additional arguments to \code{\link{plot}} (and par)
}
}
\details{
Many (but not all) of the graphical arguments to \code{\link{plot.network}} can be used and are expanded in the same way.  This does not currently use the \code{\link{plot.network}} code to draw the network as non-square plot aspect ratios would cause distortion of the vertices when drawn.
}
\value{
produces a plot, invisibly returns the coordinates of the plot. 
}

\author{
skyebend@uw.edu
}


\seealso{
\code{\link{plot.network}}, \code{\link{proximity.timeline}}
}
\examples{
# an edgelist describing an infection tree
el <-cbind(c(16, 13, 13,  10, 13, 16, 10, 13, 1, 10, 8, 1, 4, 4, 2, 2),
            1:16)
# a vector of infection times
infectionTimes <- c(583, 494, 634, 40, 712, 701, 224, 719,  
                   674, 0, 749, 621, 453, 665, 709, 575)
# make a network object, include the infection time                   
infTree<-network(el,vertex.attr = list(infectionTimes),
            vertex.attrnames = list('infectionTimes'))

transmissionTimeline(infTree,time.attr='infectionTimes')


}
