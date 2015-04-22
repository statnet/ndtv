\name{render.d3movie}
\alias{render.d3movie}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Render out a web-based animation of a networkDynamic object using ndtv-d3 player app
}
\description{
Exports a self-contained HTML file including an SVG animation of the networkDynamic object and displays it in a web browser.   See \code{\link{render.animation}} for details of animation construction.
}
\usage{
render.d3movie(net, filename=tempfile(fileext = '.html'), 
                           render.par=list(tween.frames=10,
                                           show.time=TRUE,
                                           show.stats=NULL,
                                           extraPlotCmds=NULL,
                                           initial.coords=0),
                           plot.par=list(bg='white'),
                           d3.options, 
                           output.mode=c('HTML','JSON','inline'),
                           script.type=c('embedded','remoteSrc'),
                           launchBrowser=TRUE,
                           verbose=TRUE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{net}{
The network (usally \code{\link{networkDynamic}}) object to be rendered, usually containing pre-computed vertex positions as dynamic attributes cached by \code{\link{compute.animation}}
}
  \item{filename}{
The file name of the HTML or JSON file to be generated. Must end the proper file extension ('.json' for JSON '.html' for HTML) for correct browser display.
}
  \item{render.par}{
Named list of parameters to specify the behavior of the animation.
  \itemize{
  \item \code{tween.frames} the number of interpolated frames to generate between each calculated network layout (default 10). 
  \item \code{show.time} If \code{TRUE}, labels the plot with onset and terminus time for each slice. 
  \item \code{show.stats} \code{NULL}, or a string containing a formula to be passed to summary.stergm to display the network statistics for the current slice on the plot. e.g. \code{"~edges+gwesp(0,fixed=TRUE)"}
  \item \code{extraPlotCmds} NULL, or additional plot commands to draw on each frame of the animation. 
  \item \code{initial.coords} default initial coords to be assigned to vertices. Can be a two-column numeric coordinate matrix, or a numeric values to be formed into a matrix.
  }
}
  \item{plot.par}{
list of `high-level' plotting control arguments to be passed to \code{\link{par}}. e.g. \code{bg} \code{main} for background color, margins, fonts, etc. MANY OF THESE ARE NOT YET SUPPORTED BY THE NDTV-D3 PLAYER. See list in Details below.  
}
  \item{d3.options}{
   list of options to configure ndtv-d3 player app. \code{list(
                             animationDuration=800,
                             scrubDuration=0,
                             enterExitAnimationFactor=0,
                             nodeSizeFactor=0.01,
                             playControls=TRUE,
                             animateOnLoad=FALSE,
                             slider=TRUE,
                             debugFrameInfo=FALSE,
                             debugDurationControl=FALSE,
                           )} See Details below for explanations
}
  \item{output.mode}{
character string, one of \code{'HTML'},\code{'JSON'} or \code{'inline'}.  The first exports an HTML file with embedded javascript player app including the JSON data structure describing the animation. The second just exports the JSON data structure (for loading into an existing page).  The 3rd renders the HTML inside an iframe tag and supresses all other output in an attempt to make it embedable in rmarkdown documents.
}
 \item{script.type}{if \code{script.type='embedded'}, the scripts will be embedded directly in the output html page. This option is the most portable, but will require large file sizes. If \code{script.type='remoteSrc'}, only the links to the scripts will be included, so the page will require an active internet connection to play the animation. 
 }
 \item{launchBrowser}{
 if TRUE, after exporting the file R will attempt to open it in a browser
 }
  \item{verbose}{
If \code{TRUE}, update text will be printed to the console to report the status of the rendering process.
}
  \item{\dots}{
 Other parameters to control network rendering. See \code{\link{plot.network.default}}. Most parameters can be set to TEA attribute names, or specified as a function to be evaluated at each timestep.  NOT ALL PLOT PARAMS ARE IMPLEMENTED YET}
}
\details{
Animations are generated using a process nearly identical to \code{\link{render.animation}}. However, instead of using R's plotting functions and the \code{animation} library, the relevant information is cached and written into a JSON-formatted file, embedded into a web page along with \code{ndtv-d3} player, and displayed in a web browser as an interactive HTML5 SVG animation.

The \code{ndtv-d3} player app is not a fully-featured R plot device.  It only attempts to emulate the elements of plot normally used by \code{\link{plot.network}} and it understands the graphic elements and a somewhat higher level so that it will be able to handle interaction with edge and vertex objects.

However,  \code{ndtv-d3} includes several nice features to support exploring the network: 
\itemize{
\item{controller buttons for playing, pausing and stepping through the animation}
\item{time slider for jumping and 'scrubing' to parts of the movie}
\item{pan and zoom into the network using the mouse-wheel}
\item{click on vertices and edges to reveal their ids or abitrary text attached using the \code{vertex.tooltip} and \code{edge.tooltip} properties}
\item{double-click on a vertex to highlight all of the connected edges and vertices}
}

If passed a static network, by default only a single slice will be rendered and the time slider and controllers will be disabled. 

Another advantage of \code{ndtv-d3} is that it does not require installing system libraries such as \code{ffmpeg} to render out the movie. 


The coordinates for vertex postitions are read from the \code{animation.x} and \code{animation.y} TEA attributes, normally created using \code{\link{compute.animation}}

The list of currently supported \code{\link{plot}},\code{\link{plot.network}} and \code{\link{par}} elements is
\itemize{
      \item{\code{xlab}}{: label caption below the render, on the xaxis}
      \item{\code{main}}{: main headline above the render}
      \item{\code{displaylabels}}{: should vertex labels be displayed?}
      \item{\code{usearrows}}{: should arrows be drawn on edges?}
      \item{\code{bg}}{: background color (must be html compatible? need to check this)}
      \item{\code{vertex.cex}}{: vertex expansion scale factor}
      \item{\code{label}}{: labels for vertices (defaults to vertex.names)}
      \item{\code{label.col}}{: color of vertex labels}
      \item{\code{label.cex}}{: vertex label expansion scale factor}
      \item{\code{vertex.col}}{: vertex fill colors}
      \item{\code{vertex.sides}}{: number of sides for vertex polygon (shape)}
      \item{\code{vertex.rot}}{: rotation for vertex polygon}
      \item{\code{vertex.border}}{: color of vertex border stroke}
      \item{\code{vertex.lwd}}{: width of vertex border stroke}
      \item{\code{edge.lwd}}{: width of edge stroke}
      \item{\code{edge.col}}{: edge stroke color}   
}

All of the above properties can be defined as dynamic (TEA) attributes. Noteably, curved edges, edge labels, and label positioning are not yet implemented and will be ignored. The \code{main} and \code{xlab} params will not be positioned exactly as they are in \code{\link{plot}}

There are a few special plot parameters that are only supported by \code{render.d3move}:
\itemize{
\item{\code{vertex.tooltip}}{ arbitrary text or html to be displayed when a vertex is clicked (default is the vertex id)}
\item{\code{edge.tooltip}}{ arbitrary text or html to be displayed when an edge is clicked (default is the edge id)}
\item{\code{vertex.css.class} properties for adding arbitrary class attributes for use in CSS styling of vertices}
\item{\code{edge.css.class} properties for adding arbitrary class attributes for use in CSS styling of edges}
\item{\code{vertex.label.css.class} properties for adding arbitrary class attributes for use in CSS styling of vertex labels}
}

ndtv-d3 has its own configuration properties passed in via the \code{d3.options} argument list, shown below with their default properties. Values which are set to \code{NULL} or omitted will be set to the ndtv-d3 player defaults.  

\itemize{
   \item{\code{animationDuration=800}}{ Duration (milliseconds) of each animation step during play or step actions}
   \item{\code{enterExitAnimationFactor=0}}{ Fraction (0-1) of total step animation time that edge enter/exit animations should take}
   \item{\code{nodeSizeFactor=0.01}}{ Sets default node (vertex) size, as a fraction of viewport size. }
   \item{\code{playControls=TRUE}}{ Show the player controls (play, pause, step, etc)}
   \item{\code{animateOnLoad=FALSE}} If true, animation will start playing as soon as page loads. 
   \item{\code{slider=TRUE}}{ Show the time slider control}
   \item{\code{margin=list(x=20,y=10)}}{ SVG margins - may be overridden when setting fixed aspect ratio }
   \item{\code{debugFrameInfo=FALSE}}{Show the slice timing info in corner}
  \item{\code{durationControl=TRUE}}{Show a control to change speed of animation under the menu in the upper right corner}
}

}
\value{
If \code{output.mode='HTML'}, a file will be generated including the necessary javascript and JSON data structures
If \code{output.mode='JSON'}, a JSON file will be generated including a section describing all of the rendered slices, and a seperate section including the entire networkDynamic object.
If \code{output.mode='inline'}, HTML code for an iframe elment suitible for embedding in markdown documents will be printed, all other output supressed.  
}
\references{
The github repository for the ndtv-d3 javascript library is at \url{https://github.com/statnet/ndtv-d3/} (which is the statnet release fork of \url{https://github.com/michalgm/ndtv-d3/})
}
\author{
skyebend@uw.edu
}
\note{
This is a very preliminary draft implementation. The animations preform poorly in the Linux Firefox browser, but are ok in Firefox on other platforms and excellent in the Chrome web browser. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also the ndtv-d3 vignette \code{vignette('ndtv-d3 animation examples',package='ndtv')}, \code{\link{render.animation}}, \code{\link{compute.animation}}.
}
\examples{
# render an interactive SVG animation of short.stergm.sim and open it in a browser
data(short.stergm.sim)
render.d3movie(short.stergm.sim)

# render a static network as interactive SVG with lots of html tooltip info
data(emon)
render.d3movie(emon[[5]],
    vertex.tooltip=paste(emon[[5]]\%v\%'vertex.names',
      emon[[5]]\%v\%'Command.Rank.Score',
      emon[[5]]\%v\%'Sponsorship',
      sep="<br>"),
    edge.tooltip=paste('Frequency:',emon[[5]]\%e\%'Frequency'),
    edge.lwd='Frequency')

\dontrun{
# this code chunk will embed in rmarkdown
```{r,results='asis'}
render.d3movie(short.stergm.sim,output.mode='inline')
```
}

}
