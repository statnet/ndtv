\name{ndtvAnimationWidget}
\alias{ndtvAnimationWidget}
\alias{ndtvAnimationWidgetOutput}
\alias{renderNdtvAnimationWidget}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
htmlwidget wrapper functions for including ndtv-d3 animations in shinyapps
}
\description{
Wrapper functions to provide ndtv-d3 animation as an \code{\link{htmlwidgets}} for use within an RStudio "shiny" web application.  These functions are not normally called by R users directly.  
}
\usage{
ndtvAnimationWidget(out, options, width = NULL, height = NULL)

renderNdtvAnimationWidget(expr, env = parent.frame(), quoted = FALSE) 

ndtvAnimationWidgetOutput(outputId, width = "100\%", height = "500px")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{out}{
the network animation data structure produced internally by \code{\link{render.d3movie}}
}
  \item{options}{
usually the \code{'d3.options'} from \code{\link{render.d3movie}}
}
  \item{width}{
Display width for the widget. Must be a valid CSS unit (like "100\%", "400px", "auto") or a number, which will be coerced to a string and have "px" appended.
}
  \item{height}{
Display eight for the widget. Must be a valid CSS unit (like "100\%", "400px", "auto") or a number, which will be coerced to a string and have "px" appended.
}
\item{outputId}{See \code{\link{htmlwidgets::htmlwidgets-shiny}}}
\item{name}{Name of widget to create output binding for See \code{\link{htmlwidgets::htmlwidgets-shiny}}}
\item{package}{Package containing widget ('ndtv') See \code{\link{htmlwidgets::htmlwidgets-shiny}}}
\item{expr}{An expression that generates an HTML widget. See \code{\link{htmlwidgets::htmlwidgets-shiny}}}
\item{env}{The environment in which to evaluate expr. See \code{\link{htmlwidgets::htmlwidgets-shiny}}}
\item{quoted}{Is expr a quoted expression (with quote())? This is useful if you want to save an expression in a variable. See \code{\link{htmlwidgets::htmlwidgets-shiny}}}

}
\details{
The ndtv-d3 interactive HTML5 network animation can normally be produced via \code{render.d3move(...,output.mode='htmlWidget')}. These functions are wrappers to make it possible to embed the animations as part of a 'Shiny' (\url{http://shiny.rstudio.com/}) web application.  

\code{renderNdtvAnimationWidget} should be used as the wraper for the \code{render.d3movie} call within the app's \code{server.R} file and \code{ndtvAnimationWidgetOutput} is the corresponding ui component to include  in the \code{ui.R} file. See \code{\link{htmlwidgets::htmlwidgets-shiny}}

\code{ndtvAnimationWidget} initializes the widget, usually called automatically inside \code{\link{render.d3move}} when \code{output.mode='htmlWidget'}.  

TODO: publish the example code and include links

}


\author{
skyebend@uw.edu
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{htmlwidgets}}, \code{\link{render.d3movie}}
}
