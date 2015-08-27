#' <Add Title>
#'
#' <Add Description>
#'
#' @import htmlwidgets
#'
#' @export
ndtvAnimationWidget <- function(out,options, width = NULL, height = NULL) {
  
  message('loading ndtv-d3 animation widget...')

  # forward options using x
  x = list(
    graphData = out,  # the network object and the rendering position data
    animationOptions = options  # the control options for ndtv-d3
  )
  
  # the htmlwidgets package uses non-standard arguments to toJSON command
  # have to set these back to defaults for ndtv_d3 to parse correctly
  # via hack outlined in http://www.htmlwidgets.org/develop_advanced.html
  attr(x, 'TOJSON_ARGS') <- list(matrix='rowmajor', na = 'null',auto_unbox=FALSE,null='list')
  
  # create widget
  htmlwidgets::createWidget(
    name = 'ndtvAnimationWidget',
    x,
    width = width,
    height = height,
    package = 'ndtv'
  )
}

#' Widget output function for use in Shiny
#'
#' @export
ndtvAnimationWidgetOutput <- function(outputId, width = '100%', height = '500px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'ndtvAnimationWidget', width, height, package = 'ndtv')
}

#' Widget render function for use in Shiny
#'
#' @export
renderNdtvAnimationWidget <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, ndtvAnimationWidgetOutput, env, quoted = TRUE)
}
