#  File R/ndtvAnimationWidget.R in package ndtv, part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 2016 Statnet Commons
#######################################################################

ndtvAnimationWidget <- function(out,options, width = NULL, height = NULL) {
  
  message('loading ndtv-d3 animation widget...')

  # forward options using x
  x = list(
    graphData = out,  # the network object and the rendering position data
    # the control options for ndtv-d3 need to be seperately JSON encoded or they will be mangled 
    # into a format that ndtv-d3 won't recognize
    animationOptions = jsonlite::toJSON(options,auto_unbox=TRUE,matrix='rowmajor', na = 'null',null='list')  
    
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
    package = 'ndtv',
    sizingPolicy = htmlwidgets::sizingPolicy(
      padding = 0,
      #defaultWidth = 500,
      #defaultHeight = 500
      #browser.fill = TRUE
    )
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
