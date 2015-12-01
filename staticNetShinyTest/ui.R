library(shiny)

# Define UI for application that renders static ndtv-d3 view of networks
shinyUI(fluidPage(
  
  # Application title
  titlePanel("interactive network with ndtv test"),
  
  # Sidebar with a selector for picking which network to render
  sidebarLayout(
    sidebarPanel(
      
    selectInput('networkSelection', 'Select "emon" example network:', 1:7)
    ),
    
    # Show the interactive network plot
    mainPanel(
      h2('ndtv-d3 interactive network plot'),
      p('Should display interactive vew of static network. Mouse wheel zooms, click for tooltips, drag to pan, double click to highlight neighbors.'),
      tags$style(HTML(".tooltip {opacity: 1}")), # stop boostrap css from messing up the tooltip in the widget
      ndtv::ndtvAnimationWidgetOutput("netPlot")
    )
  )
))
