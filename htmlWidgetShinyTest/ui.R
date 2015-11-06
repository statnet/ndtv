library(shiny)

# Define UI for application that renders static ndtv-d3 view of networks
shinyUI(fluidPage(
  
  # Application title
  titlePanel("ndtv shinyWidget test"),
  
  # Sidebar with a selector for picking which network to render
  sidebarLayout(
    sidebarPanel(
      
    p('This is a test of the ndtv-d3 htmlWidget from the ndtv package. Should be able to play, pause, scrub timeline, zoom in (mousewheel), pan (drag), show tooltips (click), and hilight neighbors (double click) '),
    selectInput('networkSelection', 'Select  example network:', c('short.stergm.sim','toy_epi_sim'))
    ),
    
    # Show the interactive network plot
    mainPanel(
      h2('ndtv-d3 interactive network animation'),
      tags$style(HTML(".tooltip {opacity: 1}")), # stop boostrap css from messing up the tooltip in the widget
      ndtv:::ndtvAnimationWidgetOutput("netPlot")
    )
  )
))
