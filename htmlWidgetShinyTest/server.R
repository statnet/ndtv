library(shiny)
library(ndtv)
library(network)
data(short.stergm.sim)  # load datasets to use
data(toy_epi_sim)
# Define server logic required to draw a interactive plot
shinyServer(function(input, output) {
  nets<-list(short.stergm.sim=short.stergm.sim,toy_epi_sim=toy_epi_sim)
  
  
  
  output$netPlot <- ndtv:::renderNdtvAnimationWidget({
    net<-nets[[input$networkSelection]]
    render.d3movie(net, output.mode = 'htmlWidget')
  }
  )
})
