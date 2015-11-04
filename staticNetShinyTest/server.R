library(shiny)
library(ndtv)
library(network)
data(emon)  # load datasets to use

# Define server logic required to draw a interactive plot
shinyServer(function(input, output) {
  
  
  output$netPlot <- ndtv:::renderNdtvAnimationWidget({
    # load the network
    net<-emon[[as.numeric(input$networkSelection)]]
    netName<-names(emon)[as.numeric(input$networkSelection)]
    # compute some stuff to be used in display
    totalStaff<-net%v%'Volunteer.Staff'+emon[[5]]%v%'Paid.Staff'
    sizeScale<-ifelse(is.na(totalStaff),1,totalStaff/100+1)

    # call the d3Movie render on a static network
    # capturing its output and passing it to the shiny app as HTML
    render.d3movie(net, 
                 main=paste("Search & Rescue org communication network for",netName),                      
                 #format HTML text for edge tool tips                           
                 vertex.tooltip=paste("<strong>",net%v%'vertex.names',"</strong><br>",
                                      "Decision Rank Score:",net%v%'Decision.Rank.Score',"<br>",
                                      "Command Rank Score:",net%v%'Command.Rank.Score',"<br>",
                                      "Formalization:",net%v%'Formalization',"<br>",
                                      "Location:",net%v%'Location',"<br>",
                                      "Sponsorship:",net%v%'Sponsorship',"<br>"),
                 vertex.cex=sizeScale,
                vertex.col=grDevices::adjustcolor(as.color(net%v%'Sponsorship'),alpha.f=0.5),
                # format HTML text for edge tool tips
                edge.tooltip=paste('Frequency:',net%e%'Frequency'),
                edge.lwd='Frequency',
                edge.col='#00000055',
                output.mode = 'htmlWidget',  # output directly as htmlwidget instead of to file
             #   script.type='remoteSrc', # link to .js files instead of including them directly
                 launchBrowser = FALSE , # don't load in a web browser
                )
    
  })
})
