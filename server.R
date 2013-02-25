library(shiny)
library(ggplot2)
library(reshape)

# Load model into the local environment
source("model.R", local = TRUE)

# Define server logic required to generate the plot
shinyServer(function(input, output) {
  
  # Per-session variable that holds summary data
  summaryData <- data.frame()
  
  # Run the model in a reactive expression
  modelResult <- reactive({
    
    userState <- vapply(names(state), 
                        function(name) { input[[name]] },
                        FUN.VALUE = numeric(1)
    )
    
    userParameters <- vapply(names(parameters),
                             function(name) { input[[name]] },
                             FUN.VALUE = numeric(1)
    )
    
    result <- solver(
        y     = userState
      , times = seq(time["start"], input$time.end, by = abs(input$time.end - time["start"]) / 100)
      , func  = model
      , parms = userParameters
    )
    
    # Update the summary data
    
    
    return(data.frame(result))
  })
  
  # Should be called when the tracked summary variables changes
  clearSummary <- reactive({
    variables         <- list(NULL, NULL)
    names(variables)  <- c(input$summaryX, input$summaryY)
    summaryData       <- splat(data.frame)(variables)
  })
  
  # Simulation plot
  output$modelPlot <- renderPlot({
    
    p <- ggplot(melt(modelResult(), id = "time"))           +
          geom_line( aes(time, value, colour = variable) )  +
          ylab("[variable]")                                +
          ylim(0, input$ymax)
    
    print(p)
  })
  
  # Summary download link
  output$downloadSummaryData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(summaryData, con)
    }
  )
  
})