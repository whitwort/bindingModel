library(shiny)
library(ggplot2)
library(reshape)

# Load model into the local environment
source("model.R", local = TRUE)

# Define server logic required to generate the plot
shinyServer(function(input, output) {
  
  #Session store is a reactive values ~list
  store              <- reactiveValues()
  store$summaryData  <- data.frame()
  
  # Capture input variables in a reactive expression
  runArgs   <- reactive({
    
    # Bind initial state and parameter inputs    
    return(list( 
        state = vapply( names(state)
                      , function(name) { input[[name]] }
                      , FUN.VALUE = numeric(1)
                      )
        
      , parameters = vapply( names(parameters)
                           , function(name) { input[[name]] }
                           , FUN.VALUE = numeric(1)
                           )
        
      ))  
  })
  
  # Run the model in a reactive expression
  runModel  <- reactive({
    
    args <- runArgs()
    
    # Run the simulation; convert result to a data.frame    
    result <- data.frame(solver(
        y     = args$state
      , times = seq(time["start"], input$time.end, by = abs(input$time.end - time["start"]) / 100)
      , func  = model
      , parms = args$parameters
    ))
    
    return(result)
    
  })
  
  # Observers: these are run agressively
  updateSummary <- observe({
    
    # Update with run updates
    args    <- runArgs()
    result  <- runModel()
    
    # Need to isolate the reactive value assignment call to avoid infinite
    # recursion (hopefully this will be fixed in the framework at somepoint)
    isolate({
      
      newRow <- nrow(store$summaryData) + 1
      
      # Capture result state/parameter variable
      store$summaryData[newRow, input$summaryX] <- c(args$state, args$parameters)[input$summaryX]
      
      # Capture summary result
      store$summaryData[newRow, input$summaryY] <- state.summary[[input$summaryY]](result)
      
    })
    
  })
  
  clearSummary <- observe({
    
    # Run when summary selection changes
    summaryX <- input$summaryX
    summaryY <- input$summaryY
    
    # Reset summary table; isolate to avoid recursion
    isolate({ store$summaryData <- data.frame() })
    
  })
  
  # Simulation plot
  output$modelPlot <- renderPlot({
    
    p <- ggplot(melt(runModel(), id = "time"))              +
          geom_line( aes(time, value, colour = variable) )  +
          ylab("[variable]")                                +
          ylim(0, input$ymax)
    
    print(p)
    
  })
  
  # Summary plot
  output$summaryPlot <- renderPlot({
    
    p <- ggplot( data.frame( x = store$summaryData[[input$summaryX]]
                           , y = store$summaryData[[input$summaryY]]
                           )
               , aes(x, y) 
               )                        +
          geom_point()                  +
          geom_line( colour = "green" ) +
          ylab(input$summaryY)          +
          xlab(input$summaryX)
    
    print(p)
    
  })
  
  # Summary download link
  output$downloadSummaryData <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      }
    , content = function(con) {
        write.csv(store$summaryData, con)
      }
    )
  
})