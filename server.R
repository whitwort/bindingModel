library(shiny)
library(ggplot2)
library(reshape)

# Load the model and bind it's variables for runModel below
source("model.R")
# runModel <- function(input) {
#   
#   userState <- vapply(names(state), 
#                   function(name) { input[[name]] },
#                   FUN.VALUE = numeric(1)
#                   )
#   
#   userParameters <- vapply(names(parameters),
#                            function(name) {input[[name]]},
#                            FUN.VALUE = numeric(1)
#                            )
#   
#   result <- solver(
#       y     = userState
#     , times = seq(time["start"], input$time.end, by = abs(input$time.end - time["start"]) / 100)
#     , func  = model
#     , parms = userParameters
#     )
#   
#   return(result)
# }

# Define server logic required to generate the plot
shinyServer(function(input, output) {
  
  # Run the model using the function created at load time above
  modelResult <- reactive(function() {
    
    userState <- vapply(names(state), 
                        function(name) { input[[name]] },
                        FUN.VALUE = numeric(1)
    )
    
    userParameters <- vapply(names(parameters),
                             function(name) {input[[name]]},
                             FUN.VALUE = numeric(1)
    )
    
    result <- solver(
      y     = userState
      , times = seq(time["start"], input$time.end, by = abs(input$time.end - time["start"]) / 100)
      , func  = model
      , parms = userParameters
    )
    
    return(data.frame(result))
  })
  
  output$modelPlot <- reactivePlot(function() {
    
#     result <- runModel(input)
    
    p <- ggplot(melt(modelResult(), id = "time"))           +
          geom_line( aes(time, value, colour = variable) )  +
          ylab("[variable]") +
          ylim(0, input$ymax)
    
    print(p)
  })
  
  output$summary <- reactivePrint(function() {
    d <- modelResult()
    summary(d)
    
  })
  
})