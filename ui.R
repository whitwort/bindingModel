library(shiny)
library(ggvis)
library(plyr)
library(markdown)

# Load model into the local environment
source("model.R", local = TRUE)

# Build an input UI from the model
stateNames <- names(state)
names(stateNames) <- stateFormat(stateNames)

modelInputs <- list(
  
    # Sidebar header text
    helpText(HTML(markdownToHTML(text = sidebarHeader, fragment.only = TRUE)))
    
    # state input boxes
  , lapply( names(state)
          , function(name) { 
              numericInput( name
                          , stateFormat(name)
                          , state[name]
                          , step = state[name] / 2
                          )
          })
    
    # parameter input boxes
  , lapply( names(parameters)
          , function(name) {
              numericInput( name
                          , parameterFormat(name)
                          , parameters[name]
                          , step = parameters[name] / 2
                          )
          })
    
  # Y-axis scaling
#   , sliderInput( 'yMax'
#                , 'Y-axis scale'
#                , min = min(state)
#                , max = max(state)
#                , value = state[['A']]
#                )
  , selectInput( 'yScale'
               , "Scale y-axis to"
               , choices = stateNames
               )
  , br()
  
  # Time scale adjustment
  , sliderInput( "time.end" 
               , "Time scale"
               , min = time["end"] * 0.1
               , max = time["end"] * 5
               , value = time["end"]
               , step = time["end"] * 0.1
               )

  , br()
  
    # Sidebar footer text
  , helpText(HTML(markdownToHTML(text = sidebarFooter, fragment.only = TRUE)))
    
  )

# Define UI layout for the application
shinyUI(pageWithSidebar(
  
    # Application title
    headerPanel(headerText)
  
    # Sidebar with a slider input for state and parameter variables
  , splat(sidebarPanel)(modelInputs)
  
    # Output panel
  , mainPanel(
    
    tabsetPanel(
        
        tabPanel( "Simulation"
                , ggvis_output("modelPlot")
                , uiOutput("modelPlotUI")
                )
      
      , tabPanel( "Summary"
                  
                  # TODO implement a less ugly horizontal well 
                , wellPanel( class = "well container-fluid"
                           , div( class = "row-fluid"
                                , div( class = "span5"
                                     , selectInput( "summaryY"
                                                  , "Summarize:"
                                                  , choices = names(state.summary)
                                                  )
                                )
                                , div( class = "span5"
                                     , selectInput("summaryX"
                                                  , "As a function of:"
                                                  , choices = c( stateFormat(names(state))
                                                               , parameterFormat(names(parameters))
                                                               )
                                                  )
                                     )
                                )
                           )
                 
                 , ggvis_output('summaryPlot')
                 , uiOutput('summaryPlotUI')
#                  , plotOutput('summaryPlot')
                 , actionButton("resetSummary", "Clear data")
                 , downloadButton('downloadSummaryData', 'Download Data')
                 )
       )
    )
))
