library(shiny)
library(plyr)
library(markdown)

# Load the model and build the input UI from it at load time
source("model.R")
inputs <- c(
  
    # state input boxes
    lapply(names(state),
           function(name) { 
             numericInput(name, paste("Initial [", name, "]:", sep = ""), state[name]) 
           }
      ),
    
    # parameter input boxes
    lapply(names(parameters),
           function(name) { 
             numericInput(name, paste("Rate of ", name, ":", sep = ""), parameters[name]) 
           }
      ),
    
    # scale adjustments
    sliderInput("ymax", 
                "Y-axis scale:", 
                min = 0,
                max = max(state), 
                value = 0.1 * max(state)),
    
    sliderInput("time.end", 
                "Time scale:",
                min = 0,
                max = time["end"] * 10,
                value = time["end"],
                step = time["end"] * 0.1)
  )


# Define UI layout for the application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel(headerText),
  
  # Sidebar with a slider input for state and parameter variables
  splat(sidebarPanel)(inputs),
  
  # Output panel
  mainPanel(
    plotOutput("modelPlot"),
    verbatimTextOutput("summary"),
    
    # Source code reference
    HTML(markdownToHTML(text = footerText, fragment.only = TRUE))
  )
  
))