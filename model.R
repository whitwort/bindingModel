# We'll use the deSolve package for our integration engine
library(deSolve)

# Kinetic parameters
parameters <- c(
    kon   = 1000
  , koff  = 10
  )

# Initial values of state variables (represent concentrations)
state <- c(
    A   = 0.001
  , B   = 0.01
  , AB  = 0
  )

# Time window and step size
time <- c(
    start = 0
  , end   = 1
  , step  = 0.01
  )

# deSolve functional interface; t is the model time passed by the library
model <- function(t, state, parameters) {
  
  # We'll bind state and parameter variables to clean up our model code block
  with(as.list(c(state,parameters)), {
    
    # This function returns an ordered list of rate of change calculations
    return(list(c(
      
        dA  <- (koff * AB) - (kon * A * B)
      , dB  <- (koff * AB) - (kon * A * B)
      , dAB <- (kon * A * B) - (koff * AB)
      
    )))
    
  })
  
}

# define the solver that we want to use
solver <- ode

# Page elements describing this model
headerText  <- "Bimolecular Binding Model"
footerText  <- "[Source code](https://github.com/whitwort/bindingModel) available on github."

# Little dev function to run the model in an interactive session (not run by the server)
runModel <- function() {
  result <- solver(  
          y     = state
        , times = seq(time["start"], time["end"], by = time["step"])
        , func  = model
        , parms = parameters
      )
  
  print(head(result))
  print(tail(result))
  print(summary(result))
  plot(result)
  
  return(data.frame(result))
}
