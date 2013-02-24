# We'll use the deSolve package for our integration engine
library(deSolve)
# define the solver that we want to use
solver <- ode

# Header element describing this model
headerText  <- "Bimolecular Binding Model"

# Footer with some extra text.  (this is markdown code)
footerText  <- "[Source code](https://github.com/whitwort/bindingModel) available on github."

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
    
    # This function returns an ordered list of rate of change calculations:
    # the order should match that of the state vector
    return(list(c(
      
        dA  <- (koff * AB) - (kon * A * B)
      , dB  <- (koff * AB) - (kon * A * B)
      , dAB <- (kon * A * B) - (koff * AB)
      
    )))
    
  })
  
}

# Named vector of state variable reduce functions: signature is function(x) 
# where x is a vector of state variable values along them model time points.  
# If this is NULL no summary tab is created.
state.summary <- c(
    minimum = min
  , maximum = max
  , average = mean
  )

