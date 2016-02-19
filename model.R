# We'll use the deSolve package for our integration engine
library(deSolve)

# define the solver that we want to use
solver <- ode

# Header element describing this model
headerText    <- "Binding Model"

# Sidebar header with instructions.  (this is markdown code)
sidebarHeader <- "The simulation will update as you change the parameters below.  The summary tab records results across multiple runs of the simulation."

# Footer with some extra text.  (this is markdown code)
sidebarFooter <- "Version 0.4.  [Source code](https://github.com/whitwort/bindingModel) available on github."

# Kinetic parameters; don't duplicate names with the state vector
parameters <- c( kon   = 1000
               , koff  = 10
               )

# Initial values of state variables; don't duplicate names with the parameters vector
state <- c( A   = 0.001
          , B   = 0.01
          , AB  = 0
          )

# Time window and step size
time <- c( start = 0
         , end   = 1 
         , step  = 0.01
         )

# deSolve functional interface; t is the model time passed by the library
model <- function(t, state, parameters) {
  
  # We'll bind state and parameter variables to clean up our model code block
  with(as.list(c(state, parameters)), {
    
    # This function returns an ordered list of rate of change calculations: the
    # order should match that of the state vector
    return(list(c( dA  <- (koff * AB)   - (kon * A * B)
                 , dB  <- (koff * AB)   - (kon * A * B)
                 , dAB <- (kon * A * B) - (koff * AB)
                 )
                )
           )
  })
}

# Named vector of functions that are availabe as response variable choices on
# the summary tab.  Functions should take one argument, the results data.frame,
# with a $time variable and named variables for all of the model states.
state.summary <- c(
    "min([A])"        = function(r) { min(r$A) }
  , "min([B])"        = function(r) { min(r$B) }
  , "min([AB])"       = function(r) { min(r$AB) }
  , "max([A])"        = function(r) { max(r$A) }
  , "max([B])"        = function(r) { max(r$A) }
  , "max([AB])"       = function(r) { max(r$AB) }
  )

# Label formatters
stateFormat     <- function(name) { paste("Initial [", name, "] (mM)", sep = "") }
parameterFormat <- function(name) { paste("Rate of ", name, " (s-1)",  sep = "") }
