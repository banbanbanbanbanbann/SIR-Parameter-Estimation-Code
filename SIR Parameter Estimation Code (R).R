# --- 0. INSTALL AND LOAD REQUIRED PACKAGE ---
# If you don't have deSolve installed, uncomment the line below:
# install.packages("deSolve")
library(deSolve)

# --- 1. ENTER YOUR SURVEY DATA HERE ---
N <- 1000 

# The time points of your surveys (e.g., weeks 0, 1, 2, 3)
t_data <- c(0, 1, 2, 3)

# The counts from your surveys at Phase 1, 2, 3, and 4
S_data <- c(990, 850, 600, 300)
I_data <- c(10,  120, 250, 300)
R_data <- c(0,   30,  150, 400)


# --- 2. DEFINE THE TRUE SIR MATH MODEL ---
sir_equations <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    
    # The standard SIR differential equations
    dS <- -beta * S * I / N
    dI <- beta * S * I / N - gamma * I
    dR <- gamma * I
    
    return(list(c(dS, dI, dR)))
  })
}


# --- 3. CREATE THE "GUESS CHECKER" (Objective Function) ---
# This measures how wrong a guessed beta and gamma are compared to your data
check_guess <- function(parameters) {
  # The rates the computer is currently testing
  beta_guess <- parameters[1]
  gamma_guess <- parameters[2]
  
  # Start the simulation using your actual Phase 1 numbers
  initial_state <- c(S = S_data[1], I = I_data[1], R = R_data[1])
  
  # Run the SIR model using the guessed rates
  prediction <- ode(y = initial_state, 
                    times = t_data, 
                    func = sir_equations, 
                    parms = c(beta = beta_guess, gamma = gamma_guess))
  
  # Extract the predicted numbers
  S_pred <- prediction[, "S"]
  I_pred <- prediction[, "I"]
  R_pred <- prediction[, "R"]
  
  # Calculate the error (Sum of Squared Errors)
  error <- sum((S_data - S_pred)^2) + sum((I_data - I_pred)^2) + sum((R_data - R_pred)^2)
  
  return(error)
}


# --- 4. RUN THE OPTIMIZER TO FIND THE UNKNOWN RATES ---
# We give the computer a random starting guess
initial_guess <- c(beta = 0.5, gamma = 0.1)

# The optim() function tests combinations until the error drops to the minimum.
# We use the "L-BFGS-B" method so we can set bounds (rates can't be negative).
result <- optim(par = initial_guess, 
                fn = check_guess, 
                method = "L-BFGS-B", 
                lower = c(0, 0), 
                upper = c(5, 1))

# --- 5. PRINT THE RESULTS ---
cat("--- RESULTS ---\n")
cat(sprintf("Calculated Infection Rate (Beta):  %.4f\n", result$par[1]))
cat(sprintf("Calculated Recovery Rate (Gamma): %.4f\n", result$par[2]))