# --- 6. PLOT THE FITTED CURVES VS ACTUAL DATA ---

# Create a smooth timeline for the curves (e.g., 100 points between phase 0 and 3)
smooth_times <- seq(0, 3, by = 0.05)

# Run the simulation using the newly discovered "best" rates
fitted_model <- ode(y = c(S = S_data[1], I = I_data[1], R = R_data[1]), 
                    times = smooth_times, 
                    func = sir_equations, 
                    parms = c(beta = result$par[1], gamma = result$par[2]))

# Convert the output to a data frame for easy plotting
fitted_data <- as.data.frame(fitted_model)

# 1. Draw the smooth predicted lines from the mathematical model
matplot(fitted_data$time, fitted_data[, c("S", "I", "R")], 
        type = "l", lty = 1, lwd = 2, col = c("blue", "red", "darkgreen"),
        xlab = "Time (Phases)", ylab = "Number of People",
        main = "SIR Model Fit to Survey Data")

# 2. Add your actual survey data points on top to check the accuracy!
points(t_data, S_data, pch = 16, cex = 1.5, col = "blue")
points(t_data, I_data, pch = 16, cex = 1.5, col = "red")
points(t_data, R_data, pch = 16, cex = 1.5, col = "darkgreen")

# 3. Add a legend so you know what you are looking at
legend("topright", legend = c("Susceptible (Model)", "Infectious (Model)", "Recovered (Model)", "Actual Survey Data"),
       col = c("blue", "red", "darkgreen", "gray"), 
       lty = c(1, 1, 1, NA), pch = c(NA, NA, NA, 16), lwd = 2)
