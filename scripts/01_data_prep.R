# ==============================================================================
# Step 1: Data Preparation & Parameter Generation
# ==============================================================================

# Load required libraries
if (!require("readxl")) install.packages("readxl")
if (!require("dplyr")) install.packages("dplyr")

library(readxl)
library(dplyr)

# Create output directory
dir.create("output", showWarnings = FALSE)

# Load raw data
if (!file.exists("data/utilities.xlsx")) {
  stop("Error: 'utilities.xlsx' not found in 'data/' folder.")
}
utilities <- read_excel("data/utilities.xlsx")

# Define Directions (g vector)
dirs <- data.frame (
  Circuits = 0,
  Customers = 0,
  SAIDI = rep(- mean(utilities$SAIDI), 140),
  SAIFI = rep(- mean(utilities$SAIFI), 140)
)

# Generate Marginal Costs (c_beta)
# We set the seed here to ensure costs are identical every time
set.seed(17)

utilities$c_beta <- round (
  rlnorm(140, meanlog = 12.5, sdlog = 0.1) / 1000, 
  3
)

costs <- data.frame (
  cost_beta = utilities$c_beta
)

print("   -> Data loaded and costs generated.")