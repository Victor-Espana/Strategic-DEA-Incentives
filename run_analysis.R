# ==============================================================================
# Title:        Strategic Incentives DEA - Master Execution Script
# Description:  Orchestrates data loading, simulation, plotting, and cases.
# ==============================================================================

# 1. Setup
# ------------------------------------------------------------------------------

# Load custom functions (The Core Engine)
source("R/analysis_functions.R")

# 2. Execution Pipeline
# ------------------------------------------------------------------------------

# Step 1: Data Preparation
cat("\n[1/4] Loading data and generating parameters...\n")
source("scripts/01_data_prep.R")

# Step 2: Simulation
revenues_seq <- seq(100, 45000, by = 100)

cat("\n[2/4] Running Nash Equilibrium simulation loop...\n")
source("scripts/02_simulation.R")

# Step 3: Visualization
cat("\n[3/4] Generating and saving plots...\n")
source("scripts/03_plotting.R")

# Step 4: Specific Case Study

# 1. Full Model
results_full <- incentive_projection (
  data = utilities, cost = costs, dirs = dirs,
  x = c(6, 7, 9), y = 8, 
  proj = "full", r = 1300, 
  verbose = TRUE
)

# 2. Partial Model
results_part <- incentive_projection (
  data = utilities, cost = costs, dirs = dirs,
  x = c(6, 7, 9), y = 8, 
  proj = "partial", r = 1300, 
  verbose = TRUE
)

cat("\n================================================\n")
cat("   Analysis Complete. Check 'output/' folder.   \n")
cat("================================================\n")