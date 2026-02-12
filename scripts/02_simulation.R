# ==============================================================================
# Step 2: Simulation Loop
# ==============================================================================

# Required libraries
if (!require("lpSolveAPI")) install.packages("lpSolveAPI")
library(lpSolveAPI)

# 1. Configuration Check
# ------------------------------------------------------------------------------
# If 'revenues_seq' is not defined (e.g., running script standalone), set default
if (!exists("revenues_seq")) {
  message("   ! 'revenues_seq' not found in environment. Using default sequence.")
  revenues_seq <- seq(100, 45000, 100)
}

# Initialize results dataframe using the external sequence
projec_r <- data.frame (
  revenues = revenues_seq,
  full_proj = NA,
  full_part_proj = NA,
  part_part_proj = NA,
  benef_full_proj = NA,
  benef_part_proj = NA
)

# Inputs and Outputs columns indices
# Inputs: 6 (Circuits), 7 (SAIDI), 9 (SAIFI)
# Outputs: 8 (Customers)
x_idx <- c(6, 7, 9)
y_idx <- 8

cat("   -> Starting loop over", nrow(projec_r), "revenue scenarios.\n")

# Initialize Progress Bar
pb <- txtProgressBar(min = 0, max = nrow(projec_r), style = 3)

# Main Loop
for (i in 1:nrow(projec_r)) {
  
  r <- projec_r[i, "revenues"]
  
  # Update Progress Bar
  setTxtProgressBar(pb, i)
  
  # Skip zero revenue
  if (r == 0) next
  
  # 1. Full Model (Discrete)
  results_full <- incentive_projection (
    data = utilities, cost = costs, dirs = dirs,
    x = x_idx, y = y_idx, 
    proj = "full", r = r, verbose = FALSE
  )
  
  # 2. Partial Model (Continuous)
  results_part <- incentive_projection (
    data = utilities, cost = costs, dirs = dirs,
    x = x_idx, y = y_idx, 
    proj = "partial", r = r, verbose = FALSE
  )
  
  # Store Results
  # Note: results_full[[1]] accesses the first equilibrium found
  
  # Count full efficiency
  projec_r[i, "full_proj"] <- sum(results_full[[1]]$s == results_full[[1]]$score)
  projec_r[i, "full_part_proj"] <- sum(results_part[[1]]$s == results_part[[1]]$score)
  
  # Count partial efficiency
  projec_r[i, "part_part_proj"] <- sum(results_part[[1]]$s != results_part[[1]]$score)
  
  # Average benefits
  projec_r[i, "benef_full_proj"] <- mean(results_full[[1]]$benef)
  projec_r[i, "benef_part_proj"] <- mean(results_part[[1]]$benef)
  
}

cat("\n   -> Simulation finished.")