# ==============================================================================
# Title:        DEA & Game Theory Functions
# Description:  Core functions for Directional Distance Function (DDF) and 
#               Incentive Projection (Nash Equilibrium calculation).
# ==============================================================================

# ==============================================================================
# 1. Directional Distance Function (DDF)
# ==============================================================================

# Computes efficiency scores using the Directional Distance Function (DDF)
#
# Arguments:
#   tech_xmat : Matrix of inputs defining the technology (reference set).
#   tech_ymat : Matrix of outputs defining the technology (reference set).
#   eval_xmat : Matrix of inputs for the DMUs to be evaluated.
#   eval_ymat : Matrix of outputs for the DMUs to be evaluated.
#   direction : Matrix of direction vectors (g_x, g_y) for each DMU.
#   convexity : Logical; if TRUE, imposes convexity (VRS).
#   returns   : Character; "variable" (VRS) or "constant" (CRS) returns to scale.

ddf <- function (
    tech_xmat, tech_ymat, eval_xmat, eval_ymat, 
    direction, convexity, returns
    ) {
  
  # Number of DMUs in the technology and evaluation set
  tech_dmu <- nrow(tech_xmat)
  eval_dmu <- nrow(eval_xmat)
  
  # Initialize vector of scores
  scores <- matrix(nrow = eval_dmu, ncol = 1)
  
  # Number of inputs and outputs
  nX <- ncol(tech_xmat)
  nY <- ncol(tech_ymat)
  
  for (d in 1:eval_dmu) {
    
    # Objective function setup
    objVal <- matrix(ncol = 1 + tech_dmu, nrow = 1)
    objVal[1] <- 1
    
    # Initialize LP structure
    lps <- make.lp(nrow = 0, ncol = 1 + tech_dmu)
    lp.control(lps, sense = 'max')
    set.objfn(lps, objVal)
    
    # Direction vector for the specific DMU
    G_x <- as.matrix(direction[d, 1:nX], nrow = 1)
    G_y <- as.matrix(direction[d, (nX + 1):(nX + nY)], nrow = 1)
    
    # Input constraints
    for (xi in 1:nX) {
      add.constraint(
        lps, 
        xt = c(- G_x[, xi], tech_xmat[, xi]), 
        type = "<=", 
        rhs = eval_xmat[d, xi]
      )
    }
    
    # Output constraints
    for (yi in 1:nY) {
      add.constraint(
        lps, 
        xt = c(- G_y[, yi], tech_ymat[, yi]), 
        type = ">=", 
        rhs = eval_ymat[d, yi]
      )
    }
    
    # Returns to scale and convexity constraints
    if (returns == "variable") {
      if (convexity) {
        add.constraint(lprec = lps, xt = c(0, rep(1, tech_dmu)), type = "=", rhs = 1)
      } else {
        add.constraint(lprec = lps, xt = c(0, rep(1, tech_dmu)), type = "=", rhs = 1)
        set.type(lps, columns = 1:tech_dmu + 1, type = c("binary"))
      }
    }
    
    set.bounds(lps, lower = c(- Inf, rep(0, tech_dmu)))
    
    solve(lps)
    scores[d, ] <- get.objective(lps)
    
  }
  
  return(scores)
  
}

# ==============================================================================
# 2. Incentive Projection Function
# ==============================================================================

# Computes Nash Equilibria under two models:
#   - "full": Discrete choice (All-or-nothing efficiency).
#   - "partial": Continuous choice (Effort-based efficiency).
#
# Arguments:
#   data    : Data frame containing the observed inputs and outputs.
#   cost    : Data frame containing marginal costs (c_beta) for the projection.
#   dirs    : Data frame defining the direction vector (g) for each DMU.
#   x       : Vector of column indices corresponding to inputs.
#   y       : Vector of column indices corresponding to outputs.
#   proj    : Character; "full" for the discrete model, "partial" for the continuous model.
#   r       : Numeric; total revenue (reward pool) available for distribution.
#   tol     : Numeric; tolerance for numerical comparisons (default: 1e-12).
#   verbose : Logical; if TRUE, prints detailed equilibrium tables to console.

incentive_projection <- function (
    data, cost, dirs, x, y, 
    proj = "partial", r, 
    tol = 1e-12, verbose = FALSE
    ) {
  
  # Prepare data subsets
  data <- data[, c(x, y)]
  dirs <- dirs[, names(data)]
  
  x <- 1:(ncol(data) - length(y))
  y <- (length(x) + 1):ncol(data)
  
  nX <- length(x)
  nY <- length(y)
  
  # 1. Compute efficiency scores (DDF)
  scores <- ddf (
    tech_xmat = as.matrix(data[, x]),
    tech_ymat = as.matrix(data[, y]),
    eval_xmat = as.matrix(data[, x]),
    eval_ymat = as.matrix(data[, y]),
    direction = dirs,
    convexity = TRUE,
    returns = "variable"
  )[, 1]
  
  # 2. Calculate investment costs based on slacks
  proj_x <- data[, x] + scores * dirs[1:nX]
  proj_y <- data[, y] + scores * dirs[(1 + nX):(nX + nY)]
  
  if (length(names(cost)) == 1 & names(cost) == "cost_beta") {
    k <- scores * cost$cost_beta
  } else {
    slacks <- cbind(data[, x] - proj_x, proj_y - data[, y])
    k <- rowSums(slacks * cost)
  }
  
  # Filter inefficient DMUs
  ineff_idx <- which(k >= tol)
  scores_ineff <- scores[ineff_idx]
  k_ineff <- k[ineff_idx]
  U <- length(ineff_idx)
  
  # 3. Game Theory Logic
  
  if (proj == "full") {
    
    # --- Efficiency Model (Discrete) ---
    
    cost_order <- order(k_ineff)
    ki_sorted <- k_ineff[cost_order]
    
    if (ki_sorted[1] + tol >= r) strategy <- list(integer(0))
    
    # Identify plausible coalition sizes (m)
    plausible_m <- which (
      sapply(seq_len(U), function(m) {
        (r / m + tol >= ki_sorted[m]) && 
          (m == U || r / (m + 1) - tol <= ki_sorted[m + 1])
      })
    )
    
    strategy <- lapply(plausible_m, function(m) {
      A <- which(ki_sorted <= r / m + tol)
      B <- if (m < U) which(ki_sorted <= r / (m + 1) + tol) else integer(0)
      a <- length(A)
      b <- length(B)
      
      if (a < m || b > m) return(NULL)
      
      # Helper to construct results
      construct_res <- function(indices) {
        idx_ord <- cost_order[indices]
        ind_eff <- scores_ineff[idx_ord] / scores_ineff[idx_ord] 
        tot_eff <- sum(ind_eff)
        
        list (
          s = scores_ineff[idx_ord],
          index = ineff_idx[idx_ord],
          score = scores_ineff[idx_ord], 
          costs = k_ineff[idx_ord],
          benef = (ind_eff * r / tot_eff) - k_ineff[idx_ord]
        )
      }
      
      if (a == m) return(list(construct_res(A)))
      if (b == m) return(list(construct_res(B)))
      
      # Combinatorial check for C = A \ B
      Cset <- setdiff(A, B)
      cmbs <- combn(Cset, m - b, simplify = FALSE)
      lapply(cmbs, function(ci) construct_res(sort(c(B, ci))))
    })
    
  } else if (proj == "partial") {
    
    # --- Effort Model (Continuous) ---
    
    lo <- 0
    hi <- U
    
    # Bisection method to find optimal participation mass S
    repeat {
      S <- 0.5 * (lo + hi)
      xi <- pmax(0, pmin(scores_ineff, scores_ineff * (S - (k_ineff / r) * S ^ 2)))
      total <- sum(xi / scores_ineff)
      
      if (total > S) lo <- S else hi <- S 
      if (hi - lo < tol) break
    }
    
    Sstar <- 0.5 * (lo + hi)
    s <- pmax(0, pmin(scores_ineff, scores_ineff * (Sstar - (k_ineff / r) * Sstar ^ 2)))
    
    s_no <- s != 0
    ind_effort <- s[s_no] / scores_ineff[s_no]
    tot_effort <- sum(ind_effort)
    cost_order <- order(k_ineff[s_no])
    
    strategy <- list (
      s = s[s_no][cost_order],
      index = ineff_idx[s_no][cost_order],
      score = scores_ineff[s_no][cost_order], 
      costs = s[s_no][cost_order] / scores_ineff[s_no][cost_order] * k_ineff[s_no][cost_order],
      benef = c((ind_effort * r / tot_effort) - ind_effort * k_ineff[s_no])[cost_order]
    )
    
  } else {
    stop("Argument 'proj' must be either 'full' or 'partial'.")
  }
  
  # Flatten structure if necessary
  final_strategy <- if (proj == "full") strategy[[1]] else list(strategy)
  
  # 4. Pretty Printing (Verbose Mode)
  
  if (verbose) {
    
    cat("\n")
    cat(rep("#", 70), sep = "")
    cat(paste0("\n#  RESULTS FOR REVENUE r = ", r, " (Model: ", toupper(proj), ")\n"))
    cat(rep("#", 70), sep = "")
    
    for (i in seq_along(final_strategy)) {
      eq <- final_strategy[[i]]
      
      # Calculate metrics
      pct_eliminated <- round((eq$s / eq$score) * 100, 2)
      
      # Robust revenue calculation (Cost + Net Benefit) ensures data consistency
      calc_revenue <- round(eq$costs + eq$benef, 2)
      
      df <- data.frame (
        DMU = eq$index,
        Ineff_Score = eq$score,
        Effort = eq$s,
        Ineff_Elim = pct_eliminated,
        Revenue = round(calc_revenue, 2), 
        Cost = round(eq$costs, 2),
        Net_Benefit = round(eq$benef, 2)
      )
      
      # Sort by Net Benefit descending
      df <- df[order(df$Net_Benefit, decreasing = TRUE), ]
      
      cat("\n")
      cat(rep("-", 60), sep = "")
      cat(paste0("\n  EQUILIBRIUM ", i, " (Participants: ", length(eq$index), ")\n"))
      cat(rep("-", 60), sep = "")
      cat("\n")
      
      cat(sprintf("Total Revenue Distributed: %8.2f\n", sum(df$Revenue)))
      cat(sprintf("Total Investment Cost:     %8.2f\n", sum(df$Cost)))
      cat(sprintf("Total Net Benefit:         %8.2f\n\n", sum(df$Net_Benefit)))
      
      print(knitr::kable(df, digits = 3, align = "c"))
      cat("\n")
    }
  }
  
  return(final_strategy)
  
}
