# Strategic Incentives and Efficiency: A Non-Cooperative Approach to DEA

This repository contains the source code, data, and replication files for the paper:  
> **"Strategic Incentives and Efficiency: A Non-Cooperative Approach to Data Envelopment Analysis"**

### Authors

* **Juan Aparicio** – *Center of Operations Research, Miguel Hernández University of Elche (UMH)*.
* **Victor J. España** – *Center of Operations Research, Miguel Hernández University of Elche (UMH)*.
* **Juan Carlos Gonçalves-Dosantos** – *Center of Operations Research, Miguel Hernández University of Elche (UMH)*.

---

## Abstract

In competitive environments, organizations face increasing pressure to optimize operational efficiency while balancing limited resources and strategic objectives. This paper presents a novel incentive-based framework that integrates Data Envelopment Analysis (DEA) with Game Theory to evaluate and enhance the efficiency of Decision-Making Units (DMUs) under non-cooperative scenarios. The proposed methodology incorporates both technical efficiency and strategic decision-making by considering Nash equilibria to model the interactions among DMUs competing for limited shared revenue.  Two incentive models are introduced: (i) a model rewarding technical efficiency and (ii) a model driven by incremental effort. To evaluate the cost of projecting each DMU onto the frontier, this paper also develops a DEA projection approach based on the cost per unit of input reduction or output increase. This versatile approach can be applied across sectors such as corporate, healthcare, education, and public administration, promoting a culture of continuous improvement and better use of resources.

### Models Implemented

The analysis compares two strategic behaviors:

1. SDTEM (Strategic DEA Target-Efficiency Model)
* **Type:** Discrete choice model.
* **Mechanism:** DMUs must reach the full efficient frontier to receive a share of the reward.
* **Implication:** It represents an "all-or-nothing" policy where only fully efficient units are rewarded.

2. SDEBM (Strategic DEA Effort-Based Model)
* **Type:** Continuous choice model.
* **Mechanism:** DMUs can choose partial improvements.
* **Implication:** Rewards are distributed proportionally to the effort exerted relative to the total effort in the system.

### Repository Structure

The project is organized into modular scripts to ensure reproducibility and maintainability:

```text
Strategic-DEA-Incentives/
│
├── data/
│   └── utilities.xlsx       # Dataset (134 US electric utilities)
│
├── R/
│   └── functions_dea.R      # Core logic: DDF estimation and Nash Equilibrium algorithms
│
├── scripts/
│   ├── 01_data_prep.R       # Data loading, parameter generation, and cost simulation
│   ├── 02_simulation.R      # Main simulation loop over the revenue range (r)
│   └── 03_plotting.R        # Visualization of results (Projection and Benefit plots)
│
├── output/
│   ├── proj_plot.png        # Figure: Number of projected DMUs (SDTEM vs SDEBM)
│   └── benef_plot.png       # Figure: Average benefit per DMU
│
└── run_analysis.R           # Master script: Executes the entire pipeline
```

### Core Function Usage

The analysis relies on a main function, `incentive_projection()`, located in `R/analysis_functions.R`. You can use this function to calculate equilibra for your own scenarios.

```r
incentive_projection (
  data, cost, dirs, x, y, proj = "partial", r, tol = 1e-12, verbose = FALSE
)
```

#### Arguments

* `data`: `data.frame` containing the observed inputs and outputs.
* `cost`: `data.frame` containing  the marginal costs for the projection.
* `dirs`: `data.frame` that defines the direction vector for each DMU.
* `x`: `numeric` containing column indices corresponding to inputs in data.
* `y`: `numeric` containing column indices corresponding to outputs in data.
* `project`: `"full"` to implement SDTEM (discrete) or `"partial"` to implement the SDEBM (continuous).
* `r`: `numeric` that defines the revenue (reward pool) available for distribution.
* `tol`: Tolerance for numerical comparisons (default: `1e-12`).
* `verbose`: `logical`. If `TRUE`, prints a formatted summary table of the equilibrium to the console.

```r
incentive_projection (
  data = utilities, 
  cost = costs, 
  dirs = dirs,
  x = c(6, 7, 9),
  y = 8,
  proj = "full", 
  r = 1300, 
  verbose = TRUE
)
```

### Prerequisites

The analysis is performed using `R`. Ensure you have the following packages installed:
`Rinstall.packages(c("lpSolveAPI", "readxl", "dplyr", "tidyr", "ggplot2", "patchwork", "knitr"))`

### Usage

To reproduce the full analysis, figures, and the specific case study ($r=1300$):

1. **Clone** this repository.
2. **Open** the project in RStudio (opening the `.Rproj` file is recommended).
3. **Run** the master script `run_analysis.R`:

```r
source("run_analysis.R")
```

### License

This project is licensed under the MIT License - see the LICENSE file for details.
