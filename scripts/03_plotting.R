# ==============================================================================
# Step 3: Visualization
# ==============================================================================

# 1. Setup & Safety Check
# ------------------------------------------------------------------------------
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("patchwork")) install.packages("patchwork")
if (!require("tidyr")) install.packages("tidyr")
if (!require("dplyr")) install.packages("dplyr")

library(ggplot2)
library(patchwork)
library(tidyr)
library(dplyr)

# Check if simulation data exists. If not, stop and warn the user.
if (!exists("projec_r")) {
  stop("   ! Error: 'projec_r' not found. Please run '02_simulation.R' first.")
}

cat("   -> Generating plots from", nrow(projec_r), "simulation scenarios.\n")

# 2. Prepare Data
# ------------------------------------------------------------------------------
# Create working copy to avoid modifying the original results in memory
plot_data <- projec_r %>%
  mutate (
    partial_proj = full_part_proj + part_part_proj,
    differn_proj = partial_proj - full_part_proj
  )

# Reshape to Long Format for Main Plot
projec_long <- plot_data %>%
  select(revenues, full_proj, full_part_proj, part_part_proj, partial_proj) %>%
  pivot_longer(cols = -revenues, names_to = "Projection_Type", values_to = "Count") %>%
  mutate (
    Projection_Type = factor (
      Projection_Type,
      levels = c("full_proj", "full_part_proj", "part_part_proj", "partial_proj"),
      # UPDATED LABELS: Using acronyms for clean visualization
      labels = c (
        "SDTEM",             # Strategic DEA Target-Efficiency Model
        "SDEBM (full)",      # Strategic DEA Effort-Based Model (Full projection)
        "SDEBM (partial)",   # Strategic DEA Effort-Based Model (Partial projection)
        "SDEBM (total)"      # Strategic DEA Effort-Based Model (Total)
      )
    )
  )

# 3. Generate Plots
# ------------------------------------------------------------------------------

# Main Plot: Number of Projected DMUs
p1 <- ggplot(projec_long, aes(x = revenues, y = Count, color = Projection_Type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.5, alpha = 0.6) +
  scale_color_manual(values = c("steelblue", "darkgreen", "orange", "black")) +
  scale_x_continuous(breaks = seq(0, max(plot_data$revenues), by = 5000)) +
  scale_y_continuous(breaks = seq(0, max(projec_long$Count, na.rm = TRUE), by = 10)) +
  labs(
    x = NULL,
    y = "Number of Projected DMUs",
    color = NULL # Remove legend title
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.background = element_rect(fill = alpha("white", 0.9), color = NA),
    legend.key = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.minor = element_blank()
  )

# Difference Plot
p2 <- ggplot(plot_data, aes(x = revenues, y = differn_proj)) +
  geom_line(color = "gray30", linewidth = 1.1) +
  geom_point(color = "gray30", size = 1.5) +
  scale_x_continuous(breaks = seq(0, max(plot_data$revenues), by = 5000)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  labs(
    x = "Revenue (r)",
    y = "Diff: Partial - Full"
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

# Combine plots
final_proj_plot <- p1 / p2 +
  plot_layout(heights = c(3, 1))

# Benefits Plot
benef_long <- plot_data %>%
  select(revenues, benef_full_proj, benef_part_proj) %>%
  pivot_longer(cols = -revenues, names_to = "Model", values_to = "Benefit") %>%
  mutate(
    Model = factor(
      Model,
      levels = c("benef_full_proj", "benef_part_proj"),
      # UPDATED LABELS
      labels = c("SDTEM", "SDEBM")
    )
  )

benef_plot <- ggplot(benef_long, aes(x = revenues, y = Benefit, color = Model)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.5) +
  scale_color_manual(values = c("steelblue", "black")) +
  scale_x_continuous(breaks = seq(0, max(plot_data$revenues), by = 5000)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  labs(
    x = "Revenue (r)",
    y = "Average Benefit per DMU",
    color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.background = element_rect(fill = alpha("white", 0.9), color = NA),
    panel.grid.minor = element_blank()
  )

# 4. Save Results
# ------------------------------------------------------------------------------
# Create output dir if missing
dir.create("output", showWarnings = FALSE)

ggsave (
  file = "output/proj_plot.png", 
  plot = final_proj_plot, dpi = 600, width = 10, height = 7
)

ggsave (
  file = "output/benef_plot.png", 
  plot = benef_plot, 
  dpi = 600, width = 10, height = 6, bg = "white"
)

cat("   -> Plots saved to 'output/' folder with updated labels (SDTEM/SDEBM).\n")