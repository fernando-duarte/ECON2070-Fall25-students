# TEMPORARY MOCK VERSION FOR CI TESTING
# This creates quick placeholder SVGs instead of computing actual phase diagrams
# Revert to main branch version for production

message("*** MOCK MODE: Creating placeholder SVG files for CI testing ***")

# ---- Output Paths ----
output_dir <- here::here("overleaf/Fall 2025/sections/growth/images/neoclassical_growth")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Also create in figures/ for artifact upload
figures_dir <- here::here("figures")
dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Create Mock SVG Files ----
# List of all expected output files based on original script
mock_files <- c(
  "fig_neoclassical_growth_cdot_locus.svg",
  "fig_neoclassical_growth_kdot_locus.svg",
  "fig_neoclassical_growth_phase_quadrants.svg",
  "fig_neoclassical_growth_paths_baseline.svg",
  "fig_neoclassical_growth_saddle_full.svg",
  "fig_neoclassical_growth_baseline_color.svg",
  "fig_neoclassical_growth_long.svg",
  "fig_neoclassical_growth_long_color.svg",
  "fig_neoclassical_growth_very_long.svg",
  "fig_neoclassical_growth_very_long_color.svg",
  "fig_neoclassical_growth_near_ss.svg",
  "fig_neoclassical_growth_near_ss_color.svg",
  "fig_neoclassical_growth_halfgap.svg",
  "fig_neoclassical_growth_halfgap_color.svg",
  "fig_neoclassical_growth_halfk.svg",
  "fig_neoclassical_growth_halfk_color.svg",
  "fig_neoclassical_growth_halfk_long.svg",
  "fig_neoclassical_growth_halfk_long_color.svg"
)

# Create minimal valid SVG content
create_mock_svg <- function(filename) {
  svg_content <- '<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" width="400" height="300" viewBox="0 0 400 300">
  <rect width="400" height="300" fill="#f8f8f8"/>
  <text x="200" y="150" text-anchor="middle" font-size="16" fill="#333">
    Mock Phase Diagram Placeholder
  </text>
  <text x="200" y="180" text-anchor="middle" font-size="12" fill="#666">
    (Revert to main branch for actual figures)
  </text>
</svg>'
  writeLines(svg_content, filename)
}

# Generate all mock files (in both overleaf/ and figures/)
for (fname in mock_files) {
  # Create in overleaf directory
  fpath <- file.path(output_dir, fname)
  create_mock_svg(fpath)

  # Also create in figures/ for artifact upload
  fpath_figures <- file.path(figures_dir, fname)
  create_mock_svg(fpath_figures)

  message(sprintf("Created mock: %s", fname))
}

# Create mock initial_conditions.csv for reproducibility
csv_path <- here::here("data/processed/neoclassical")
dir.create(csv_path, showWarnings = FALSE, recursive = TRUE)
mock_params <- data.frame(
  alpha = 0.9,
  delta = 0.01,
  A = 0.4,
  n = 0.005,
  g = 0.02,
  rho = 0.02,
  theta = 2.0,
  k_peak = 10.0,
  k_star = 8.0,
  c_star = 2.0,
  k0 = 2.67,
  cF = 1.5,
  cA = 2.6,
  cB = 2.0,
  cC = 1.84,
  cD = 0.24,
  dk_sp = NA_real_
)
write.csv(mock_params, file = file.path(csv_path, "initial_conditions.csv"), row.names = FALSE)

message(sprintf("*** MOCK MODE COMPLETE: Created %d placeholder files in %.1f seconds ***", length(mock_files), 0.1))
