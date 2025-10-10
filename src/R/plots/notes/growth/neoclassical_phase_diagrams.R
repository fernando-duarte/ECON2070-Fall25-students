# ---- Load Project Functions ----
source_all <- function(path = here::here("R/neoclassical_growth")) {
  files <- list.files(path, pattern = "\\.[Rr]$", full.names = TRUE)
  for (f in files) source(f, local = FALSE)
}

source_all()
cfg <- k_cfg(here::here("R/neoclassical_growth/config.yml"))

# ---- Parameter Setup ----
# allow environment overrides for alpha/delta
get_num_env <- function(key) {
  v <- Sys.getenv(key, unset = "")
  if (nzchar(v)) as.numeric(v) else NULL
}
# Revert n to baseline and increase horizontal speed by lowering n below baseline
base_params <- list(A = 0.4, alpha = 0.9, n = 0.005, g = 0.02, delta = 0.01, rho = 0.02, theta = 2.0)
ovr <- list(alpha = get_num_env("ALPHA_OVERRIDE"), delta = get_num_env("DELTA_OVERRIDE"))
ovr <- ovr[!vapply(ovr, is.null, logical(1))]
if (length(ovr)) base_params[names(ovr)] <- ovr

# ---- Targeting Helpers ----
# normalize A so that k_peak hits a desired numeric scale
set_A_for_kpeak <- function(pars, kpeak_target = NULL) {
  if (is.null(kpeak_target)) {
    return(pars)
  }
  B <- pars$n + pars$g + pars$delta
  pars$A <- (B / pars$alpha) * (kpeak_target^(1 - pars$alpha))
  pars
}

kpeak_target <- get_num_env("KPEAK_TARGET")
base_params <- set_A_for_kpeak(base_params, kpeak_target)

# adjust theta so that k* is a fixed ratio of k_peak
adjust_kstar_ratio <- function(pars, ratio = 0.8) {
  reg <- create_registry()
  entry <- get_production(reg, "CD")
  P <- entry$builder(pars)
  B <- pars$n + pars$g + pars$delta
  # k_peak solves fp(k)=B
  target_fun <- function(x) P$fp(x) - B
  lo <- 1e-12
  hi <- 1.0
  while (target_fun(hi) > 0) hi <- hi * 2
  k_peak <- uniroot(target_fun, c(lo, hi))$root
  k_target <- ratio * k_peak
  fp_target <- P$fp(k_target)
  # solve theta from fp(k*) = rho + theta*g + delta
  theta_req <- (fp_target - pars$rho - pars$delta) / pars$g
  if (!is.finite(theta_req) || theta_req <= 0) theta_req <- max(0.1, abs(theta_req))
  pars$theta <- theta_req
  list(params = pars, k_peak = k_peak, k_target = k_target, theta = theta_req)
}

ratio <- as.numeric(Sys.getenv("KSTAR_RATIO", unset = "0.8"))
adj <- adjust_kstar_ratio(base_params, ratio = ratio)
base_params <- adj$params

# refine theta so ċ = 0 holds numerically at k* with the chosen theta
refine_theta <- function(pars) {
  reg <- create_registry()
  P <- get_production(reg, "CD")$builder(pars)
  target <- function(x) P$fp(x) - (pars$rho + pars$theta * pars$g + pars$delta)
  lo <- 1e-12
  hi <- 1.0
  while (target(hi) > 0) hi <- hi * 2
  ks <- uniroot(target, c(lo, hi))$root
  theta2 <- (P$fp(ks) - pars$rho - pars$delta) / pars$g
  pars$theta <- theta2
  pars
}
base_params <- refine_theta(base_params)

dk_sp_env <- Sys.getenv("DK_SP", unset = "")
dk_sp_val <- if (nzchar(dk_sp_env)) as.numeric(dk_sp_env) else NULL
# ---- Plot Configuration ----
# Start from script defaults; merge config overrides if present; allow env to override dk_sp
plot_overrides <- list(
  label_E_side_fig4 = "NE",
  label_E_side_fig5 = "NW",
  label_E_dx_frac_fig5 = 0.01,
  label_E_dy_frac_fig5 = 0.02
)
if (!is.null(cfg) && is.list(cfg$plot)) {
  plot_overrides <- merge_options(plot_overrides, cfg$plot)
}
if (!is.null(dk_sp_val) && is.finite(dk_sp_val) && dk_sp_val > 0) plot_overrides$dk_sp <- dk_sp_val

palette_cfg <- NULL
if (!is.null(cfg$palette) && is.list(cfg$palette$math_econ)) {
  palette_cfg <- cfg$palette$math_econ
}
palette_defaults <- default_palette()
palette_color <- function(name) {
  if (!is.null(palette_cfg) && !is.null(palette_cfg[[name]]) && nzchar(palette_cfg[[name]])) {
    return(palette_cfg[[name]])
  }
  palette_defaults[[name]]
}
palette_overrides <- list(
  col_kdot = palette_color("kdot"),
  col_cdot = palette_color("cdot"),
  col_sp = palette_color("saddle"),
  col_traj = palette_color("trajectories"),
  arrow_col = palette_color("arrow")
)

model <- ramsey_model(prod = "CD", params = base_params, plot = plot_overrides)

# ---- Output Paths ----
output_dir <- here::here("overleaf/Fall 2025/sections/growth/images/neoclassical_growth")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Device settings for SVG/PNG output
dev_width <- if (!is.null(cfg$device$width)) cfg$device$width else 8
dev_height <- if (!is.null(cfg$device$height)) cfg$device$height else 6
base_family <- if (!is.null(cfg$device$family)) cfg$device$family else "serif"

#' Render a single Ramsey paths panel to file
#'
#' Ensures the device and graphics parameters are opened and restored locally
#' while delegating all drawing to `render_paths_panel()`.
#'
#' @param plot_opts Plot option overrides.
#' @param dest_filename Output file name, including extension.
#' @keywords internal
produce_paths_figure <- function(plot_opts, dest_filename) {
  model_tmp <- ramsey_model(prod = "CD", params = base_params, plot = plot_opts)
  device <- tolower(img_ext)
  open_device <- switch(device,
    svg = grDevices::svg,
    png = function(file, width, height) grDevices::png(filename = file, width = width, height = height, units = "in", res = 300),
    stop(sprintf("Unsupported image device '%s'", device), call. = FALSE)
  )
  open_device(dest_filename, width = dev_width, height = dev_height)
  on.exit(grDevices::dev.off(), add = TRUE)
  old_par_local <- par(no.readonly = TRUE)
  on.exit(par(old_par_local), add = TRUE)
  par(family = base_family)
  render_paths_panel(model_tmp)
}

message(sprintf("k_peak=%.4g, target k*=%.4g, theta set to %.4g", adj$k_peak, adj$k_target, adj$theta))

# ---- Diagnostics ----
# report initial conditions used for trajectories
ss <- steady_state(model)
k0 <- (1 / 3) * ss$k
ch <- model$dynamics$kdot0_locus(k0)
cF <- model$paths$c_on_saddle(k0)
cA <- min(ch * 1.30, 0.95 * (limits_for_model(model)$ymax))
cB <- ch
frac <- model$plot_opts$c_frac_FB
if (!is.finite(frac)) frac <- 0.5
cC <- if (is.finite(cF) && is.finite(cB)) (1 - frac) * cF + frac * cB else 0.92 * ch
cD <- max(0.12 * ch, 0.02 * (limits_for_model(model)$ymax))
message(sprintf("k* = %.4g, c* = %.4g, k0 = %.4g", ss$k, ss$c, k0))
message(sprintf("c0 on saddle (F) = %.4g | A=%.4g, B=%.4g, C=%.4g, D=%.4g", cF, cA, cB, cC, cD))

# export initial conditions to CSV for reproducibility
c0_out <- data.frame(
  alpha = base_params$alpha,
  delta = base_params$delta,
  A = base_params$A,
  n = base_params$n,
  g = base_params$g,
  rho = base_params$rho,
  theta = base_params$theta,
  k_peak = adj$k_peak,
  k_star = ss$k,
  c_star = ss$c,
  k0 = k0,
  cF = cF,
  cA = cA,
  cB = cB,
  cC = cC,
  cD = cD,
  dk_sp = if (is.null(dk_sp_val)) NA_real_ else dk_sp_val
)
csv_path <- here::here("data/processed/neoclassical")
dir.create(csv_path, showWarnings = FALSE, recursive = TRUE)
write.csv(c0_out, file = file.path(csv_path, "initial_conditions.csv"), row.names = FALSE)

# ---- Derived Outputs ----
# Generate SVGs (one file per page)
img_ext <- "svg"
suite_base <- "neoclassical_phase_suite"
svg_pattern <- file.path(output_dir, sprintf("%s-%%d.%s", suite_base, img_ext))
unlink(Sys.glob(sub("%d", "*", svg_pattern, fixed = TRUE)))
ramsey_save(model, file = svg_pattern, which = "combined", device = img_ext, width = dev_width, height = dev_height)

fig_keys <- c("cdot_locus", "kdot_locus", "phase_quadrants", "paths_baseline", "saddle_full")
src_figs <- file.path(output_dir, sprintf("%s-%d.%s", suite_base, seq_along(fig_keys), img_ext))
dest_figs <- file.path(output_dir, paste0("fig_neoclassical_growth_", fig_keys, ".", img_ext))
for (i in seq_along(src_figs)) {
  if (file.exists(dest_figs[i])) file.remove(dest_figs[i])
  if (file.exists(src_figs[i])) {
    invisible(file.rename(src_figs[i], dest_figs[i]))
  }
}

# NOTE: Color variant generation commented out due to long computation time (5+ minutes)
# Re-enable when trajectory calculation optimization is complete
if (FALSE) {
plot_overrides_color <- merge_options(plot_overrides, palette_overrides)
produce_paths_figure(
  plot_overrides_color,
  file.path(output_dir, sprintf("fig_neoclassical_growth_baseline_color.%s", img_ext))
)
}


# ---- Supplemental Figures ----
# NOTE: Supplemental figures commented out due to long computation times (trajectory
# calculations for extended variants can take 10+ minutes). Re-enable when optimization
# is complete or when running locally with sufficient time.
# TODO: Optimize trajectory calculations for long/very_long/near_ss/halfgap/halfk variants

# Generate extended-y Figure 4 variant ("-long")
if (FALSE) {  # Disabled - see note above
base_fig4_ylim <- model$plot_opts$fig4_ylim_frac
if (!is.finite(base_fig4_ylim) || base_fig4_ylim <= 0) base_fig4_ylim <- 0.5
plot_overrides_4_long <- merge_options(plot_overrides, list(
  fig4_ylim_frac = 1.5 * base_fig4_ylim,
  fig4_equalize_D = TRUE,
  traj_cA_multiplier = 1.7
))
model4_long <- ramsey_model(prod = "CD", params = base_params, plot = plot_overrides_4_long)
out4_long_pattern <- file.path(output_dir, sprintf("neoclassical_growth_long-%%d.%s", img_ext))
unlink(Sys.glob(file.path(output_dir, sprintf("neoclassical_growth_long-*.%s", img_ext))))
ramsey_save(model4_long, file = out4_long_pattern, which = "combined", device = img_ext, width = dev_width, height = dev_height)
src4_long <- file.path(output_dir, sprintf("neoclassical_growth_long-4.%s", img_ext))
dest4_long <- file.path(output_dir, sprintf("fig_neoclassical_growth_long.%s", img_ext))
tmp4_long <- setdiff(Sys.glob(file.path(output_dir, sprintf("neoclassical_growth_long-*.%s", img_ext))), src4_long)
if (length(tmp4_long)) unlink(tmp4_long)
if (file.exists(dest4_long)) file.remove(dest4_long)
if (file.exists(src4_long)) invisible(file.rename(src4_long, dest4_long))

plot_overrides_4_long_color <- merge_options(plot_overrides_4_long, palette_overrides)
produce_paths_figure(
  plot_overrides_4_long_color,
  file.path(output_dir, sprintf("fig_neoclassical_growth_long_color.%s", img_ext))
)

# Generate extra-extended Figure 4 variant ("-very-long")
plot_overrides_4_vlong <- merge_options(plot_overrides, list(
  fig4_ylim_frac = 3.0 * base_fig4_ylim,
  fig4_equalize_D = TRUE,
  traj_cA_multiplier = 1.7,
  fig4_label_A_dx_frac = 0.006,
  fig4_label_A_dy_frac = 0.010,
  label_E_dx_frac = 0.010,
  label_E_dy_frac = 0.020,
  remove_labels = c("D", "F")
))
model4_vlong <- ramsey_model(prod = "CD", params = base_params, plot = plot_overrides_4_vlong)
out4_vlong_pattern <- file.path(output_dir, sprintf("neoclassical_growth_very_long-%%d.%s", img_ext))
unlink(Sys.glob(file.path(output_dir, sprintf("neoclassical_growth_very_long-*.%s", img_ext))))
ramsey_save(model4_vlong, file = out4_vlong_pattern, which = "combined", device = img_ext, width = dev_width, height = dev_height)
src4_vlong <- file.path(output_dir, sprintf("neoclassical_growth_very_long-4.%s", img_ext))
dest4_vlong <- file.path(output_dir, sprintf("fig_neoclassical_growth_very_long.%s", img_ext))
tmp4_vlong <- setdiff(Sys.glob(file.path(output_dir, sprintf("neoclassical_growth_very_long-*.%s", img_ext))), src4_vlong)
if (length(tmp4_vlong)) unlink(tmp4_vlong)
if (file.exists(dest4_vlong)) file.remove(dest4_vlong)
if (file.exists(src4_vlong)) invisible(file.rename(src4_vlong, dest4_vlong))

plot_overrides_4_vlong_color <- merge_options(plot_overrides_4_vlong, palette_overrides)
produce_paths_figure(
  plot_overrides_4_vlong_color,
  file.path(output_dir, sprintf("fig_neoclassical_growth_very_long_color.%s", img_ext))
)

# Figure 6: same as Figure 4 with k(0) 30% closer to k*
spec_default <- default_trajectory_spec(model)
k_star_base <- model$dynamics$k_star
k0_default <- spec_default$k0
k_gap_share <- model$plot_opts$fig6_gap_share
if (!is.finite(k_gap_share) || k_gap_share <= 0 || k_gap_share >= 1) k_gap_share <- 0.4
k0_closer <- k_star_base - k_gap_share * (k_star_base - k0_default)
k0_frac_closer <- if (k_star_base > 0) k0_closer / k_star_base else plot_overrides$fig4_k0_frac
plot_overrides_6 <- merge_options(plot_overrides, list(
  fig4_k0_frac = k0_frac_closer,
  fig4_label_A_southeast = TRUE,
  fig4_equalize_D = TRUE,
  fig4_label_A_dx_frac = 0.010,
  fig4_label_A_dy_frac = 0.016,
  c_fracs_override = c(C = 1.05, D = 0.85)
))
model6 <- ramsey_model(prod = "CD", params = base_params, plot = plot_overrides_6)
out6_pattern <- file.path(output_dir, sprintf("neoclassical_growth_near_ss-%%d.%s", img_ext))
unlink(Sys.glob(file.path(output_dir, sprintf("neoclassical_growth_near_ss-*.%s", img_ext))))
ramsey_save(model6, file = out6_pattern, which = "combined", device = img_ext, width = dev_width, height = dev_height)
src6 <- file.path(output_dir, sprintf("neoclassical_growth_near_ss-4.%s", img_ext))
dest6 <- file.path(output_dir, sprintf("fig_neoclassical_growth_near_ss.%s", img_ext))
tmp6 <- setdiff(Sys.glob(file.path(output_dir, sprintf("neoclassical_growth_near_ss-*.%s", img_ext))), src6)
if (length(tmp6)) unlink(tmp6)
if (file.exists(dest6)) file.remove(dest6)
if (file.exists(src6)) invisible(file.rename(src6, dest6))

plot_overrides_6_color <- merge_options(plot_overrides_6, palette_overrides)
produce_paths_figure(
  plot_overrides_6_color,
  file.path(output_dir, sprintf("fig_neoclassical_growth_near_ss_color.%s", img_ext))
)


# Figure 7: higher k(0) so gap to k* is halved relative to Figure 6
k_gap_half_share <- 0.5 * k_gap_share
k0_halfgap <- k_star_base - k_gap_half_share * (k_star_base - k0_default)
k0_frac_half <- if (k_star_base > 0) k0_halfgap / k_star_base else plot_overrides$fig4_k0_frac
plot_overrides_7 <- merge_options(plot_overrides, list(
  fig4_k0_frac = k0_frac_half,
  fig4_label_A_southeast = TRUE,
  fig4_equalize_D = TRUE,
  fig4_axis_from_paths = TRUE,
  fig4_axis_margin_frac = 0.0
))
model7 <- ramsey_model(prod = "CD", params = base_params, plot = plot_overrides_7)
out7_pattern <- file.path(output_dir, sprintf("neoclassical_growth_halfgap-%%d.%s", img_ext))
unlink(Sys.glob(file.path(output_dir, sprintf("neoclassical_growth_halfgap-*.%s", img_ext))))
ramsey_save(model7, file = out7_pattern, which = "combined", device = img_ext, width = dev_width, height = dev_height)
src7 <- file.path(output_dir, sprintf("neoclassical_growth_halfgap-4.%s", img_ext))
dest7 <- file.path(output_dir, sprintf("fig_neoclassical_growth_halfgap.%s", img_ext))
tmp7 <- setdiff(Sys.glob(file.path(output_dir, sprintf("neoclassical_growth_halfgap-*.%s", img_ext))), src7)
if (length(tmp7)) unlink(tmp7)
if (file.exists(dest7)) file.remove(dest7)
if (file.exists(src7)) invisible(file.rename(src7, dest7))

plot_overrides_7_color <- merge_options(plot_overrides_7, palette_overrides)
produce_paths_figure(
  plot_overrides_7_color,
  file.path(output_dir, sprintf("fig_neoclassical_growth_halfgap_color.%s", img_ext))
)


# Extra: generate single Figure 4b plot using canonical Figure 4 logic with k(0)=0.5*k*
plot_overrides_4b <- merge_options(plot_overrides, list(
  fig4_k0_frac = 0.5,
  fig4_label_A_southeast = TRUE,
  fig4_label_A_dx_frac = 0.008,
  fig4_label_A_dy_frac = 0.014
))
model4b <- ramsey_model(prod = "CD", params = base_params, plot = plot_overrides_4b)

out4b_pattern <- file.path(output_dir, sprintf("neoclassical_growth_halfk-%%d.%s", img_ext))
# Clean previous 4b files
unlink(Sys.glob(file.path(output_dir, sprintf("neoclassical_growth_halfk-*.%s", img_ext))))

# Render full suite to paged files, then keep only page 4 (named as -4)
ramsey_save(model4b, file = out4b_pattern, which = "combined", device = img_ext, width = dev_width, height = dev_height)
src4 <- file.path(output_dir, sprintf("neoclassical_growth_halfk-4.%s", img_ext))
dest4 <- file.path(output_dir, sprintf("fig_neoclassical_growth_halfk.%s", img_ext))
if (file.exists(src4)) {
  # Remove any other temporary 4b pages, leaving only the -4 file
  tmp4b <- setdiff(Sys.glob(file.path(output_dir, sprintf("neoclassical_growth_halfk-*.%s", img_ext))), src4)
  if (length(tmp4b)) unlink(tmp4b)
  if (file.exists(dest4)) file.remove(dest4)
  invisible(file.rename(src4, dest4))
  message("Figure 4b written to ", src4)
} else {
  warning("Expected 4b page not found: ", src4)
}

plot_overrides_4b_color <- merge_options(plot_overrides_4b, palette_overrides)
produce_paths_figure(
  plot_overrides_4b_color,
  file.path(output_dir, sprintf("fig_neoclassical_growth_halfk_color.%s", img_ext))
)

# Extended-y Figure 4b variant ("-long")
plot_overrides_4b_long <- merge_options(plot_overrides_4b, list(fig4_ylim_frac = 1.5 * base_fig4_ylim))
model4b_long <- ramsey_model(prod = "CD", params = base_params, plot = plot_overrides_4b_long)
out4b_long_pattern <- file.path(output_dir, sprintf("neoclassical_growth_halfk_long-%%d.%s", img_ext))
unlink(Sys.glob(file.path(output_dir, sprintf("neoclassical_growth_halfk_long-*.%s", img_ext))))
ramsey_save(model4b_long, file = out4b_long_pattern, which = "combined", device = img_ext, width = dev_width, height = dev_height)
src4b_long <- file.path(output_dir, sprintf("neoclassical_growth_halfk_long-4.%s", img_ext))
dest4b_long <- file.path(output_dir, sprintf("fig_neoclassical_growth_halfk_long.%s", img_ext))
tmp4b_long <- setdiff(Sys.glob(file.path(output_dir, sprintf("neoclassical_growth_halfk_long-*.%s", img_ext))), src4b_long)
if (length(tmp4b_long)) unlink(tmp4b_long)
if (file.exists(dest4b_long)) file.remove(dest4b_long)
if (file.exists(src4b_long)) invisible(file.rename(src4b_long, dest4b_long))

plot_overrides_4b_long_color <- merge_options(plot_overrides_4b_long, palette_overrides)
produce_paths_figure(
  plot_overrides_4b_long_color,
  file.path(output_dir, sprintf("fig_neoclassical_growth_halfk_long_color.%s", img_ext))
)

obsolete_halfk <- file.path(output_dir, sprintf(c("ramsey_fig4b_paths.%s", "ramsey_fig4b_paths_long.%s"), img_ext))
obsolete_halfk <- obsolete_halfk[file.exists(obsolete_halfk)]
if (length(obsolete_halfk)) file.remove(obsolete_halfk)
}  # End of if (FALSE) block for supplemental figures
