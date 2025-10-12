# Generate ONLY fig_neoclassical_growth_halfk_color.svg for validation
# This is a one-time validation script

source_all <- function(path = here::here("R/neoclassical_growth")) {
  files <- list.files(path, pattern = "\\.[Rr]$", full.names = TRUE)
  for (f in files) {
    source(f, local = FALSE)
  }
}

source_all()
cfg <- k_cfg(here::here("R/neoclassical_growth/config.yml"))

# Parameter Setup
get_num_env <- function(key) {
  v <- Sys.getenv(key, unset = "")
  if (nzchar(v)) as.numeric(v) else NULL
}

base_params <- list(A = 0.4, alpha = 0.9, n = 0.005, g = 0.02, delta = 0.01, rho = 0.02, theta = 2.0)
ovr <- list(alpha = get_num_env("ALPHA_OVERRIDE"), delta = get_num_env("DELTA_OVERRIDE"))
ovr <- ovr[!vapply(ovr, is.null, logical(1))]
if (length(ovr)) {
  base_params[names(ovr)] <- ovr
}

set_a_for_kpeak <- function(pars, kpeak_target = NULL) {
  if (is.null(kpeak_target)) {
    return(pars)
  }
  break_even_rate <- pars$n + pars$g + pars$delta
  pars$A <- (break_even_rate / pars$alpha) * (kpeak_target^(1 - pars$alpha))
  pars
}

adjust_kstar_ratio <- function(pars, ratio = 0.8) {
  reg <- create_registry()
  entry <- get_production(reg, "CD")
  prod_fun <- entry$builder(pars)
  break_even_rate <- pars$n + pars$g + pars$delta
  target_fun <- function(x) prod_fun$fp(x) - break_even_rate
  lo <- 1e-12
  hi <- 1.0
  while (target_fun(hi) > 0) {
    hi <- hi * 2
  }
  k_peak <- uniroot(target_fun, c(lo, hi))$root
  k_target <- ratio * k_peak
  fp_target <- prod_fun$fp(k_target)
  theta_req <- (fp_target - pars$rho - pars$delta) / pars$g
  if (!is.finite(theta_req) || theta_req <= 0) {
    theta_req <- max(0.1, abs(theta_req))
  }
  pars$theta <- theta_req
  list(params = pars, k_peak = k_peak, k_target = k_target, theta = theta_req)
}

refine_theta <- function(pars) {
  reg <- create_registry()
  prod_fun <- get_production(reg, "CD")$builder(pars)
  target <- function(x) prod_fun$fp(x) - (pars$rho + pars$theta * pars$g + pars$delta)
  lo <- 1e-12
  hi <- 1.0
  while (target(hi) > 0) {
    hi <- hi * 2
  }
  ks <- uniroot(target, c(lo, hi))$root
  theta2 <- (prod_fun$fp(ks) - pars$rho - pars$delta) / pars$g
  pars$theta <- theta2
  pars
}

kpeak_target <- get_num_env("KPEAK_TARGET")
base_params <- set_a_for_kpeak(base_params, kpeak_target)
ratio <- as.numeric(Sys.getenv("KSTAR_RATIO", unset = "0.8"))
adj <- adjust_kstar_ratio(base_params, ratio = ratio)
base_params <- adj$params
base_params <- refine_theta(base_params)

dk_sp_env <- Sys.getenv("DK_SP", unset = "")
dk_sp_val <- if (nzchar(dk_sp_env)) as.numeric(dk_sp_env) else NULL

plot_overrides <- list(
  label_E_side_fig4 = "NE",
  label_E_side_fig5 = "NW",
  label_E_dx_frac_fig5 = 0.01,
  label_E_dy_frac_fig5 = 0.02
)
if (!is.null(cfg) && is.list(cfg$plot)) {
  plot_overrides <- merge_options(plot_overrides, cfg$plot)
}
if (!is.null(dk_sp_val) && is.finite(dk_sp_val) && dk_sp_val > 0) {
  plot_overrides$dk_sp <- dk_sp_val
}

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

dev_width <- if (!is.null(cfg$device$width)) cfg$device$width else 8
dev_height <- if (!is.null(cfg$device$height)) cfg$device$height else 6
base_family <- if (!is.null(cfg$device$family)) cfg$device$family else "serif"

model <- ramsey_model(prod = "CD", params = base_params, plot = plot_overrides)
output_dir <- here::here("overleaf/Fall 2025/sections/growth/images/neoclassical_growth")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
img_ext <- "svg"

# Helper function with minimal margins
produce_paths_figure <- function(plot_opts, dest_filename) {
  model_tmp <- ramsey_model(prod = "CD", params = base_params, plot = plot_opts)
  device <- tolower(img_ext)
  open_device <- switch(
    device,
    svg = grDevices::svg,
    png = function(file, width, height) {
      grDevices::png(
        filename = file,
        width = width,
        height = height,
        units = "in",
        res = 300
      )
    },
    stop(sprintf("Unsupported image device '%s'", device), call. = FALSE)
  )
  open_device(dest_filename, width = dev_width, height = dev_height)
  on.exit(grDevices::dev.off(), add = TRUE)
  old_par_local <- par(no.readonly = TRUE)
  on.exit(par(old_par_local), add = TRUE)
  par(family = base_family, xpd = FALSE)
  render_paths_panel(model_tmp)
  # Ensure final clipping to plot region for any elements drawn after
  par(xpd = FALSE)
}

# Generate halfk color variant
cat("Generating fig_neoclassical_growth_halfk_color.svg...\n")
plot_overrides_4b <- merge_options(
  plot_overrides,
  list(
    fig4_k0_frac = 0.5,
    fig4_label_A_southeast = TRUE,
    fig4_label_A_dx_frac = 0.008,
    fig4_label_A_dy_frac = 0.014,
    mar = c(1.5, 1.5, 0.2, 0.2),
    xaxs = "i",
    yaxs = "i",
    axis_label_offset_frac = 0.02, # Reduce offset for k(0) and k^* labels (default 0.04)
    axis_title_line = 0.5 # Reduce line offset for k title (bring closer to axis)
  )
)
plot_overrides_4b_color <- merge_options(plot_overrides_4b, palette_overrides)
produce_paths_figure(
  plot_overrides_4b_color,
  file.path(output_dir, sprintf("fig_neoclassical_growth_halfk_color.%s", img_ext))
)

cat("✓ fig_neoclassical_growth_halfk_color.svg generated\n")
