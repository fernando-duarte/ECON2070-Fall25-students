# Generate ONLY fig_ramsey_paths_long.svg for validation
# This is a one-time validation script

source_all <- function(path = here::here("R/neoclassical_growth")) {
  files <- list.files(path, pattern = "\\.[Rr]$", full.names = TRUE)
  for (f in files) source(f, local = FALSE)
}

source_all()
cfg <- k_cfg(here::here("R/neoclassical_growth/config.yml"))

# Parameter Setup (from main script)
get_num_env <- function(key) {
  v <- Sys.getenv(key, unset = "")
  if (nzchar(v)) as.numeric(v) else NULL
}

base_params <- list(A = 0.4, alpha = 0.9, n = 0.005, g = 0.02, delta = 0.01, rho = 0.02, theta = 2.0)
ovr <- list(alpha = get_num_env("ALPHA_OVERRIDE"), delta = get_num_env("DELTA_OVERRIDE"))
ovr <- ovr[!vapply(ovr, is.null, logical(1))]
if (length(ovr)) base_params[names(ovr)] <- ovr

# Targeting helpers (from main script)
set_A_for_kpeak <- function(pars, kpeak_target = NULL) {
  if (is.null(kpeak_target)) return(pars)
  B <- pars$n + pars$g + pars$delta
  pars$A <- (B / pars$alpha) * (kpeak_target^(1 - pars$alpha))
  pars
}

adjust_kstar_ratio <- function(pars, ratio = 0.8) {
  reg <- create_registry()
  entry <- get_production(reg, "CD")
  P <- entry$builder(pars)
  B <- pars$n + pars$g + pars$delta
  target_fun <- function(x) P$fp(x) - B
  lo <- 1e-12
  hi <- 1.0
  while (target_fun(hi) > 0) hi <- hi * 2
  k_peak <- uniroot(target_fun, c(lo, hi))$root
  k_target <- ratio * k_peak
  fp_target <- P$fp(k_target)
  theta_req <- (fp_target - pars$rho - pars$delta) / pars$g
  if (!is.finite(theta_req) || theta_req <= 0) theta_req <- max(0.1, abs(theta_req))
  pars$theta <- theta_req
  list(params = pars, k_peak = k_peak, k_target = k_target, theta = theta_req)
}

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

kpeak_target <- get_num_env("KPEAK_TARGET")
base_params <- set_A_for_kpeak(base_params, kpeak_target)
ratio <- as.numeric(Sys.getenv("KSTAR_RATIO", unset = "0.8"))
adj <- adjust_kstar_ratio(base_params, ratio = ratio)
base_params <- adj$params
base_params <- refine_theta(base_params)

# Plot configuration
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
if (!is.null(dk_sp_val) && is.finite(dk_sp_val) && dk_sp_val > 0) plot_overrides$dk_sp <- dk_sp_val

# Device settings
dev_width <- if (!is.null(cfg$device$width)) cfg$device$width else 8
dev_height <- if (!is.null(cfg$device$height)) cfg$device$height else 6
base_family <- if (!is.null(cfg$device$family)) cfg$device$family else "serif"

# Create baseline model
model <- ramsey_model(prod = "CD", params = base_params, plot = plot_overrides)

# Output path
output_dir <- here::here("overleaf/Fall 2025/sections/growth/images")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
img_ext <- "svg"

# Generate extended-y Figure 4 variant ("-long")
cat("Generating fig_ramsey_paths_long.svg...\n")
base_fig4_ylim <- model$plot_opts$fig4_ylim_frac
if (!is.finite(base_fig4_ylim) || base_fig4_ylim <= 0) base_fig4_ylim <- 0.5
plot_overrides_4_long <- merge_options(plot_overrides, list(
  fig4_ylim_frac = 1.5 * base_fig4_ylim,
  fig4_equalize_D = TRUE,
  traj_cA_multiplier = 1.7
))
model4_long <- ramsey_model(prod = "CD", params = base_params, plot = plot_overrides_4_long)
out4_long_pattern <- file.path(output_dir, sprintf("ramsey_paths_long-%%d.%s", img_ext))
unlink(Sys.glob(file.path(output_dir, sprintf("ramsey_paths_long-*.%s", img_ext))))
ramsey_save(model4_long, file = out4_long_pattern, which = "combined", device = img_ext, width = dev_width, height = dev_height)
src4_long <- file.path(output_dir, sprintf("ramsey_paths_long-4.%s", img_ext))
dest4_long <- file.path(output_dir, sprintf("fig_ramsey_paths_long.%s", img_ext))
tmp4_long <- setdiff(Sys.glob(file.path(output_dir, sprintf("ramsey_paths_long-*.%s", img_ext))), src4_long)
if (length(tmp4_long)) unlink(tmp4_long)
if (file.exists(dest4_long)) file.remove(dest4_long)
if (file.exists(src4_long)) invisible(file.rename(src4_long, dest4_long))

cat("✓ fig_ramsey_paths_long.svg generated\n")
