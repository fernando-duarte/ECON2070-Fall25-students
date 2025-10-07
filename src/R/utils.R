# R/utils.R
library(fs)
save_svg <- function(plot, file, width = 6, height = 4) {
  dir_create(path_dir(file))
  svglite::svglite(file, width = width, height = height)
  on.exit(grDevices::dev.off(), add = TRUE)
  print(plot)
}

save_pdf <- function(plot, file, width = 6, height = 4) {
  dir_create(path_dir(file))
  grDevices::cairo_pdf(filename = file, width = width, height = height)
  on.exit(grDevices::dev.off(), add = TRUE)
  print(plot)
}

# Paths that match your Overleaf tree exactly
ol_img_dir <- function(chapter) path("overleaf", "Fall 2025", "sections", chapter, "images")

# Notes figure base names (no extension)
notes_base <- function(chapter, name) path("figures", "notes", "2025", chapter, name)

# Slides figure base names
slides_base <- function(chapter, lecture, name) {
  path("figures", "slides", "2025", chapter, lecture, name)
}

save_notes_dual <- function(plot, chapter, name, width = 6, height = 4) {
  # SVG for versioning
  svg_path <- paste0(notes_base(chapter, name), ".svg")
  save_svg(plot, svg_path, width, height)
  # PDF directly into Overleaf submodule so LaTeX picks it up
  pdf_path <- path(ol_img_dir(chapter), paste0(name, ".pdf"))
  save_pdf(plot, pdf_path, width, height)
  # Optional: keep a copy of the SVG next to the PDF for Overleaf preview
  svg_copy <- path(ol_img_dir(chapter), paste0(name, ".svg"))
  file_copy(svg_path, svg_copy, overwrite = TRUE)
  # Return all created file paths for targets
  c(svg_path, pdf_path, svg_copy)
}

save_slides_svg <- function(plot, chapter, lecture, name, width = 7, height = 4) {
  svg_path <- paste0(slides_base(chapter, lecture, name), ".svg")
  save_svg(plot, svg_path, width, height)
  # Return file path for targets
  svg_path
}
