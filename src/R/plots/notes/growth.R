# R/plots/notes/growth.R
# Minimal implementations for testing

fetch_kaldor <- function(registry_file) {
  # Return dummy data for testing
  data.frame(
    year = 2000:2020,
    gdp = rnorm(21, mean = 100, sd = 10),
    capital = rnorm(21, mean = 50, sd = 5)
  )
}

plot_growth_kaldor <- function(data) {
  ggplot2::ggplot(data, ggplot2::aes(x = year, y = gdp)) +
    ggplot2::geom_line() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Kaldor Facts", x = "Year", y = "GDP")
}

plot_growth_kaldor_slides <- function(data) {
  ggplot2::ggplot(data, ggplot2::aes(x = year, y = gdp)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "lm") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Kaldor Facts (Slides)", x = "Year", y = "GDP")
}
