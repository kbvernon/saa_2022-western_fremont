
library(dplyr)
library(ggplot2)
library(here)
library(knitr)
library(kableExtra)
library(patchwork)
library(sf)
library(viridis)
library(xaringanExtra)
library(xaringanthemer)

knitr::opts_chunk$set(
  echo = FALSE,
  warning = FALSE,
  message = FALSE,
  error = FALSE,
  fig.align = "center",
  fig.retina = 3
)

options(
  digits = 4,
  htmltools.dir.version = FALSE,
  str = strOptions(vec.len = 2)
)

style_duo_accent(
  primary_color = "#5e5e5e",
  secondary_color = "#CC0000",
  colors = c(white = "#FFFFFF"),
  inverse_header_color = "#FFFFFF",
  code_font_google = google_font("Fira Mono"),
  header_h1_font_size = "45px",
  header_h2_font_size = "35px",
  header_h3_font_size = "20px"
)

xaringanExtra::use_tachyons()

xaringanExtra::use_scribble()

xaringanExtra::use_tile_view()

xaringanExtra::use_animate_css()

# not obvious, but images for xaringan must live in libs folder:
# https://github.com/yihui/xaringan/issues/145#issuecomment-758371822

figure <- function(x) {
  
  full_path <- paste0("libs/images/", x)
  
  knitr::include_graphics(full_path)
  
}
