
# Create figures for SAA presentation

####################.
#    OUTLINE
# 01) R Preamble
# 02) Data
# 03) Map Defaults
# 04) North America map
# 05) Maize map
# 06) Southwest map
# 07) Fremont map
# 08) Fremont zoom
# 09) Basemap
# 10) Western Fremont map
####################.

# R Preamble --------------------------------------------------------------

library(ggfx)
library(gratia)
library(here)
library(mgcv)
library(patchwork)
library(sf)
library(tidyverse)
library(viridis)
library(webp)

gpkg <- here("data", "saa2022.gpkg")

sys.source(
  here("R", "get_basemap.R"),
  envir = attach(NULL, name = "basemap")
)

# CRS for plotting
# USA Contiguous Albers Equal Area Conic
# epsg: 5070

theme_set(theme_void(14))
theme_update(
  panel.background = element_rect(color = "transparent", fill = "transparent"),
  plot.background = element_rect(color = "transparent", fill = "transparent")
)

# Data --------------------------------------------------------------------

north_america <- read_sf(gpkg, layer = "north_america") %>% st_transform(5070)

states <- read_sf(gpkg, layer = "states") %>% st_transform(5070)

southwest <- read_sf(gpkg, layer = "southwest") %>% st_transform(5070)

fremont <- read_sf(gpkg, layer = "fremont") %>% st_transform(5070)

window <- read_sf(gpkg, layer = "window") %>% st_transform(5070)

maize <- read_sf(gpkg, layer = "maize") %>% st_transform(5070)

provinces <- read_sf(gpkg, layer = "provinces") %>% st_transform(5070)

watersheds <- read_sf(gpkg, layer = "watersheds") %>% st_transform(5070)

# cut out maize from southwest so they don't overlap
southwest <- southwest %>% 
  st_difference(maize) %>% 
  st_cast("POLYGON") %>% 
  filter(st_area(.) == max(st_area(.)))

settlement_model <- here("data", "western_fremont_model.Rds") %>% read_rds()

# Map Defaults ------------------------------------------------------------

bb8 <- north_america %>% st_buffer(150000) %>% st_bbox()

dy <- bb8[["ymax"]] - bb8[["ymin"]]
dx <- dy * 16/9

mid_x <- (bb8[["xmax"]] + bb8[["xmin"]])/2

bb8[["xmax"]] <- mid_x + (dx/2)
bb8[["xmin"]] <- mid_x - (dx/2)

remove(dy,dx,mid_x)

background <- list(
  geom_sf(
    data = north_america,
    fill = "#fcf2de",
    color = "black",
    size = 0.03
  ),
  geom_sf(
    data = states,
    fill = "transparent",
    color = "gray70",
    size = 0.1
  )
)

coordinates <- list(
  coord_sf(
    xlim = c(bb8[["xmin"]], bb8[["xmax"]]),
    ylim = c(bb8[["ymin"]], bb8[["ymax"]]),
    expand = FALSE
  )
)

color_maize <- "#4CB944"
color_southwest <- "#D45308"
color_fremont <- "#247BA0"

# North America map -------------------------------------------------------

ggplot() + 
  background +
  geom_sf(
    data = north_america,
    fill = "transparent",
    color = "black",
    size = 0.4
  ) +
  coordinates
  
ggsave(
  here("libs", "images", "north_america.png"),
  width = 16,
  height = 9,
  dpi = 600
)

# Maize map ---------------------------------------------------------------

ggplot() + 
  with_inner_glow(
    geom_sf(
      data = maize,
      fill = color_maize,
      color = color_maize
    )
  ) +
  annotate(
    "curve",
    x = -3000000,
    xend = -1055000,
    y = -70000,
    yend = -380000,
    arrow = arrow(length = unit(0.17, "inches")),
    color = color_maize,
    curvature = 0.5,
    angle = 115,
    size = 0.9
  ) +
  annotate(
    "text",
    x = -3000000,
    y = 60000,
    label = "origin of maize?",
    lineheight = 0.8,
    size = 8,
    hjust = 0.85,
    vjust = 0,
    family = "Rock Salt",
    color = color_maize
  ) +
  coordinates

ggsave(
  here("libs", "images", "north_america-maize.svg"),
  width = 16,
  height = 9
)

# Southwest map -----------------------------------------------------------

ggplot() + 
  with_inner_glow(
    geom_sf(
      data = southwest,
      fill = color_southwest,
      color = color_southwest
    )
  ) +
  annotate(
    "curve",
    x = 2150000,
    xend = -400000,
    y = 1400000,
    yend = 1000000,
    arrow = arrow(length = unit(0.17, "inches")),
    color = color_southwest,
    curvature = 0.5,
    angle = 100,
    size = 0.9
  ) +
  annotate(
    "text",
    x = 2150000,
    y = 1370000,
    label = "greater\nsouthwest",
    lineheight = 0.8,
    size = 8,
    hjust = 0.1,
    vjust = 1,
    family = "Rock Salt",
    color = color_southwest
  ) +
  coordinates

ggsave(
  here("libs", "images", "north_america-southwest.svg"),
  width = 16,
  height = 9
)

# Fremont map -------------------------------------------------------------

ggplot() + 
  with_inner_glow(
    geom_sf(
      data = fremont,
      fill = color_fremont,
      color = color_fremont
    )
  ) +
  annotate(
    "curve",
    x = -2900000,
    xend = -1400000,
    y = 3160000,
    yend = 2280000,
    arrow = arrow(length = unit(0.17, "inches")),
    color = color_fremont,
    curvature = -0.5,
    angle = 75,
    size = 0.9
  ) +
  annotate(
    "text",
    x = -2980000,
    y = 3160000,
    label = "fremont",
    lineheight = 0.8,
    size = 8,
    hjust = 1,
    vjust = 0.3,
    family = "Rock Salt",
    color = color_fremont
  ) +
  coordinates

ggsave(
  here("libs", "images", "north_america-fremont.svg"),
  width = 16,
  height = 9
)

# Fremont zoom ----------------------------------------------------------

bb8 <- states %>%
  filter(name == "Utah") %>% 
  st_transform(26912) %>% 
  st_buffer(dist = 70000) %>% 
  st_bbox()

dy <- bb8[["ymax"]] - bb8[["ymin"]]
dx <- dy * 16/9

mid_x <- (bb8[["xmax"]] + bb8[["xmin"]])/2

bb8[["xmax"]] <- mid_x + (dx/2)
bb8[["xmin"]] <- mid_x - (dx/2)

coordinates_zoom <- list(
  coord_sf(
    crs = 26912,
    datum = 26912,
    xlim = c(bb8[["xmin"]], bb8[["xmax"]]),
    ylim = c(bb8[["ymin"]], bb8[["ymax"]]),
    expand = FALSE
  )
)

suppressWarnings({
  
  provinces <- provinces %>% 
    st_cast("LINESTRING") %>% 
    st_intersection(
      states %>% filter(name == "Utah")
    ) %>% 
    st_cast("LINESTRING") %>% 
    filter(st_length(.) != min(st_length(.))) %>% 
    slice(2:3)
  
})

pbox <- provinces %>% st_transform(26912) %>% st_bbox()

remove(dx,dy,mid_x)

ggplot() + 
  background +
  with_inner_glow(
    geom_sf(
      data = fremont,
      fill = color_fremont,
      color = color_fremont
    )
  ) +
  geom_sf(
    data = provinces,
    fill = "transparent",
    color = "darkred",
    size = 1
  ) +
  #----- COLORADO PLATEAU -----#
  annotate(
    "segment",
    x = pbox[["xmax"]] + 30000,
    y = pbox[["ymin"]] + 40000,
    xend = pbox[["xmax"]] + 120000,
    yend = pbox[["ymin"]] + 40000,
    arrow = arrow(length = unit(0.13, "inches"), type = "closed")
  ) +
  annotate(
    "text",
    x = pbox[["xmax"]] + 130000,
    y = pbox[["ymin"]] + 40000,
    label = "colorado\nplateau",
    lineheight = 0.8,
    size = 9,
    family = "Rock Salt",
    hjust = 0,
    vjust = 0.4
  ) +
  #----- GREAT BASIN -----#
  annotate(
    "segment",
    x = pbox[["xmin"]] + 25000,
    y = pbox[["ymax"]] - 40000,
    xend = pbox[["xmin"]] - 80000,
    yend = pbox[["ymax"]] - 40000,
    arrow = arrow(length = unit(0.13, "inches"), type = "closed")
  ) +
  annotate(
    "text",
    x = pbox[["xmin"]] - 90000,
    y = pbox[["ymax"]] - 40000,
    label = "great\nbasin",
    lineheight = 0.8,
    size = 9,
    family = "Rock Salt",
    hjust = 1,
    vjust = 0.4
  ) +
  coordinates_zoom

ggsave(
  here("libs", "images", "fremont-provinces.png"),
  width = 16,
  height = 9,
  dpi = 600
)

# Basemap -----------------------------------------------------------------

basemap <- get_basemap(
  bb8, 
  map = "imagery",
  size = c(16000, 9000),
  imageSR = 26912
)

cover <- window %>% 
  st_transform(26912) %>% 
  st_sym_difference(st_as_sfc(bb8))

ggplot() + 
  annotation_raster(
    basemap, 
    bb8[["xmin"]], bb8[["xmax"]], 
    bb8[["ymin"]], bb8[["ymax"]]
  ) +
  geom_sf(
    data = cover,
    color = "transparent",
    fill = alpha("white", 0.7)
  ) +
  geom_sf(
    data = states,
    fill = "transparent",
    color = "gray55",
    size = 0.35
  ) +
  coordinates_zoom

fn <- tempfile(fileext = ".jpeg")

ggsave(
  filename = fn,
  width = 16,
  height = 9,
  dpi = 600
)

write_webp(
  jpeg::readJPEG(fn),
  here("libs", "images", "satellite_imagery.webp")
)

remove(fn)

# Western Fremont map -----------------------------------------------------

ggplot() +
  geom_sf(
    data = window,
    fill = "transparent"
  ) +
  geom_sf(
    data = provinces,
    fill = "transparent",
    color = "darkred",
    size = 1
  ) +
  coordinates_zoom

ggsave(
  here("libs", "images", "fremont-west.png"),
  width = 16,
  height = 9,
  dpi = 600
)

# Site counts map ---------------------------------------------------------

ggplot() + 
  geom_sf(
    data = watersheds,
    aes(fill = sites),
    color = "gray95", 
    size = 0.3
  ) +
  scale_fill_viridis(
    name = NULL,
    option = "magma",
    breaks = seq(0, 120, by = 40),
    labels = seq(0, 120, by = 40),
    limits = c(0, 120),
    guide = guide_colorbar(
      barheight = unit(2, "in"),
      barwidth = unit(0.25, "in"),
      label.position = "left",
      draw.ulim = TRUE
    )
  ) +
  geom_sf(
    data = window,
    fill = "transparent"
  ) +
  coordinates_zoom +
  theme(
    legend.background = element_rect(fill = "transparent", color = "transparent"),
    legend.justification = c("right", "bottom"),
    legend.position = c(0.27, 0.38),
    legend.text = element_text(size = 20)
  )

ggsave(
  here("libs", "images", "fremont-site_counts.png"),
  width = 16,
  height = 9,
  dpi = 600
)

# Covariates map ----------------------------------------------------------

background <- ggplot() +
  geom_sf(
    data = states %>% filter(name == "Utah"),
    fill = "gray95"
  ) +
  theme(
    legend.background = element_rect(fill = "gray95", color = "transparent"),
    legend.justification = c("left", "bottom"),
    legend.position = c(0.65, 0.25)
  )

gdd <- background +
  geom_sf(
    data = watersheds, 
    aes(fill = gdd),
    color = "gray75", 
    size = 0.3
  ) +
  scale_fill_viridis(
    name = "GDD (°C)",
    option = "magma"
  )

ppt <- background +
  geom_sf(
    data = watersheds, 
    aes(fill = precipitation),
    color = "gray75", 
    size = 0.3
  ) +
  scale_fill_viridis(
    name = "PPT (mm)",
    option = "magma"
  )

streams <- background +
  geom_sf(
    data = watersheds, 
    aes(fill = streams),
    color = "gray75", 
    size = 0.3
  ) +
  scale_fill_viridis(
    name = "Streams\n(hours)",
    option = "magma"
  )

gdd + ppt + streams & coord_sf(datum = 26912, crs = 26912)

ggsave(
  here("libs", "images", "covariates.png"),
  width = 12,
  height = 5,
  dpi = 600
)

remove(gdd, ppt, streams)

# Response plots ----------------------------------------------------------

inv_link <- family(settlement_model)$linkinv

margins <- settlement_model %>% 
  smooth_estimates(n = 300) %>% 
  mutate(
    y = inv_link(est),
    ymin = inv_link(est - (2*se)),
    ymax = inv_link(est + (2*se))
  ) %>% 
  select(precipitation:ymax) %>% 
  pivot_longer(
    c(precipitation, gdd, streams),
    names_to = "variable",
    values_to = "x"
  ) %>% 
  filter(
    !is.na(x)
  )

remove(inv_link)

scale_pretty <- function(x, n = 4L) {
  
  brks <- pretty(x, n = n)
  
  nbrks <- length(brks)
  
  i <- ifelse(
    nbrks > n,
    c(1, nbrks),
    nbrks
  )
  
  brks[-i]
  
}

ggplot(margins, aes(x, y)) +
  geom_ribbon(
    aes(ymin = ymin, ymax = ymax, fill = variable)
  ) + 
  scale_fill_manual(
    values = c("#F26419", "#187795", "#008148")
  ) +
  geom_line() +
  labs(
    x = NULL,
    y = "Number of sites"
  ) +
  facet_wrap(
    ~variable, 
    scale = "free_x",
    strip.position = "bottom",
    labeller = as_labeller(
      c(
        "precipitation" = "Precipitation (mm)",
        "gdd"           = "Maize GDD (°C)",
        "streams"       = "CD to streams (hours)"
      )
    )
  ) +
  scale_x_continuous(breaks = scale_pretty) +
  theme_bw(16) +
  theme(
    axis.text = element_text(size = 13),
    axis.title.y = element_text(size = 15),
    legend.position = "none",
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text = element_text(size = 15)
  )

ggsave(
  here("libs", "images", "model-response_plots.svg"),
  width = 8,
  height = 8 * 0.45
)

# Model map ---------------------------------------------------------------

watersheds <- watersheds %>% 
  mutate(
    estimate = predict(
      settlement_model, 
      newdata = watersheds, 
      type = "response"
    ),
    residuals = residuals(
      settlement_model,
      type = "response"
    )
  )


background <- ggplot() +
  geom_sf(
    data = states %>% filter(name == "Utah"),
    fill = "gray95"
  ) +
  theme(
    legend.background = element_rect(fill = "gray95", color = "transparent"),
    legend.justification = c("left", "bottom"),
    legend.position = c(0.65, 0.35)
  )


obs <- background +
  geom_sf(
    data = watersheds, 
    aes(fill = sites),
    color = "gray75", 
    size = 0.3
  ) +
  scale_fill_viridis(
    name = "Observed\nSite Count",
    option = "magma"
  )

est <-  background +
  geom_sf(
    data = watersheds, 
    aes(fill = estimate),
    color = "gray75", 
    size = 0.3
  ) +
  scale_fill_viridis(
    name = "Estimated\nSite Count",
    option = "magma"
  )

res <- background +
  geom_sf(
    data = watersheds, 
    aes(fill = residuals),
    color = "gray75", 
    size = 0.3
  ) +
  scale_fill_viridis(
    name = "Residuals",
    option = "magma"
  )

obs + est + res & coord_sf(datum = 26912, crs = 26912)

ggsave(
  here("libs", "images", "model-map.png"),
  width = 12,
  height = 5,
  dpi = 600
)

remove(background, obs, est, res)

# 2d response -------------------------------------------------------------

watershed_points <- watersheds %>%
  st_drop_geometry() %>%
  mutate(cell = 1:n()) %>% 
  select(cell, area, protected, streams)

gdd <- here("data", "gdd.prediction.Rds") %>% 
  read_rds() %>% 
  rename_with(tolower) %>% 
  rename("gdd" = "prediction (scaled)") %>% 
  select(cell, year, gdd) %>% 
  filter(year > 1274, year < 1325)

ppt <- here("data", "ppt.prediction.Rds") %>% 
  read_rds() %>% 
  rename_with(tolower) %>% 
  rename("precipitation" = "prediction (scaled)") %>% 
  select(cell, year, precipitation) %>% 
  filter(year > 1274, year < 1325)

watershed_points <- watershed_points %>% 
  left_join(gdd, by = "cell") %>% 
  left_join(ppt, by = c("cell", "year")) %>% 
  mutate(
    sites = predict(settlement_model, newdata = ., type = "response"),
    sites = round(sites)
  ) %>% 
  uncount(sites)

remove(gdd, ppt)

ggplot(
  watershed_points, 
  aes(precipitation, gdd)
) + 
  stat_density_2d(
    geom = "polygon", 
    contour = TRUE,
    contour_var = "ndensity",
    aes(fill = after_stat(level)), 
    colour = "gray98"
  ) +
  scale_fill_viridis(
    name = "Relative\nOccurrence\nRate",
    option = "magma",
    na.value = "transparent"
  ) +
  labs(
    x = "Precipitation (mm)",
    y = "Maize GDD (°C)",
    title = "Distribution of Fremont Sites", 
    subtitle = "For the period 1275 to 1324 CE"
  ) +
  coord_cartesian(xlim = c(0, 800), ylim = c(800, 1600)) +
  theme_bw(16) +
  theme(
    aspect.ratio = 1,
    panel.background = element_rect(fill = "gray95"),
    panel.grid = element_blank()
  )

ggsave(
  here("libs", "images", "2d-response.svg"),
  width = 6.5,
  height = 6.5
)
