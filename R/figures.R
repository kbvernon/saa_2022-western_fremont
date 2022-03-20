
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
# 11) Site counts map
# 12) Covariates map
# 13) Response plots
# 14) Model map
# 15) Elevation profile
# 16) Rain
# 17) 2d response
# 18) 2d response gif
####################.

# R Preamble --------------------------------------------------------------

library(FedData)
library(gganimate)
library(ggfx)
library(gratia)
library(here)
library(mgcv)
library(patchwork)
library(sf)
library(slider)
library(terra)
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

gdd <- here("data", "gdd.prediction.Rds") %>% 
  read_rds() %>% 
  rename_with(tolower) %>% 
  rename("gdd" = "prediction (scaled)") %>% 
  select(cell, year, gdd) 

ppt <- here("data", "ppt.prediction.Rds") %>% 
  read_rds() %>% 
  rename_with(tolower) %>% 
  rename("precipitation" = "prediction (scaled)") %>% 
  select(cell, year, precipitation)

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

# Elevation profile -------------------------------------------------------

bb8 <- tibble(
  x = c(216765, 495057),
  y = c(4402719, 4503168)
) %>% 
  st_as_sf(
    coords = c("x", "y"),
    crs = 26912
  ) %>% 
  st_bbox() %>% 
  st_as_sfc() %>% 
  st_as_sf()

dem <- get_ned(bb8, label = "temp")

dem <- dem %>% 
  rast() %>% # convert to a spatRaster
  crop(st_transform(bb8, crs = 4326)) %>% 
  project("epsg:26912")

dem <- 
  dem %>% 
  resample(
    rast(
      extent = ext(dem), 
      resolution = c(500,500),
      crs = crs(dem)
    )
  ) %>% 
  mask(vect(bb8))

profile <- tibble(
  x = xFromCol(dem, 1:ncol(dem)),
  y = dem %>% as.matrix(wide = TRUE) %>% colMeans(na.rm = TRUE)  
) %>% 
  filter(!is.na(y))

remove(bb8, dem)

ggplot(profile, aes(x, ymin = 1000, ymax = y)) +
  geom_ribbon(
    fill = alpha("gray35", 0.5)
  ) +
  labs(
    x = "Easting (m)",
    y = NULL,
    title = "Elevation Profile of Western Utah",
    subtitle = "Average in meters for 440km to 450km North"
  ) +
  theme_bw(16) +
  coord_cartesian(expand = FALSE) +
  theme(
    axis.ticks = element_line(color = "gray85"),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray85")
  ) +
  scale_x_continuous(
    breaks = seq(225000, 475000, by = 50000),
    labels = scales::comma
  ) +
  annotate(
    "text",
    x = 300000,
    y = 2450,
    label = "West Desert",
    vjust = 1,
    family = "Rock Salt",
    color = "#B86A06",
    size = 6
  ) +
  annotate(
    "text",
    x = 420000,
    y = 2450,
    label = "Salt Lake\nValley",
    vjust = 1,
    family = "Rock Salt",
    color = "#A20000",
    size = 6
  )

ggsave(
  here("libs", "images", "elevation-profile.svg"),
  width = 12,
  height = 5
)



# Rain --------------------------------------------------------------------

watershed_years <- watersheds %>% 
  st_drop_geometry() %>% 
  mutate(cell = 1:n()) %>% 
  select(cell, id, elevation) %>% 
  left_join(ppt, by = c("cell")) %>% 
  filter(year > 849, year < 1375) %>% 
  mutate(
    elevation = cut(elevation, breaks = 3, labels = c("low", "medium", "high"))
  ) %>% 
  group_by(cell) %>% 
  arrange(year) %>% 
  mutate(
    precipitation = slide_vec(precipitation, mean, .before = 25, .after = 25)
  ) %>% 
  ungroup()

ggplot(watershed_years, aes(year, precipitation)) +
  geom_line(
    aes(group = cell),
    color = "gray35",
    alpha = 0.2
  ) +
  stat_summary(
    aes(color = forcats::fct_rev(elevation)),
    geom = "line",
    fun = mean,
    size = 2
  ) +
  scale_color_viridis(
    name = "Elevation",
    option = "magma",
    direction = -1,
    discrete = TRUE
  ) +
  coord_cartesian(expand = FALSE) +
  theme_bw(16) +
  theme(
    axis.ticks = element_line(color = "gray85"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray85")
  ) +
  labs(
    x = "Year",
    y = "Precipitation (mm)",
    title = "Annual Precipitation by Watershed",
    subtitle = "Fifty year rolling average"
  )

ggsave(
  here("libs", "images", "rain.svg"),
  width = 12,
  height = 6)

# 2d response -------------------------------------------------------------

watershed_points <- watersheds %>%
  st_drop_geometry() %>%
  mutate(cell = 1:n()) %>% 
  select(cell, elevation, area, protected, streams)

watershed_points <- watershed_points %>% 
  left_join(gdd, by = "cell") %>% 
  left_join(ppt, by = c("cell", "year")) %>% 
  mutate(
    period = case_when(
      (year > 0899 & year < 0950) ~ "Apogee (900 to 949 CE)",
      (year > 1274 & year < 1325) ~ "Collapse (1275 to 1324 CE)",
      TRUE ~ "other"
    )
  ) %>% 
  filter(
    period != "other"
  ) %>% 
  group_by(cell, period) %>%
  #----- Get the median values over the time period -----#
  summarize(
    precipitation = median(precipitation),
    gdd = median(gdd),
    protected = unique(protected),
    area = unique(area),
    streams = unique(streams)
  ) %>%
  ungroup() %>% 
  #----- Estimate site counts using the medians -----#
  mutate(
    sites = predict(settlement_model, newdata = ., type = "response"),
    sites = round(sites)
  ) %>% 
  uncount(sites)

ggplot(
  watershed_points, 
  aes(precipitation, gdd)
) + 
  stat_density_2d(
    geom = "polygon", 
    contour = TRUE,
    contour_var = "ndensity",
    aes(fill = after_stat(level)), 
    colour = "gray98",
    bins = 7,
    size = 0.3
  ) +
  scale_fill_viridis(
    name = NULL,
    option = "magma",
    na.value = "transparent"
  ) +
  labs(
    x = "Precipitation (mm)",
    y = "Maize GDD (°C)",
    title = "Relative Occurrence Rate Estimates for the Fremont"
  ) +
  coord_cartesian(xlim = c(0, 800), ylim = c(800, 1600)) +
  facet_wrap(~period) +
  theme_bw(16) +
  theme(
    aspect.ratio = 1,
    panel.background = element_rect(fill = "gray95"),
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(
      size = 16, 
      hjust = 0,
      margin = margin(l = 0, b = 5)
    )
  )

ggsave(
  here("libs", "images", "2d-response.svg"),
  width = 10,
  height = 5.5
)

remove(watershed_points)

# 2d response gif ---------------------------------------------------------

watershed_points <- watersheds %>%
  st_drop_geometry() %>%
  mutate(cell = 1:n()) %>% 
  select(cell, elevation, area, protected, streams) %>% 
  left_join(gdd, by = "cell") %>% 
  left_join(ppt, by = c("cell", "year")) %>% 
  filter(year %in% 950:1340) %>% 
  mutate(
    sites = predict(settlement_model, newdata = ., type = "response"),
    sites = slide_vec(sites, median, .before = 25, .after = 25),
    decade = cut(year, breaks = 40, labels = seq(950, 1340, by = 10))
  ) %>% 
  group_by(cell, decade) %>% 
  summarize(
    gdd = median(gdd),
    precipitation = median(precipitation),
    sites = median(sites),
    sites = round(sites)
  ) %>% 
  ungroup() %>% 
  uncount(sites) %>% 
  select(decade, cell, precipitation, gdd) %>% 
  arrange(decade, cell)

response <- ggplot(
  watershed_points, 
  aes(precipitation, gdd, group = decade)
) + 
  geom_density_2d_filled(
    aes(color = ..level..),
    contour_var = "ndensity",
    bins = 5,
    size = 0.3
  ) +
  scale_color_manual(
    name = NULL,
    values = c("transparent", rep("gray95", 4)),
    labels = c("very low", "low", "medium", "high", "very high")
  ) +
  scale_fill_viridis(
    name = NULL,
    option = "magma",
    na.value = "black",
    labels = c("very low", "low", "medium", "high", "very high"),
    discrete = TRUE
  ) +
  labs(
    x = "Precipitation (mm)",
    y = "Maize GDD (°C)",
    title = "Relative Occurrence Rate",
    subtitle = "Decade: {closest_state}"
  ) +
  coord_cartesian(xlim = c(0, 800), ylim = c(800, 1600)) +
  theme_bw(16) +
  theme(
    aspect.ratio = 1,
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(
      size = 16, 
      hjust = 0,
      margin = margin(l = 0, b = 5)
    )
  ) +
  transition_states(decade, 4, 2) +
  enter_fade()

anim_save(
  here("libs", "images", "2d-response.gif"),
  response,
  nframes = 40 * 6
)
