# script to make Fig 2

rm(list = ls())
opar <- par()

# load necessary files to make figures
load(here("data/countsHT.rda"))
load(here("data/IUCNareas.rda"))
AOH_heatmap <- raster(here("data/AOH_heatmap.tif"))

# world pop data
WP <- raster(here("data/wpop_resampled.tif"))
crs(WP) <- "+proj=longlat +datum=WGS84 +no_defs"

# region of interest
SEA.shp <- st_read(here("data/SEA.shp")) %>%
  st_set_crs(st_crs(AOH_heatmap)) %>%
  st_make_valid() %>%
  st_crop(st_bbox(AOH_heatmap))

# overlap of bats and humans calculations
# mask the population count raster by the AOH raster
WPmasked <- mask(WP, AOH_heatmap)
# multiply human population by species richness, apply log+1 transformation
overlap <- log(WPmasked * AOH_heatmap + 1)
# Standardize values 0-1
overlap_stnd <- overlap/cellStats(overlap, max)


myTheme <- theme_bw() + 
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "#def3f6"))


# Plot richness map (Fig 2a)
f2a <-
  ggplot() +
  layer_spatial(AOH_heatmap) +
  geom_sf(data = SEA.shp, color = "black", fill = NA, size = 0.3) +
  scale_fill_viridis(option = 'turbo', na.value = NA, limits = c(1, 16), 
                     breaks = c(1, 6, 11, 16)) +
  guides(fill = guide_colourbar(barwidth = 0.85, barheight = 6, nbin = 100,
                                draw.ulim = FALSE, draw.llim = FALSE)) +
  myTheme + 
  theme(legend.position = c(0.92, 0.45),
        legend.text = element_text(size = 12)) +
  coord_sf(xlim = st_bbox(AOH_heatmap)[c(1, 3)],
           ylim = st_bbox(AOH_heatmap)[c(2, 4)],
           expand = FALSE)

# Fig 2b
f2b <-
  ggplot() +
  layer_spatial(overlap_stnd) +
  geom_sf(data = SEA.shp, color = "black", fill = NA, size = 0.3) +
  scale_fill_viridis(option = 'plasma', na.value = NA) +
  guides(fill = guide_colourbar(barwidth = 0.85, barheight = 6)) +
  myTheme + 
  theme(legend.position = c(0.90, 0.45),
        legend.text = element_text(size = 11)) +
  coord_sf(xlim = st_bbox(overlap_stnd)[c(1, 3)],
           ylim = st_bbox(overlap_stnd)[c(2, 4)],
           expand = FALSE)

# create fig 2
fig2 <- f2a + f2b + 
  plot_annotation(tag_levels = 'a')  

# Save png, fig size following Nature specs 89 mm
ggsave(here('figures/Fig2.png'), fig2,
       device = 'png', width = 2, height = 0.85, dpi = 300, scale = 5)
