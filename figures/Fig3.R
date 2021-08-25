# script to make Fig 3

rm(list = ls())
opar <- par()

# load necessary files to make figures
load(here("data/countsHT.rda"))
load(here("data/IUCNareas.rda"))
AOH_heatmap <- raster(here("data/AOH_heatmap.tif"))

# world pop data
WP <- raster(here("data/wpop_resampled.tif"))
crs(WP) <- "+proj=longlat +datum=WGS84 +no_defs"
load(here("data/mapColors.rda"))


SEA.shp <- st_read(here("data/SEA.shp")) %>%
  st_set_crs(st_crs(AOH_heatmap)) %>%
  st_make_valid() %>%
  st_crop(st_bbox(AOH_heatmap))


# Plot richness map (Fig 3a)
richness <-
  ggplot() +
  layer_spatial(AOH_heatmap) +
  geom_sf(data = SEA.shp, color = "black", fill = NA, size = 0.3) +
  scale_fill_viridis(option = 'turbo', na.value = NA, limits = c(1, 14), 
                     breaks = c(1, 5, 10, 14)) +
  guides(fill = guide_colourbar(barwidth = 0.85, barheight = 6, nbin = 100,
                                draw.ulim = FALSE, draw.llim = FALSE)) +
  theme_bw() + 
  theme(legend.position = c(0.92, 0.45),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        #panel.background = element_rect(fill = 'white'),
        #panel.grid.major = element_line(color = "#def3f6"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        #legend.background = element_rect(fill="#def3f6"),
        legend.key = element_rect(fill="#def3f6"),
        legend.text=element_text(size = 12)) +
  coord_sf(xlim = st_bbox(AOH_heatmap)[c(1, 3)],
           ylim = st_bbox(AOH_heatmap)[c(2, 4)],
           expand = FALSE)



# relative spillover risk (Fig 3b)

# mask the population count raster by the AOH raster
WPmasked <- mask(WP, AOH_heatmap)

# multiply human population by species richness, apply log+1 transformation
spillRisk <- log(WPmasked * AOH_heatmap + 1)
# Standarize values 0-1
spillRisk_stnd <- spillRisk/cellStats(spillRisk, max)

# Plot outbreak risk
out_risk <-
  ggplot() +
  layer_spatial(spillRisk_stnd) +
  geom_sf(data = SEA.shp, color = "black", fill = NA, size = 0.3) +
  scale_fill_viridis(option = 'plasma', na.value = NA) +
  guides(fill = guide_colourbar(barwidth = 0.85, barheight = 6)) +
  theme_bw() + 
  theme(legend.position = c(0.90, 0.45),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        #panel.background = element_rect(fill = 'white'),
        #panel.grid.major = element_line(color = "#def3f6"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        #legend.background = element_rect(fill="#def3f6"),
        legend.key = element_rect(fill="#def3f6"),
        legend.text=element_text(size = 11)) +
  coord_sf(xlim = st_bbox(spillRisk_stnd)[c(1, 3)],
           ylim = st_bbox(spillRisk_stnd)[c(2, 4)],
           expand = FALSE)

# create fig 3
combinedPlots <- richness + out_risk

fig3 <- combinedPlots + plot_annotation(tag_levels = 'a')  

# Save png, fig size following Nature specs 89 mm
# ggsave(here('figures/fig3.png'), fig3,
#        device = 'png', width = 2, height = 0.85, dpi = 300, scale = 5)
