# code for Fig 2

load(here("data/countsHT.rda"))
load(here("data/mapColors.rda"))

# Fig. 1a: scatterplot of people in AOH vs AOH size-----------------------------

spCounts <- countsHT %>% 
  group_by(species) %>% 
  dplyr::summarise(totArea = sum(areakm2), peopleWP = sum(people_WPCount)) %>% 
  mutate(dens = peopleWP/totArea) %>% 
  mutate(totArea = totArea/1000000, count = peopleWP/1000000) %>% 
  # abbreviations for plotting panel a
  mutate(spAbb = str_replace_all(species, "Rhinolophus ", "R. "),
         spAbb = str_replace_all(spAbb, "Hipposideros ", "H. "),
         spAbb = str_replace_all(spAbb, "Chaerephon ", "C. "),
         spAbb = str_replace_all(spAbb, "Nyctalus ", "N. "),
         spAbb = str_replace_all(spAbb, "Tadarida ", "T. "),
         spAbb = str_replace_all(spAbb, "Aselliscus ", "A. "))

#https://stackoverflow.com/questions/7549694/add-regression-line-equation-and-r2-on-graph
myFormula <- y ~ (x + 0)

ggplot(spCounts, aes(x = totArea, y = count)) +
  geom_smooth(method = "lm", se = F, color = "gray", formula = myFormula) +
  stat_poly_eq(formula = myFormula, 
               aes(label = ..rr.label..),
               parse = TRUE) +
  geom_point() +
  geom_text_repel(size = 3, label = spCounts$spAbb, direction = "y", 
                  hjust = 0, fontface = "italic",
                  nudge_x = 0.3, nudge_y = 5,
                  box.padding = 0.1,
                  segment.color = "gray50", max.overlaps = 20) +
  xlab(expression(paste("Total area (million km"^2, ") of AOH"))) + 
  ylab("Number of people in AOH (millions)") +
  scale_y_continuous(breaks = seq(0, 225, 25)) +
  scale_x_continuous(breaks = seq(0, 3, 0.25)) +
  theme_bw() -> p1a

# Fig 1b: species AOH broken down by habitat proportions------------------------

mapColors %<>% 
  mutate_at(vars(habType), as.numeric)

# divide area by 1 million for easier comprehension
# use original Jung map colors
# it didn't include 7.1 (caves), so going to use gray for now
spAreas <- countsHT %>% 
  mutate(totalArea = areakm2/1000000) %>% 
  left_join(., mapColors) %>% 
  replace_na(list(hex = "#808080")) %>% 
  mutate(habType = as.factor(habType))

# reorder species by total area
spAreas$species <- reorder(spAreas$species, spAreas$totalArea, sum)

# calculate habitat %s for labels
barPercents <- spAreas %>% 
  group_by(species) %>% 
  mutate(tot = sum(areakm2)) %>% 
  group_by(species, habType) %>% 
  mutate(perc = round(areakm2/tot, 2)*100) %>% 
  mutate(lab = paste0(perc, "%")) %>% 
  mutate(lab = str_replace_all(lab, "^0%$", "<1%")) %>% 
  dplyr::select(species, habType, perc, lab)


myColors <- spAreas$hex
names(myColors) <- spAreas$habType

habLabs <- c("1.4 Forest - Temperate", 
             "1.5 Forest - Subtropical/tropical dry",
             "1.6 Forest - Subtropical/tropical moist lowland",
             "1.9 Forest - Subtropical/tropical moist montane",
             "3.4 Shrubland - Temperate",
             "4.4 Grassland - Temperate",
             "6 Rocky Areas (e.g., inland cliffs, mountain peaks)",
             "7.1 Carbonate rock outcrops",
             "14.1 Arable Land",
             "14.2 Pastureland",
             "14.3 Plantations",
             "14.5 Urban Areas")

# show area proportions for habitat types within AOH
# to add lines between stacks, add color = "black" to geom_bar
ggplot(spAreas, aes(x = species, y = totalArea, fill = habType)) +
  geom_bar(stat = "identity", position = position_fill(reverse = T), 
           alpha = 0.8) +
  geom_text(barPercents, mapping = aes(x = species, y = perc, label = lab),
            position = position_fill(reverse = T, vjust = 0.5),
            size = 2.5) +
  scale_fill_manual(name = "Habitat type", values = myColors, 
                    guide = guide_legend(reverse = T), labels = habLabs) +
  #coord_flip() +
  ylab("Proportion of AOH") + xlab("") +
  theme_bw() +
  theme(legend.text = element_text(size = 8),
        legend.title = element_blank(),
        legend.key.height = unit(0.15, "in"),
        legend.position = "bottom",
        axis.text.x = element_text(face = "italic", angle = 90),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  guides(fill = guide_legend(nrow = 6, byrow = FALSE))  -> p1b

p1a / p1b +
  plot_layout(heights = c(1.5, 1)) + 
  plot_annotation(tag_levels = 'a')  

ggsave(here("figures/Fig1.png"), height = 13, width = 8, units = "in", 
       dpi = 300)
ggsave(here("figures/Fig1.tiff"), height = 13, width = 8, units = "in", 
       dpi = 300)
