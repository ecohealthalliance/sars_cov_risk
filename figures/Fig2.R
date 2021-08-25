# code for Fig 2

load(here("data/countsHT.rda"))
load(here("data/mapColors.rda"))

# Fig. 2A: scatterplot of people in AOH vs AOH size-----------------------------

spCounts <- countsHT %>% 
  group_by(species) %>% 
  dplyr::summarise(totArea = sum(areakm2), peopleWP = sum(people_WPCount)) %>% 
  mutate(dens = peopleWP/totArea)

spCounts %<>%
  mutate(totArea = totArea/1000000, count = peopleWP/1000000)

#https://stackoverflow.com/questions/7549694/add-regression-line-equation-and-r2-on-graph
myFormula <- y ~ (x + 0)

ggplot(spCounts,  aes(x = totArea, y = count)) +
  geom_smooth(method = "lm", se = F, color = "gray", formula = myFormula) +
  stat_poly_eq(formula = myFormula, 
               aes(label = ..rr.label..),
               parse = TRUE) +
  geom_point() +
  geom_text_repel(size = 2.25, label = spCounts$species, direction = "y", 
                  hjust = 0, nudge_x = 0.2, nudge_y = 5, 
                  segment.color = "gray50") +
  xlab(expression(paste("Total area (million km"^2, ") of AOH"))) + 
  ylab("Number of people in AOH (millions)") +
  scale_y_continuous(breaks = seq(0, 150, 25)) +
  scale_x_continuous(breaks = seq(0, 2, 0.25)) +
  theme_bw() -> p2A

# Fig 2B: species AOH broken down by habitat proportions------------------------

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
  geom_bar(stat = "identity", position = position_fill(reverse = T)) +
  scale_fill_manual(name = "Habitat type", values = myColors, 
                    guide = guide_legend(reverse = T), labels = habLabs) +
  coord_flip() +
  ylab("Proportion of AOH") + xlab("") +
  theme_bw() +
  theme(legend.text = element_text(size = 8),
        legend.title = element_blank(),
        legend.key.height = unit(0.15, "in"),
        legend.position = "bottom",
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  guides(fill = guide_legend(nrow = 6, byrow = FALSE)) -> p2B


#tiff("./figures/Fig2.tif", height = 10, width = 7.5, units = "in", res = 300)
grid.arrange(p2A, p2B, ncol = 1)
#dev.off()
