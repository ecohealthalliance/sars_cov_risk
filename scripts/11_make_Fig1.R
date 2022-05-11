# code for Fig 1

# load data
load(here("data/countsHT.rda"))

# species abbreviations for plotting
countsHT %<>%
  mutate(species = str_replace_all(species, "Rhinolophus ", "R. "),
         species = str_replace_all(species, "Hipposideros ", "H. "),
         species = str_replace_all(species, "Chaerephon ", "C. "),
         species = str_replace_all(species, "Nyctalus ", "N. "),
         species = str_replace_all(species, "Tadarida ", "T. "),
         species = str_replace_all(species, "Aselliscus ", "A. "))

# labels for habitat types
habLabs <- c("1.4 Forest - Temperate", 
             "1.5 Forest - Subtropical/tropical dry",
             "1.6 Forest - Subtropical/tropical moist lowland",
             "1.9 Forest - Subtropical/tropical moist montane",
             "3.4 Shrubland - Temperate",
             "4.4 Grassland - Temperate",
             #"6 Rocky Areas (e.g., inland cliffs, mountain peaks)",
             "6 Rocky Areas",
             "Carbonate rock outcrops",
             "14.1 Arable Land",
             "14.2 Pastureland",
             "14.3 Plantations",
             "14.5 Urban Areas")

myTheme <- theme_bw() +
  theme(axis.text = element_text(size = 5, color = "black"),
        axis.title = element_text(size = 5),
        panel.grid.major = element_line(size = 0.2),
        panel.grid.minor = element_line(size = 0.1),
        axis.ticks.length = unit(0.5, "mm"))

# http://mkweb.bcgsc.ca/colorblind/palettes.mhtml
myPal <- c("#009F81", "#9F0162", "#FFC33B", "#00FCCF", "#8400CD", "#FFB2FD",
           "3008DF9", "#00C2F9", "#A40122", "#E20134", "#FF6E3A", "#FF5AAF")
  
# Fig. 1a: scatterplot of people in AOH vs AOH size-----------------------------

spCounts <- countsHT %>% 
  group_by(species) %>% 
  dplyr::summarise(totArea = sum(areakm2), peopleWP = sum(people_WPCount)) %>% 
  mutate(dens = peopleWP/totArea) %>% 
  mutate(totArea = totArea/1000000, count = peopleWP/1000000)

#https://stackoverflow.com/questions/7549694/add-regression-line-equation-and-r2-on-graph
myFormula <- y ~ (x + 0)

spCounts$species[c(8, 11, 13, 26)] <- ""

ggplot(spCounts, aes(x = totArea, y = count)) +
  geom_smooth(method = "lm", se = F, color = "gray", formula = myFormula, 
              size = 0.5) +
  stat_poly_eq(formula = myFormula, 
               aes(label = ..rr.label..),
               parse = TRUE, size = 2) +
  geom_point(size = 0.5) +
  geom_text_repel(size = 1.4, label = spCounts$species, 
                  segment.size = 0.2,
                  hjust = 0, 
                  fontface = "italic",
                  nudge_x = 0.5,
                  force = 2, 
                  direction = "x",
                  max.time = 5,
                  segment.color = "gray50",
                  min.segment.length = unit(1, "mm"),
                  max.overlaps = 20) +
  xlab(expression(paste("Total area (million km"^2, ") of AOH"))) + 
  ylab("People in AOH (millions)") +
  scale_y_continuous(breaks = seq(0, 225, 25)) +
  scale_x_continuous(breaks = seq(0, 3, 0.5)) +
  myTheme -> p1a

# Fig 1b: species AOH broken down by habitat proportions------------------------

# divide area by 1 million for easier comprehension
# use original Jung map colors
# it didn't include 7.1 (caves), so going to use gray for now
spAreas <- countsHT %>% 
  mutate(totalArea = areakm2/1000000) %>%
  mutate(habType = as.factor(habType))

# reorder species by total area
spAreas$species <- reorder(spAreas$species, spAreas$totalArea, sum)

# calculate habitat %s for labels
# barPercents <- spAreas %>% 
#   group_by(species) %>% 
#   mutate(tot = sum(areakm2)) %>% 
#   group_by(species, habType) %>% 
#   mutate(perc = round(areakm2/tot, 2)*100) %>% 
#   mutate(lab = paste0(perc, "%")) %>% 
#   mutate(lab = str_replace_all(lab, "^0%$", "<1%")) %>% 
#   dplyr::select(species, species, habType, perc, lab)

# show area proportions for habitat types within AOH
# to add lines between stacks, add color = "black" to geom_bar
ggplot(spAreas, aes(x = species, y = totalArea, fill = habType)) +
  geom_bar(stat = "identity", position = position_fill(reverse = T), 
           alpha = 0.8) +
  # geom_text(barPercents, mapping = aes(x = species, y = perc, label = lab),
  #           position = position_fill(reverse = T, vjust = 0.5),
  #           size = 2) +
  scale_fill_manual(values = myPal) +
  ylab("Proportion of AOH") + xlab("") +
  myTheme +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 4),
        axis.title.y = element_text(size = 4),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.margin = margin(t = 0, r = 1, b = 0, l = 1)) +
  guides(fill = guide_legend(nrow = 6, byrow = FALSE)) -> p1b

# Fig 1c: number of people within species AOH broken down by habitat------------

pplHT <- countsHT %>% 
  rename(people = people_WPCount) %>%
  mutate(people = people/1000000,
         habType = as.factor(habType))

# reorder species by area (to match first two panels)
pplHT$species <- reorder(pplHT$species, pplHT$areakm2, sum)

# calculate %s for labels
# pplPercents <- pplHT %>% 
#   group_by(species) %>% 
#   mutate(tot = sum(people)) %>% 
#   group_by(species, habType) %>% 
#   mutate(perc = round(people/tot, 2)*100) %>% 
#   mutate(lab = paste0(perc, "%")) %>% 
#   mutate(lab = str_replace_all(lab, "^0%$", "<1%")) %>% 
#   dplyr::select(species, habType, people, perc, lab)

# look at area % versus people % for carbonate rock outcrops
# comps <- left_join(barPercents, pplPercents, 
#                          by = c("species", "habType")) %>% 
#   filter(habType == "701") %>% 
#   mutate(pplGreater = ifelse(perc.y > perc.x, "yes", "no")) %>% 
#   dplyr::select(species, habType, perc.x, perc.y, pplGreater)

ggplot(pplHT, aes(x = species, y = people, fill = habType)) +
  geom_col(position = position_stack(reverse = T), 
           alpha = 0.8) +
  scale_fill_manual(values = myPal, labels = habLabs) +
  ylab("People in AOH (millions)") + xlab("") +
  myTheme +
  theme(legend.text = element_text(size = 5),
        legend.title = element_blank(),
        legend.key.size = unit(2, "mm"),
        legend.position = "bottom",
        legend.margin = margin(t = 0, r = 1, b = 0, l = 1),
        axis.text.x = element_text(face = "italic", angle = 90),
        axis.text.y = element_text(size = 4),
        axis.title.y = element_text(size = 4),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(t = 0, r = 1, b = 0, l = 1)) +
  scale_y_continuous(breaks = seq(0, 200, 25)) +
  guides(fill = guide_legend(nrow = 6, byrow = FALSE)) -> p1c

p1c_noleg <- p1c + 
  theme(legend.position = "none")

# plotting together-------------------------------------------------------------

# legend
myLegend <- get_legend(p1c)

# align all plots vertically
myPlots <- cowplot::align_plots(p1a, p1b, p1c_noleg, align = 'v', axis = 'l')

# put together the right column
right_col <- plot_grid(
  myPlots[[2]], myPlots[[3]], myLegend,
  labels = c("b", "c"), label_size = 7,
  rel_heights = c(0.6, 1, 0.4),
  ncol = 1
)

# combine left plot and right column
p1 <- plot_grid(myPlots[[1]], right_col, rel_widths = c(0.45, 0.55), 
                labels = c("a"), nrow = 1, label_size = 7)

ggsave(here("figures/Fig1.png"), p1, height = 65, width = 130, units = "mm",
       dpi = 300)

ggsave(here("figures/Fig1.pdf"), p1, height = 65, width = 130, units = "mm",  
       dpi = 300)
