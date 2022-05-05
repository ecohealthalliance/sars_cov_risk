# Fig. S2: human density within each species AOH

load(here("data/countsHT.rda"))

spCounts <- countsHT %>% 
  group_by(species) %>% 
  dplyr::summarise(totArea = sum(areakm2), peopleWP = sum(people_WPCount)) %>% 
  mutate(dens = peopleWP/totArea)

# reorder species by highest number of people
spCounts$species <- reorder(spCounts$species, spCounts$dens)

ggplot(spCounts) +
  geom_bar(aes(x = species, y = dens), stat = "identity") +
  ylab(expression(paste("Human population density (per km"^2, ") within AOH"))) + 
  xlab("Species") +
  coord_flip() +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        axis.title.x = element_text(size = 9),
        axis.text.y = element_text(face = "italic"),
        axis.text = element_text(color = "black"))

ggsave("figures/FigS3.png", width = 5, height = 4.5, units = "in", dpi = 300)
