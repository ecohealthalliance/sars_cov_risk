# Fig S1: comparison of species IUCN ranges to AOH

load(here("data/countsHT.rda"))
load(here("data/IUCNareas.rda"))

areaComp <- countsHT %>% 
  group_by(species) %>% 
  dplyr::summarise(areaAOH = sum(areakm2)) %>% 
  left_join(., IUCNareas) %>% 
  rename(AOH = areaAOH, IUCN = IUCNareas) %>% 
  pivot_longer(cols = -c(species), names_to = "type", 
               values_to = "area") %>% 
  mutate(species = fct_rev(species))

ggplot(areaComp, aes(x = species, y = area/1000000, group = type, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("gray70", "gray30"), 
                    guide = guide_legend(reverse = T),
                    name = "") +
  ylab(expression(paste("Total area (million km"^2, ")"))) + xlab("Species") +
  coord_flip() +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = c(0.85, 0.2))

# ggsave("figures/FigS1.tiff", width = 5, height = 3, units = "in", dpi = 300)

# calculate the % difference from IUCN to AOH
# for the text
areaDiff <- countsHT %>% 
  group_by(species) %>% 
  dplyr::summarise(areaAOH = sum(areakm2)) %>% 
  left_join(., IUCNareas) %>% 
  rename(AOH = areaAOH, IUCN = IUCNareas) %>% 
  mutate(diff = IUCN - AOH, pDiff = diff/IUCN) %>% 
  arrange(desc(pDiff))

median(areaDiff$pDiff)
