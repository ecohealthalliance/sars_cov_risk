source(here("R/modifyFacets.R"))

load(here("data/lhsdata.rda"))
load(here("data/sobolIndices.rda"))

load(here("data/lhs_adjContact.rda"))
load(here("data/lhs_adjDetect.rda"))
load(here("data/lhs_adjContactDetect.rda"))

# density plots-----------------------------------------------------------------
data_combined <- bind_rows(
  lhsdata %>% 
    mutate(source = "full"),
  lhs_adjContact %>% 
    mutate(source = "contact"),
  lhs_adjDetect %>% 
    mutate(source = "detect"),
  lhs_adjContactDetect %>% 
    mutate(source = "both")
)

myPalette <- brewer.pal("YlGnBu", n = 5)[2:5]

ggplot(data_combined) +
  geom_density(aes(x = totInf, group = source, color = source), size = 2) +
  scale_color_manual(values = myPalette,
                     labels = rev(c("Original", "Adjusted contact", 
                                    "Adjusted detection",
                                    "Adjusted contact and detection")),
                     name = "") +
  scale_x_log10(breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 
                           100000000),
                labels = c(1, 10, 100, "1,000", "10,000", "100,000", "1e6", 
                           "1e7", "1e8"),
                name = "Total people infected annually with SARSr-CoVs in SE Asia") +
  ylab("Density") +
  theme_bw() +
  theme(legend.position = c(0.2, 0.75),
        legend.text = element_text(size = 12, color = "black"),
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black")) -> p3a

# inputs versus output----------------------------------------------------------

lhsLong <- lhsdata %>% 
  pivot_longer(nPeople:pPastyear, names_to = "inputVar", values_to = "value")

# facet labels
my_labeller <- as_labeller(c(nPeople = "N[people]~(millions)", 
                             pContact = "P[contact]", 
                             pDetect = "P[detect]",
                             pPastyear = "P[pastyear]"),
                           default = label_parsed)

ggplot(data = lhsLong) +
  geom_point(aes(x = value, y = totInf), color = myPalette[4], alpha = 0.1) +
  facet_wrap_custom(~inputVar, nrow = 1, scales = "free_x", 
                    labeller = my_labeller,
                    scale_overrides = list(
                      scale_override(1, scale_x_continuous(
                        breaks = seq(4e8, 5.8e8, length.out = 7), 
                        labels = seq(4e8, 5.8e8, length.out = 7)/1e6)))) +
  scale_y_continuous(name = "People infected annually (millions)",
                     breaks = seq(0, 4e7, length.out = 11),
                     labels = seq(0, 4e7, length.out = 11)/1e6) +
  theme_bw() +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14, color = "black"),
        strip.text = element_text(size = 12, color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) -> p3b

# sobol indices and CIs---------------------------------------------------------

ggplot(data = sobolIndices) +
  geom_point(aes(x = var, y = original), size = 2) +
  geom_errorbar(aes(x = var, ymin = `min. c.i.`, ymax = `max. c.i.`), 
                width = 0.2, size = 0.8) +
  scale_x_discrete(name = "", labels = c(expression(N[people]), 
                                         expression(P[contact]),
                                         expression(P[detect]),
                                         expression(P[pastyear]))) +
  scale_y_continuous(name = "Sensitivity index value", limits = c(0, 1)) +
  facet_wrap(~order, nrow = 1) +
  theme_bw() +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        strip.text = element_text(size = 12, color = "black")) -> p3c

# plot together-----------------------------------------------------------------

# slow because of number of points
p3 <- p3a / p3b / p3c +
  plot_annotation(tag_levels = 'a')  

ggsave(here("figures/Fig3.png"), p3, height = 11, width = 8.5, units = "in", 
       dpi = 300)
