# denscomp provides a density plot of each fitted distribution 
# with the histogram of the data for continuous data. 

# assign some common settings
plotTheme <- theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),
        axis.title = element_text(color = "black", size = 12))

# Contact (original distribution)
denscomp(fitBetaContact, xlim = c(0, 1), ylim = c(0, 6), 
         xlab = expression(P[contact]~(all~estimates)), main = "",
         fitlty = 2, fitcol = "black", fitlwd = 1.5,
         plotstyle = "ggplot") + 
  annotate("text", label = paste0("Beta(", round(fitBetaContact$estimate[1], 4), 
                                  ", ", round(fitBetaContact$estimate[2], 4),
                                  ")"), 
           x = 0.65, y = 5.5, size = 4) +
  plotTheme -> p1

# Contact (adjusted distribution)
denscomp(fitBetaContact_B, xlim = c(0, 1), ylim = c(0, 7), 
         xlab = expression(P[contact]~(adjusted)), main = "",
         fitlty = 2, fitcol = "black", fitlwd = 1.5,
         plotstyle = "ggplot") + 
  annotate("text", label = paste0("Beta(", round(fitBetaContact_B$estimate[1], 4), 
                                  ", ", round(fitBetaContact_B$estimate[2], 4),
                                  ")"), 
           x = 0.65, y = 6.5, size = 4) +
  plotTheme -> p2

# Detect (original distribution)
denscomp(fitBetaDetect, xlim = c(0, 0.3), ylim = c(0, 20), 
         xlab = expression(P[detect]~(all~estimates)), main = "",
         fitlty = 2, fitcol = "black", fitlwd = 1.5,
         plotstyle = "ggplot") + 
  annotate("text", x = 0.2, y = 19, size = 4,
           label = paste0("Beta(", round(fitBetaDetect$estimate[1], 4), ", ", 
                 round(fitBetaDetect$estimate[2], 4), ")")) + 
  plotTheme -> p3

# Detect (adjusted distribution)
denscomp(fitBetaDetect_B, xlim = c(0, 0.3), ylim = c(0, 75), 
         xlab = expression(P[detect]~(adjusted)), main = "",
         fitlty = 2, fitcol = "black", fitlwd = 1.5,
         plotstyle = "ggplot") + 
  annotate("text", x = 0.2, y = 70, size = 4,
           label = paste0("Beta(", round(fitBetaDetect_B$estimate[1], 4), ", ", 
                          round(fitBetaDetect_B$estimate[2], 4), ")")) + 
  plotTheme -> p4


# put plots into one figure
figS4 <- (p1 + p2) / (p3 + p4) +
    plot_annotation(tag_levels = 'a')  


# Save png, fig size following Nature specs 89 mm
ggsave(here('figures/FigS4.png'), figS4, width = 6, height = 6, units = "in", 
       dpi = 300)
