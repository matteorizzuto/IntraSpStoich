# Define colorblind-safe palette for 2 groups
RdBl <- c("#ca0020","#0571b0")

# Create panel labels for later faceting
labels_conc <- c("C_mean" = "Carbon", "N_mean" = "Nitrogen", "P" = "Phosphorus")

# Create a separate dataset for concentrations only
HareConcentrations_norep <- 
  HareMorphStoich_norep %>% select(Date:Hare_Sex, AgeGroup, AvgBodyLength,
                                   C_mean, N_mean, P)

# Turn it from wide into long format
HareConcentrations_noreplong <- gather(HareConcentrations_norep, 
                                       key = Nutrient, 
                                       value = Concentration, C_mean:P, 
                                       factor_key = TRUE)


#### Figure 1 in Manuscript ####
# Faceted boxplot of the C, N, and P concentrations grouped by Sex

ggplot(HareConcentrations_noreplong, aes(x = Hare_Sex, y = Concentration)) + 
  geom_boxjitter(aes(fill = Hare_Sex, color = Hare_Sex), alpha = 0.5,
                 jitter.shape = 21, jitter.size = 4,
                 jitter.alpha = 0.6, 
                 outlier.shape = NA,
                 errorbar.draw = TRUE) + 
  ylab("Concentration (%)\n") + 
  xlab("") + 
  theme_classic() +
  scale_fill_manual(name = "Sex", values = RdBl) + 
  scale_color_manual(name = "Sex", values = RdBl) + 
  theme(text = element_text(size = 20), 
        panel.grid.major = element_blank(), strip.background = element_blank(),
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.text=element_text(colour = "black"),
        legend.position = "none",
        strip.text = element_text(colour = "black")) + 
  facet_wrap(~Nutrient, scales = "free_y", labeller = as_labeller(labels_conc))

ggsave("Figure1_ConcBySex.pdf", device = "pdf", dpi = 600, 
       bg = "transparent", height = 8, width = 12)


#### Figure 2 in Manuscript ####
# Faceted Concentrations vs. BMI plot

ggplot(HareConcentrations_noreplong, aes(x = K_n, y = Concentration)) +
  geom_ribbon(stat = "smooth", method = "glm", alpha = .1) +
  geom_line(stat = "smooth", method = "glm", alpha = 0.75) +
  geom_point(aes(fill = Hare_Sex, color = Hare_Sex), shape = 21, 
             size = 4, alpha = 0.6) + 
  ylab("Concentration (%)") + 
  xlab(bquote("Relative Body Condition ("~K[n]~")")) + 
  theme_classic() + 
  scale_fill_manual(name = "Sex", values = RdBl) + 
  scale_color_manual(name = "Sex", values = RdBl) + 
  theme(text = element_text(size = 20), 
        panel.grid.major = element_blank(), strip.background = element_blank(),
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        axis.title.y = element_text(margin = margin(l = 15)),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.text=element_text(colour = "black"),
        strip.text = element_text(colour = "black"))  + 
  facet_wrap(~Nutrient, scales = "free_y", labeller = as_labeller(labels_conc))

ggsave("Figure2_ConcByKn.pdf", device = "pdf", dpi = 600, bg = "transparent",
       height = 8, width = 12)


#### Figure 3 in Manuscript ####
# A 6-panel plot: the first line of 3 plots shows concentrations of C, N, and P
# against estimated Age, coloured by Sex.
# The second line of 3 plots shows ratios of C:N, C:P, and N:P against estimated
# Age, coloured again by Sex

# Note: This figure requires assembly: each line of three plots is a subplot
# produced independetly, then brought together with ggarrange.

# First subplot: Molar Ratios against Continuous Age, coloured by Sex

# Step 0. Create panel labels for later faceting
labels_ratios <- c("CN_ratio" = "C:N", "CP_ratio" = "C:P", "NP_ratio" = "N:P")

# Step 1. Create a long dataset for stoichiometric ratios
HareMorphStoich_ratios_long <- gather(HareMorphStoich_norep, key = Ratio, 
                                      value = Stoich_Ratio, 
                                      c(CN_ratio, CP_ratio, NP_ratio), 
                                      factor_key = TRUE)

# Step 2. Faceted plot of stoichiometric ratios against estimated Age, 
# coloured by Sex
molRsexp <- ggplot(HareMorphStoich_ratios_long, aes(x = EstimatedAge, 
                                                   y = Stoich_Ratio,
                                                   fill = Hare_Sex,
                                                   color = Hare_Sex)) + 
  geom_smooth(method = "glm", alpha = 0.1, size = 0) +
  stat_smooth (method = "glm", geom = "line", size = 1, alpha = 0.75) + 
  geom_point(aes(), 
             shape = 21, size = 4, alpha = 0.6, 
             position = position_jitter()) + 
  ylab("Ratio") + 
  xlab("Age") + 
  theme_classic() + 
  scale_fill_manual(name = "Sex", values = RdBl) + 
  scale_color_manual(name = "Sex", values = RdBl) + 
  # scale_x_discrete(labels = c("Young", "Juvenile", "Adult",  "Old")) +
  theme(text = element_text(size = 20), 
        panel.grid.major = element_blank(), strip.background = element_blank(),
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        # axis.title.y = element_text(margin = margin(l = 15)),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.text=element_text(colour = "black"),
        strip.text = element_text(colour = "black")) +
  facet_wrap(~Ratio, scales = "free_y", labeller = as_labeller(labels_ratios)) 

# Second subplot: Concentrations against Age continuous, coloured by Sex

# Step 3. Faceted plot of concentrations against estimated Age, 
# coloured by Sex
concSexp <- ggplot(HareConcentrations_noreplong, aes(x = EstimatedAge, 
                                                     y = Concentration,
                                                     fill = Hare_Sex, 
                                                     color = Hare_Sex)) +
  geom_smooth(method = "glm", alpha = 0.1, size = 0) +
  stat_smooth (method = "glm", geom="line", size = 1, alpha = 0.75) +
  geom_point(aes(), shape = 21, 
             size = 4, alpha = 0.6, position = position_jitter()) + 
  ylab("Concentration (%)") + 
  xlab(" ") + 
  theme_classic() + 
  scale_fill_manual(name = "Sex", values = RdBl) + 
  scale_color_manual(name = "Sex", values = RdBl) +  
  theme(text = element_text(size = 20, 
                            colour = "black"), 
        panel.grid.major = element_blank(), strip.background = element_blank(),
        plot.background = element_rect(fill = "transparent", colour = NA), 
        legend.text=element_text()) + 
  facet_wrap(~Nutrient, scales = "free_y", labeller = as_labeller(labels_conc))


# Step 4. Finally, combine the two plots above to create Figure 3
ConcStoich_plot <- ggarrange(concSexp, molRsexp, ncol = 1, nrow = 2, 
                             common.legend = TRUE, legend = "right")

ggsave("Figure3_ConcStoichPlot.pdf", ConcStoich_plot,  device = "pdf", 
       dpi = 600, height = 8, width = 12)



##### SI FIGURES #####

# Figure S1
ggsave("FigureS1_SMIplots.pdf", smiplots, device = "pdf", dpi = 600, height = 10, width = 10)

# Figure S2
ggsave("FigureS2_C_explorer.pdf", cvar_boxplot, device = "pdf", dpi = 600, 
       height = 12, width = 8) 

# Figure S3
ggsave("FigureS3_N_explorer.pdf", nvar_boxplot, device = "pdf", dpi = 600, 
       height = 12, width = 8)

# Figure S4 - Variability in P among the 5 samples that were run in triplicate
pvar_boxplot <- ggplot(data = pseudoreps, aes(x = SpecimenLabel, y = P)) + 
  geom_boxplot() + geom_jitter(fill = "red", size = 3, shape = 21) +
  coord_flip() + 
  ylab("Phosphorus content (%)") +
  xlab("Sample ID") +
  theme_minimal() 

ggsave("FigureS4_P_explorer.pdf", pvar_boxplot, device = "pdf", dpi = 600) 


# Figure S5
# Variation in relative body condition and average body length by age and sex

# Note: This figure requires assembly: each line of three plots is a subplot
# produced independetly, then brought together with ggarrange.

# First subplot: relative body condition against age, coloured by sex
knplot <- ggplot(HareMorphStoich_norep, 
                 aes(x = EstimatedAge, 
                     y = K_n, 
                     fill = Hare_Sex, 
                     color = Hare_Sex,
                     group = interaction(Hare_Sex, EstimatedAge))) + 
          geom_boxjitter(jitter.shape = 21,
                         jitter.size = 2,
                         jitter.alpha = 0.4,
                         outlier.shape = NA,
                         outlier.size = 2,
                         outlier.intersect = TRUE,
                         errorbar.draw = TRUE,
                         alpha = 0.4) + 
          xlab("Age") + 
          ylab("Relative Body Condition") +
          scale_fill_manual(name = "Sex", values = RdBl) + 
          scale_color_manual(name = "Sex", values = RdBl) + 
          theme(text = element_text(size = 20), 
                panel.grid.major = element_blank(), 
                strip.background = element_blank(),
                axis.text.x = element_text(colour = "black"), 
                axis.text.y = element_text(colour = "black"),
                plot.background = element_rect(fill = "transparent", 
                                               colour = NA),
                legend.text=element_text(colour = "black"),
                legend.position = "none",
                strip.text = element_text(colour = "black")) + 
          theme_classic()

# First subplot: average body length against age, coloured by sex
ablplot <- ggplot(HareMorphStoich_norep, 
                  aes(x = EstimatedAge, 
                      y = AvgBodyLength, 
                      fill = Hare_Sex, 
                      color = Hare_Sex,
                      group = interaction(Hare_Sex, EstimatedAge))) + 
           geom_boxjitter(jitter.shape = 21,
                          jitter.size = 2,
                          jitter.alpha = 0.4,
                          outlier.shape = NA,
                          outlier.size = 2,
                          outlier.intersect = TRUE,
                          errorbar.draw = TRUE,
                          alpha = 0.4) + 
           xlab("Age") + 
           ylab("Average Body Length") +
           scale_fill_manual(name = "Sex", values = RdBl) + 
           scale_color_manual(name = "Sex", values = RdBl) + 
           theme(text = element_text(size = 20), 
                 panel.grid.major = element_blank(), 
                 strip.background = element_blank(),
                 axis.text.x = element_text(colour = "black"), 
                 axis.text.y = element_text(colour = "black"),
                 plot.background = element_rect(fill = "transparent", 
                                                colour = NA),
                 legend.text=element_text(colour = "black"),
                 legend.position = "none",
                 strip.text = element_text(colour = "black")) +  
           theme_classic()

bcivsage <- ggarrange(knplot, ablplot, ncol = 2, nrow = 1, labels = "auto", 
						common.legend = TRUE, legend = "right")

ggsave(filename = "FigureS5_BCIvsAge.pdf", bcivsage, device = "pdf", 
		dpi = 600, height = 6, width = 8)