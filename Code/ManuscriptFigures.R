# source("HareBodyCondition.R")

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

ggsave("../Results/ConcBySex.pdf", device = "pdf", dpi = 600, 
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

ggsave("../Results/ConcByKn.pdf", device = "pdf", dpi = 600, bg = "transparent",
       height = 8, width = 12)


#### Figure 3 in Manuscript ####
# A 6-panel plot: the first line of 3 plots shows concentrations of C, N, and P
# against estimated Age, coloured by Sex.
# The second line of 3 plots shows ratios of C:N, C:P, and N:P against estimated
# Age, coloured again by Sex
# This is a multi-step process.

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

# ggsave("../Results/RatiosBySexAndAge.pdf", device = "pdf", dpi = 600, 
#       bg = "transparent", height = 8, width = 12)
 

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

# ggsave("../Results/ConcBySexAndAgeContinuous.pdf", device = "pdf", dpi = 600, 
#       bg = "transparent", height = 8, width = 12)


# Step 4. Finally, combine the two plots above to create Figure 4
ConcStoich_plot <- ggarrange(concSexp, molRsexp, ncol = 1, nrow = 2, 
                             common.legend = TRUE, legend = "right")

ggsave("../Results/ConcStoichPlot.pdf", ConcStoich_plot,  device = "pdf", 
       dpi = 600, height = 8, width = 12)



##### SI FIGURES #####

# Figure S1
ggsave("../Results/SMIplots.pdf", smiplots, device = "pdf", dpi = 600, height = 10, width = 10)

# Figure S2
ggsave("../Results/C_explorer.pdf", cvar_boxplot, device = "pdf", dpi = 600, 
       height = 12, width = 8) 

# Figure S3
ggsave("../Results/N_explorer.pdf", nvar_boxplot, device = "pdf", dpi = 600, 
       height = 12, width = 8)

# Figure S4

knplot <- ggplot(HareMorphStoich_norep, 
                 aes(x = EstimatedAge, 
                     y = K_n, 
                     fill = Hare_Sex, 
                     color = Hare_Sex,
                     group = interaction(Hare_Sex, EstimatedAge))) + 
          geom_boxjitter(jitter.shape = 21,
                         jitter.size = 2,
                         jitter.alpha = 0.4,
                         outlier.shape = 24,
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

ablplot <- ggplot(HareMorphStoich_norep, 
                  aes(x = EstimatedAge, 
                      y = AvgBodyLength, 
                      fill = Hare_Sex, 
                      color = Hare_Sex,
                      group = interaction(Hare_Sex, EstimatedAge))) + 
           geom_boxjitter(jitter.shape = 21,
                          jitter.size = 2,
                          jitter.alpha = 0.4,
                          outlier.shape = 24,
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

bcivsage <- ggarrange(knplot, ablplot, ncol = 2, nrow = 1, labels = "auto", common.legend = TRUE, legend = "right")

ggsave(filename = "../Results/BCIvsAge.pdf", bcivsage, device = "pdf", dpi = 600, height = 6, width = 8)



#### Potentially another SI Figure ####

# Showing variability in P among the 5 samples that were run in triplicate
pvar_boxplot <- ggplot(data = pseudoreps, aes(x = SpecimenLabel, y = P)) + 
  geom_boxplot() + geom_jitter(fill = "red", size = 3, shape = 21) +
  coord_flip() + 
  ylab("%P") +
  xlab("Sample ID") +
  theme_minimal() 

ggsave("../Results/P_explorer.pdf", pvar_boxplot, device = "pdf", dpi = 600) 



##### An alternative way to produce Figure 4 #####

# I create 1 graph per contrast then combine them with ggarrange

# C:N vs Age

cnAgep <- ggplot(HareMorphStoich_norep, aes(x = EstimatedAge, y = CN_ratio)) + 
  geom_smooth(method = "glm", alpha = 0.2, size = 0, span = 0.5) +
  stat_smooth (method = "glm", geom="line", span=0.5, size = 1.5) + 
  geom_point(aes(fill = Hare_Sex), shape = 21, size = 4, alpha = 0.5) + 
  ylab("C:N") + 
  xlab("Estimated Age") + 
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
        strip.text = element_text(colour = "black"),
        aspect.ratio = 1)

# C:P vs Age

cpAgep <- ggplot(HareMorphStoich_norep, aes(x = EstimatedAge, y = CP_ratio)) + 
  geom_smooth(method = "glm", alpha = 0.2, size = 0, span = 0.5) +
  stat_smooth (method = "glm", geom="line", span=0.5, size = 1.5) + 
  geom_point(aes(fill = Hare_Sex), shape = 21, size = 4, alpha = 0.5) + 
  ylab("C:P") + 
  xlab(" ") + 
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
        strip.text = element_text(colour = "black"),
        aspect.ratio = 1)

# N:P vs Age

npAgep <- ggplot(HareMorphStoich_norep, aes(x = EstimatedAge, y = NP_ratio)) + 
  geom_smooth(method = "glm", alpha = 0.2, size = 0, span = 0.5) +
  stat_smooth (method = "glm", geom="line", span=0.5, size = 1.5) + 
  geom_point(aes(fill = Hare_Sex), shape = 21, size = 4, alpha = 0.5) + 
  ylab("N:P") + 
  xlab(" ") + 
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
        strip.text = element_text(colour = "black"),
        aspect.ratio = 1)

# C vs. Age

cAgep <- ggplot(HareConcentrations_norep, aes(x = EstimatedAge, y = C_mean)) + 
  geom_smooth(method = "glm", alpha = 0.2, size = 0, span = 0.5) +
  stat_smooth (method = "glm", geom="line", span=0.5, size = 1.5) + 
  geom_point(aes(fill = Hare_Sex), shape = 21, size = 4, alpha = 0.5) + 
  ylab("% C") + 
  xlab(" ") + 
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
        strip.text = element_text(colour = "black"),
        aspect.ratio = 1)

# N vs. Age

nAgep <- ggplot(HareConcentrations_norep, aes(x = EstimatedAge, y = N_mean)) + 
  geom_smooth(method = "glm", alpha = 0.2, size = 0, span = 0.5) +
  stat_smooth (method = "glm", geom="line", span=0.5, size = 1.5) + 
  geom_point(aes(fill = Hare_Sex), shape = 21, size = 4, alpha = 0.5) + 
  ylab("% N") + 
  xlab(" ") + 
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
        strip.text = element_text(colour = "black"),
        aspect.ratio = 1)

# P vs. Age

pAgep <- ggplot(HareConcentrations_norep, aes(x = EstimatedAge, y = P)) + 
  geom_smooth(method = "glm", alpha = 0.2, size = 0, span = 0.5) +
  stat_smooth (method = "glm", geom="line", span=0.5, size = 1.5) + 
  geom_point(aes(fill = Hare_Sex), shape = 21, size = 4, alpha = 0.5) + 
  ylab("% P") + 
  xlab(" ") + 
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
        strip.text = element_text(colour = "black"),
        aspect.ratio = 1)

sixpanelplot <- ggarrange(cAgep, nAgep, pAgep, cnAgep, cpAgep, npAgep, ncol = 3, nrow = 2, common.legend = TRUE, legend = "right", labels = "auto", font.label = list(size = 18, face = "plain", color = "black"))

sixpanelplot

# ggsave("../Results/SixPanelPlot.pdf", sixpanelplot, device = "pdf", dpi = 1200, height = 8, width = 12)


##### Outdated Plots #####

# NOTE: These plots DO NOT produce figure feature in the manuscript


# Boxplot showing variability in concentration by age group, coloured by Sex
# This is outdated as we have since moved on to use Age as a continuous 
# variable

ggplot(HareConcentrations_noreplong, aes(x = AgeGroup, y = Concentration)) + 
  geom_boxjitter(aes(fill = Hare_Sex, color = Hare_Sex), alpha = 0.4,
                 jitter.shape = 21,
                 jitter.height = 0, 
                 jitter.size = 4, jitter.alpha = 0.5,
                 outlier.shape = NA,
                 errorbar.draw = TRUE,
                 position = "dodge2") + 
  ylab("Concentration (%)\n") + 
  xlab("Age") + 
  theme_classic() +
  scale_fill_manual(name = "Sex", values = RdBl) + 
  scale_color_manual(name = "Sex", values = RdBl) + 
  scale_x_discrete(labels = c("<1", "1-2", "3-4",  ">4")) +
  theme(text = element_text(size = 20), 
        panel.grid.major = element_blank(), strip.background = element_blank(),
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.text=element_text(colour = "black"),
        legend.position = "top",
        strip.text = element_text(colour = "black")) + 
  facet_wrap(~Nutrient, scales = "free_y", labeller = as_labeller(labels_conc))

# ggsave("../Results/ConcentrationsBySexAndAge.pdf", device = "pdf", dpi = 600, 
#       bg = "transparent", height = 8, width = 12)



# Concentrations against Average Body Length

ggplot(HareConcentrations_noreplong, aes(x = AvgBodyLength, 
                                         y = Concentration)) +
  geom_ribbon(stat = "smooth", method = "glm", alpha = .15) +
  geom_line(stat = "smooth", method = "glm", size = 1.5) +
  geom_point(aes(fill = Hare_Sex, color = Hare_Sex), shape = 21, 
             size = 4, alpha = 0.5) + 
  ylab("Concentration (%)") + 
  xlab("Average Body Length (mm)") + 
  theme_classic() + 
  scale_fill_manual(name = "Sex", values = RdBl) + 
  scale_color_manual(name = "Sex", values = RdBl) + 
  theme(text = element_text(size = 20), 
        panel.grid.major = element_blank(), strip.background = element_blank(),
        axis.text.x = element_text(colour = "black"), 
        axis.title.y = element_text(margin = margin(l = 15)),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.text=element_text(colour = "black"),
        strip.text = element_text(colour = "black"))  + 
  facet_wrap(~Nutrient, scales = "free_y", labeller = as_labeller(labels_conc))

# ggsave("../Results/ConcBySexAndABL.pdf", device = "pdf", dpi = 600, 
#       bg = "transparent", height = 8, width = 12)

##### Figure 3 in Manuscript #####
# 3 panels showing %C vs %N, %C vs %P, and %N vs %P

# Step 1. Mean C concentration vs. Mean N concentration

cn_plot<- ggplot(HareConcentrations_norep, aes(x = C_mean, y = N_mean)) +
  geom_ribbon(stat = "smooth", method = "glm", alpha = .1) +
  geom_line(stat = "smooth", method = "glm", alpha = 0.75) +
  geom_point(aes(fill = Hare_Sex, color = Hare_Sex, shape = AgeGroup), 
             size = 4, alpha = 0.6) + 
  ylab("%N\n") + 
  xlab("\n%C") + 
  theme_classic() + 
  scale_colour_manual(name = "Sex", values = RdBl) +   
  scale_shape_manual(name = "Age", values = c(21, 22, 23, 24), 
                     labels = c("<1", "1-2", "3-4",  ">4")) +
  scale_fill_manual(name = "Sex", values = RdBl) +
  theme(text = element_text(size = 20), 
        panel.grid.major = element_blank(), strip.background = element_blank(),
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"), 
        axis.title.y = element_text(margin = margin(l = 15)),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.position = "none",
        strip.text = element_text(colour = "black"))


# Step 2. Mean C concentration vs. Mean P concentration

cp_plot <- ggplot(HareConcentrations_norep, aes(x = C_mean, y = P)) +
  geom_ribbon(stat = "smooth", method = "glm", alpha = .1) +
  geom_line(stat = "smooth", method = "glm", alpha = 0.75) +
  geom_point(aes(fill = Hare_Sex, color = Hare_Sex, shape = AgeGroup), 
             size = 4, alpha = 0.6) + 
  ylab("%P\n") + 
  xlab("\n%C") + 
  theme_classic() + 
  scale_colour_manual(name = "Sex", values = RdBl) +   
  scale_shape_manual(name = "Age", values = c(21, 22, 23, 24),
                     labels = c("<1", "1-2", "3-4",  ">4")) +
  scale_fill_manual(name = "Sex", values = RdBl) +
  theme(text = element_text(size = 20), 
        panel.grid.major = element_blank(), strip.background = element_blank(),
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        axis.title.y = element_text(margin = margin(l = 15)),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.position = "none",
        strip.text = element_text(colour = "black"))


# Step 3. Mean N concentration vs. P concentration

np_plot <- ggplot(HareConcentrations_norep, aes(x = N_mean, y = P)) +
  geom_ribbon(stat = "smooth", method = "glm", alpha = .1) +
  geom_line(stat = "smooth", method = "glm", alpha = 0.75) +
  geom_point(aes(fill = Hare_Sex, color = Hare_Sex, shape = AgeGroup), 
             size = 4, alpha = 0.6) + 
  ylab("%P\n") + 
  xlab("\n%N") + 
  theme_classic() + 
  scale_colour_manual(name = "Sex", values = RdBl) +   
  scale_shape_manual(name = "Age", values = c(21, 22, 23, 24),
                     labels = c("<1", "1-2", "3-4",  ">4")) +
  scale_fill_manual(name = "Sex", values = RdBl) +
  theme(text = element_text(size = 20), 
        panel.grid.major = element_blank(), strip.background = element_blank(),
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        axis.title.y = element_text(margin = margin(l = 15)),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.text=element_text(colour = "black"),
        # legend.position = "none",
        strip.text = element_text(colour = "black"))


# combine the plots above
comb_plot <- ggarrange(cn_plot, cp_plot, np_plot, ncol = 3, nrow = 1, 
                       common.legend = TRUE, legend = "right", labels = "AUTO", 
                       hjust = -8, font.label = list(size = 20, face = "plain", 
                                                     color = "black"))

# ggsave("../Results/CNP_vs_eachother.pdf", comb_plot, device = "pdf", dpi = 600, 
#        bg = "transparent", height = 8, width = 12)
