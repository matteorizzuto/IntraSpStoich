# This script estimates the body condition of snowshoe hares using
# morphometric data collected from 50 frozen specimen.

# I will calculate Body Condition using the Scaled Mass Index method 
# proposed by Peig & Green (2009, Oikos), which accounts for 
# allometric scaling of body length measurements over body weight. 

# The SMI is the body weight each individual would have if its chosen 
# length measurements was equal to the population average for the 
# length measurement

#------------------------------------#
#                 Setup              #
#------------------------------------#

# source("StoichAnalyses.R")

HareMorphStoich_norep$AvgBodyLength <- rowMeans(HareMorphStoich_norep[, 48:50])
HareMorphStoich_norep$AvgLeftHindFootLength <- rowMeans(HareMorphStoich_norep[,51:53])
HareMorphStoich_norep$AvgNeckCirc <- rowMeans(HareMorphStoich_norep[, 54:56])
HareMorphStoich_norep$AvgSkullLength <- rowMeans(HareMorphStoich_norep[, 57:59])
HareMorphStoich_norep$AvgSkullWidth <- rowMeans(HareMorphStoich_norep[, 60:62])

# Steps involved in obtaining the SMI

# 0. test if total body length is correlated to left hind foot length 
# 1. evaluate which measurement of length has the strongest correlation with body size
# 2. estimate the scaling exponent of the SMI (b_SMA) from a Standardized Major Axis regression of the body weight on the chosen length measurement
# 3. calculate the arithmetic mean of the chosen length measurement
# 4. calculate the SMI for the chosen sample

# Stpes 0 to 4
# 0. Finding the strongest correlate of body length
# Avg Body Length ~ Avg Left Hind Foor Length
ggplot(HareMorphStoich_norep, aes(x = AvgBodyLength, y = AvgLeftHindFootLength)) + stat_smooth(method = "lm", formula = y~x) + geom_point() + stat_poly_eq(parse=T, aes(label = ..eq.label..), formula=y~x)

LHF_model <- lm(AvgLeftHindFootLength ~ AvgBodyLength, data = HareMorphStoich_norep)
summary(LHF_model) # this seems to be the one

# Avg Body Length ~ Avg Skull Width
ggplot(HareMorphStoich_norep, aes(x = AvgBodyLength, y = AvgSkullWidth)) + stat_smooth(method = "lm", formula = y~x) + geom_point() + stat_poly_eq(parse=T, aes(label = ..eq.label..), formula=y~x)

SkW_model <- lm(AvgSkullWidth ~ AvgBodyLength, data = HareMorphStoich_norep)
summary(LHF_model)

# Avg Body Length ~ Avg Skull Length
ggplot(HareMorphStoich_norep, aes(x = AvgBodyLength, y = AvgSkullLength)) + stat_smooth(method = "lm", formula = y~x) + geom_point() + stat_poly_eq(parse=T, aes(label = ..eq.label..), formula=y~x)

SkL_model <- lm(AvgSkullLength ~ AvgBodyLength, data = HareMorphStoich_norep)
summary(LHF_model)

# 1. Check if Avg Left Hind Foot Length varies strongly with Body Size
LHF_BW_p <- ggplot(HareMorphStoich_norep, aes(x = log(AvgLeftHindFootLength), 
                                  y = log(Hare_Weight))) + 
  geom_point() +
  xlab("Ln Left Hind Foot length (cm)") +
  ylab("Ln Hare Body Weight (g)") +
  # theme_fira() +
  theme_classic() + 
  theme(text = element_text(size = 14), 
        panel.grid.major = element_blank(), strip.background = element_blank(), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        # axis.title.y = element_text(margin = margin(l = 15)),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.text=element_text(colour = "black"),
        legend.position = "top")


LHF_BW_model <- lm(log(AvgLeftHindFootLength) ~ log(Hare_Weight), data = HareMorphStoich_norep)
summary(LHF_BW_model)

SW_BW_p <- ggplot(HareMorphStoich_norep, aes(x = log(AvgSkullWidth), y = log(Hare_Weight))) + 
  geom_point() +
  xlab("Ln Skull width (cm)") +
  ylab("Ln Hare Body Weight (g)") +
  # theme_fira() +
  theme_classic() + 
  theme(text = element_text(size = 14), 
        panel.grid.major = element_blank(), strip.background = element_blank(), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        # axis.title.y = element_text(margin = margin(l = 15)),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.text=element_text(colour = "black"),
        legend.position = "top")
  
SW_BW_model <- lm(log(AvgSkullWidth) ~ log(Hare_Weight), data = HareMorphStoich_norep)
summary(SW_BW_model)

SL_BW_p <- ggplot(HareMorphStoich_norep, aes(x = log(AvgSkullLength), y = log(Hare_Weight))) +
  geom_point() +
  xlab("Ln Skull length (cm)") +
  ylab("Ln Hare Body Weight (g)") +
  # theme_fira() +
  theme_classic() + 
  theme(text = element_text(size = 14), 
        panel.grid.major = element_blank(), strip.background = element_blank(),
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        # axis.title.y = element_text(margin = margin(l = 15)),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.text=element_text(colour = "black"),
        legend.position = "top")

SL_BW_model <- lm(log(AvgSkullLength) ~ log(Hare_Weight), data = HareMorphStoich_norep)
summary(SL_BW_model)

ABL_BW_p <- ggplot(HareMorphStoich_norep, aes(x = log(AvgBodyLength), y = log(Hare_Weight))) +
  geom_point() +
  xlab("Ln Total Body length (cm)") +
  ylab("Ln Hare Body Weight (g)") +
  # theme_fira() +
  theme_classic() + 
  theme(text = element_text(size = 14), 
        panel.grid.major = element_blank(), strip.background = element_blank(), 
        axis.text.x = element_text(colour = "black"), 
        axis.text.y = element_text(colour = "black"),
        # axis.title.y = element_text(margin = margin(l = 15)),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.text=element_text(colour = "black"),
        legend.position = "top")

ABL_BW_model <- lm(log(AvgBodyLength) ~ log(Hare_Weight), data = HareMorphStoich_norep)
summary(ABL_BW_model)

# combine the 4 plots above
smiplots <- ggarrange(LHF_BW_p, SW_BW_p, SL_BW_p, ABL_BW_p, ncol = 2, nrow = 2, labels = "auto")

smiplots


# I will use Average Left Hind Food Length to estimate the scaling exponent for
# the SMI, by using a Standardize Major Axis regression on Body Weight

# 2. Calculate the SMA slope (SMI scaling exponent)
LHF_BW_sma <- sma(Hare_Weight ~ AvgLeftHindFootLength, data = HareMorphStoich_norep, log = "xy")
summary(LHF_BW_sma)
LHF_BW_sma

# test for outliers
qqnorm(residuals(LHF_BW_sma))
plot(LHF_BW_sma, which = "residual")
abline(a = 0, b = 0)

# 3. Get arithmetic mean of the chose length measurement (Left Hind Foot Length)
LHFmean <- mean(HareMorphStoich_norep$AvgLeftHindFootLength)

# 4. First try at calculating the SMI for moprhData
smi <- HareMorphStoich_norep$Hare_Weight*((LHFmean/HareMorphStoich_norep$AvgLeftHindFootLength)^LHF_BW_sma[["slopetest"]][[1]][["b"]])

# using a for loop to append the SMI value to morphData_long

HareMorphStoich_norep$SMI <- NA
bSMA <- LHF_BW_sma[["slopetest"]][[1]][["b"]]

for (i in 1:nrow(HareMorphStoich_norep)) {
  HareMorphStoich_norep$SMI[i] <- HareMorphStoich_norep$Hare_Weight[i]*((LHFmean/HareMorphStoich_norep$AvgLeftHindFootLength[i])^bSMA)
}

HareMorphStoich_norep <- HareMorphStoich_norep %>% dplyr::select("Date":"HareDryWeight", "SMI", "Age":"AvgSkullWidth")

# Calculating Relative Condition (K_n), i.e. the ratio of observed to expected weight (as def. by Stevenson and Wood, 2006)
HareMorphStoich_norep <- add_column(HareMorphStoich_norep, K_n = HareMorphStoich_norep$Hare_Weight/HareMorphStoich_norep$SMI, .after = "SMI")

# Boxplot comparing Wet Body Weight and SMI
ggplot(HareMorphStoich_norep, aes(x = AgeGroup, y = Hare_Weight, color = "Weight")) + geom_boxplot(alpha = 0) + geom_boxplot(aes(y = SMI, color = "SMI"), alpha = 0) + facet_grid(.~Hare_Sex) + geom_jitter(aes(color = "Weight")) + geom_jitter(aes(y = SMI, color = "SMI")) + scale_color_manual(name = " ", values = c(Weight = "black", SMI = "red")) + xlab("Age Group") + ylab("Weight (g)") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(HareMorphStoich_norep, aes(x = Hare_Weight, y = SMI, col = EstimatedAge)) + geom_point() + geom_abline(slope = 1, intercept = 1) + scale_color_distiller(palette = "BrBG")

summary(lm(SMI ~ Hare_Weight, data = HareMorphStoich_norep))


# transfer elemental ratios to HareMorphStoich_norep

# HareMorphStoich_norep$CN_ratio <- HareMolarRatios_norep$CN_ratio
# HareMorphStoich_norep$CP_ratio <- HareMolarRatios_norep$CP_ratio
# HareMorphStoich_norep$NP_ratio <- HareMolarRatios_norep$NP_ratio
#  
# is CN varying with SMI/BW ratio?

summary(lm(CN_ratio ~ K_n, data = HareMorphStoich_norep))
ggplot(HareMorphStoich_norep, aes(K_n, CN_ratio)) + geom_point() + stat_smooth(method = "lm")

summary(lm(CP_ratio ~ K_n, data = HareMorphStoich_norep))
ggplot(HareMorphStoich_norep, aes(K_n, CP_ratio)) + geom_point() + stat_smooth(method = "lm")

summary(lm(NP_ratio ~ K_n, data = HareMorphStoich_norep))
ggplot(HareMorphStoich_norep, aes(K_n, NP_ratio)) + geom_point() + stat_smooth(method = "lm")


# Relationship between Hare Body Weight and K_n
BW_Kn_lm <- lm(Hare_Weight ~ K_n, data = HareMorphStoich_norep)
summary(BW_Kn_lm)

ageshapes <- c(21, 22, 23, 24)

ggplot(HareMorphStoich_norep, aes(x = K_n, y = Hare_Weight)) + 
  geom_smooth (method = "lm", alpha=0.2, size=0, span=0.5) +
  geom_point(aes(fill = Hare_Sex, shape = AgeGroup), size = 5, alpha = .75) +
  stat_smooth (method = "lm", geom="line", span=0.5, size = 1.5) +
  ylab("Wet Body Weight (g)") + 
  labs(x=expression(~K[n])) +
  theme_classic() + 
  # scale_color_startrek(name = "Sex") + 
  scale_fill_startrek(name = "Sex", guide = "legend") + 
  scale_shape_manual(values = ageshapes, labels = c("Young (<1 y.o.)", "Juvenile (1-2 y.o.)", "Adult (3-4 y.o.)",  "Old (>6 y.o.)"), name = "Age") +
  theme(text = element_text(size = 30), panel.grid.major = element_blank(), strip.background = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(vjust = 2), plot.background = element_rect(fill = "transparent", colour = NA), legend.text = element_text(size = 22)) +
  guides(fill=guide_legend(override.aes=list(shape=21)))

# ggsave("~/Desktop/PhD/Talks&Posters/Images/BW_vs_Kn.png", device = "png", dpi = 300, bg = "transparent", height = 10, width = 12)




##### Using Skull Length #####
# Do result change if I calculate the SMI using Skull Length instead 
# of Left Hind Foot length?

# 2. Calculate the SMA slope (SMI scaling exponent)
Sk_BW_sma <- sma(Hare_Weight ~ AvgSkullLength, data = HareMorphStoich_norep, log = "xy")
summary(Sk_BW_sma)
Sk_BW_sma

# test for outliers
qqnorm(residuals(Sk_BW_sma))
plot(Sk_BW_sma, which = "residual")
abline(a = 0, b = 0)

# 3. Get arithmetic mean of the chose length measurement (Left Hind Foot Length)
Sk_mean <- mean(HareMorphStoich_norep$AvgSkullLength)

# 4. First try at calculating the SMI for morphData
sk_smi <- HareMorphStoich_norep$Hare_Weight*((Sk_mean/HareMorphStoich_norep$AvgSkullLength)^Sk_BW_sma[["slopetest"]][[1]][["b"]])

# using a for loop to append the SMI value to morphData_long

HareMorphStoich_norep$SkSMI <- NA
bSkSMA <- Sk_BW_sma[["slopetest"]][[1]][["b"]]

for (i in 1:nrow(HareMorphStoich_norep)) {
  HareMorphStoich_norep$SkSMI[i] <- HareMorphStoich_norep$Hare_Weight[i]*((Sk_mean/HareMorphStoich_norep$AvgSkullLength[i])^bSkSMA)
}

HareMorphStoich_norep <- HareMorphStoich_norep %>% dplyr::select("Date":"K_n", "SkSMI", "Age":"AvgSkullWidth")

# Calculating Relative Condition (K_n), i.e. the ratio of observed to expected weight (as def. by Stevenson and Wood, 2006)
HareMorphStoich_norep <- add_column(HareMorphStoich_norep, SkK_n = HareMorphStoich_norep$Hare_Weight/HareMorphStoich_norep$SkSMI, .after = "SkSMI")

# Boxplot comparing Wet Body Weight and SMI
ggplot(HareMorphStoich_norep, aes(x = AgeGroup, y = Hare_Weight, color = "Weight")) + geom_boxplot(alpha = 0) + geom_boxplot(aes(y = SkSMI, color = "SkSMI"), alpha = 0) + facet_grid(.~Hare_Sex) + geom_jitter(aes(color = "Weight")) + geom_jitter(aes(y = SkSMI, color = "SkSMI")) + scale_color_manual(name = " ", values = c(Weight = "black", SkSMI = "red")) + xlab("Age Group") + ylab("Weight (g)") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(HareMorphStoich_norep, aes(x = Hare_Weight, y = SkSMI, col = EstimatedAge)) + geom_point() + geom_abline(slope = 1, intercept = 1) + scale_color_distiller(palette = "BrBG")

summary(lm(SkSMI ~ Hare_Weight, data = HareMorphStoich_norep))

