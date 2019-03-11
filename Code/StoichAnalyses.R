# This script reshapes the datasets for C and N concentrations in our snowshoe 
# hare samples, merges them into a single dataset and performs summary stats 
# on them

#### 1. Data Import ####

# import the morphometric data
morphData <- read.csv("../Data/HH_MorphRawData.csv", header = TRUE)

# remove the first entry, which was a test specimen
morphData <- subset(morphData, SpecimenLabel != "Test_1")

# import the DNA Sex assignments
dnaData <- read.csv("../Data/DNA_SexAssignements.csv", header = TRUE)
# each column in this dataset correspond to a different set of sex assignments

# replacing the visual sexing in the morphometric data with the dna sexing
# morphData$Hare_Sex <- dnaData$Hare_Sex_Dec5wVis # Dec 5 assignments with Visual sex fill-ins
# morphData$Hare_Sex <- dnaData$Hare_Sex_Feb12nr # this includes 3 no reactions
morphData$Hare_Sex <- dnaData$Hare_Sex_Feb12 # this assign sexes from Dec 5 to the 3 no reactions of Feb 12



#### 2. Create wide dataset ####
# The chemical compostion datasets from AFL are in the long format, but 
# the majority of the analyses we will do needs a wide dataset. 

# Carbon Data
c_data_long <- read.csv("../Data/SSH_C_data.csv", header = TRUE)
c_data <- spread(c_data_long, Replicate, C)

# rename columns to identify replicates
c_data <- rename(c_data, "C_rep_1" = "1")
c_data <- rename(c_data, "C_rep_2" = "2")
c_data <- rename(c_data, "C_rep_3" = "3")

# Nitrogen Data
n_data_long <- read.csv("../Data/SSH_N_data.csv", header = TRUE)
n_data <- spread(n_data_long, Replicate, N)

# rename columns to identify replicates
n_data <- rename(n_data, "N_rep_1" = "1")
n_data <- rename(n_data, "N_rep_2" = "2")
n_data <- rename(n_data, "N_rep_3" = "3")

# Phosphorus data
p_data <- read.csv("../Data/SSH_P_data.csv", header = TRUE)



#### 3. Exploratory plots ####
# Explore the distribution of our chemical composition data

# C 
cvar_boxplot <- ggplot(c_data_long, aes(x = SubmitterSampleID, y = C)) + 
  geom_boxplot() + geom_jitter(fill = "red", size = 1.5, shape = 21) +   
  coord_flip() + 
  ylab("Carbon content (%)") + 
  xlab("Sample ID")+
  theme_minimal()

cvar_boxplot

# N
nvar_boxplot <- ggplot(n_data_long, aes(x = SubmitterSampleID, y = N)) + 
  geom_boxplot() + geom_jitter(fill = "red", size = 1.5, shape = 21) +   
  coord_flip() + 
  ylab("Nitrogen content (%)") + 
  xlab("Sample ID") +
  theme_minimal()

nvar_boxplot

# P
ggplot(p_data, aes(x = P)) + geom_density()



#### 4. Summary stats ####
# Since AFL ran each sample in triplicate for C and N, we will calculate the 
# mean, median, and SD values for their concentrations
# They did not run P in triplicate, so there is no need to produce these 
# summary statistics for this element

# C content 
c_data$C_mean <- apply(c_data[,3:5], 1, mean)

c_data$C_median <- apply(c_data[,3:5], 1, median)

c_data$C_sd <- apply(c_data[,3:5], 1, sd)

# N content 
n_data$N_mean <- apply(n_data[,3:5], 1, mean)

n_data$N_median <- apply(n_data[,3:5], 1, median)

n_data$N_sd <- apply(n_data[,3:5], 1, sd)


#### 5. Merging the Morphometrics and Stoichiometric Datasets ####
# While sampling, to control for variability in our sampling, we collected 
# 3 samples each from 5 random specimens and included them in the shipment to 
# AFl. However, for the rest of our analyses, we will need a single value for
# each specimen. This section deals with merging the morphometric and 
# stoichiometric datasets, as well as removing the pseudoreplicates we produced
# in the laboratory.
# 
# First, in order to append the stoichiometric data to the morphometric dataset
# both datasets need to be in the long rather than wide format, so I 
# will convert the morphometric to a long format. To do so, the first step is 
# to subset the original triplicate samples from the rest (creating 2 datasets)
# and convert those from wide to long. Then I will merge the non-triplicate 
# dataset with the triplicate one, obtaining one long-format morphometric 
# dataset. 
# 
# After appending the stoichiometric data, I will then remove the 
# pseudoreplicated entries by taking the mean measurements. This will produce a
# single dataset containing morphometric and stoichiometric data for 50 
# snowshoe hares.


# Step 1. COnvert the morphometric dataset from wide to long 

# Create a dataset containing only laboratory-produced triplicates
morphData_origTripl <- subset(morphData, (!is.na(WeightFinalSample_B)) & (!is.na(WeightFinalSample_C)))
morphData_origTripl <- droplevels(morphData_origTripl)

# And a dataset for the rest of the samples
morphData_noTripl <- subset(morphData, (is.na(WeightFinalSample_B)) & (is.na(WeightFinalSample_C)))
morphData_noTripl <- droplevels(morphData_noTripl)

# I want 1 entry for each pseudoreplicate, so I need to melt the original
# triplicate dataset into long format from wide format
morphData_molten <- melt(morphData_origTripl, measure.vars = c("WeightFinalSample_A", "WeightFinalSample_B", "WeightFinalSample_C", "SampleDryWeight_A", "SampleDryWeight_B", "SampleDryWeight_C"), variable.name = "Replicate", value.name = "SampleWetWeight")

# separate the 'dry weight' dataset from the 'wet weight' one
dry_morphData_molten <- morphData_molten[morphData_molten$Replicate=="SampleDryWeight_A"|morphData_molten$Replicate=="SampleDryWeight_B"|morphData_molten$Replicate=="SampleDryWeight_C",]

colnames(dry_morphData_molten)[ncol(dry_morphData_molten)-1] <- "DryReplicate"
colnames(dry_morphData_molten)[ncol(dry_morphData_molten)] <- "SampleDryWeight"

wet_morphData_molten <- morphData_molten[morphData_molten$Replicate=="WeightFinalSample_A"|morphData_molten$Replicate=="WeightFinalSample_B"|morphData_molten$Replicate=="WeightFinalSample_C",]

# combine the 'dry weight' and the 'wet weight' datasets into a single one 
# with 3 entries for sample (as per original triplicates)
morphData_molten_join <- cbind(wet_morphData_molten, dry_morphData_molten[!names(dry_morphData_molten) %in% names(wet_morphData_molten)])

# rename each sample to show the replicate (i.e., TCH_037 -> TCH_037_A, _B, _C)
morphData_molten_join <- droplevels(morphData_molten_join)
morphData_molten_join$SpecimenLabel <- as.character(morphData_molten_join$SpecimenLabel)
morphData_molten_join["1","SpecimenLabel"] <- "TCH037_A"
morphData_molten_join["2","SpecimenLabel"] <- "TCH040_A"
morphData_molten_join["3","SpecimenLabel"] <- "TCH042_A"
morphData_molten_join["4","SpecimenLabel"] <- "TCH045_A"
morphData_molten_join["5","SpecimenLabel"] <- "TCH048_A"
morphData_molten_join["6","SpecimenLabel"] <- "TCH037_B"
morphData_molten_join["7","SpecimenLabel"] <- "TCH040_B"
morphData_molten_join["8","SpecimenLabel"] <- "TCH042_B"
morphData_molten_join["9","SpecimenLabel"] <- "TCH045_B"
morphData_molten_join["10","SpecimenLabel"] <- "TCH048_B"
morphData_molten_join["11","SpecimenLabel"] <- "TCH037_C"
morphData_molten_join["12","SpecimenLabel"] <- "TCH040_C"
morphData_molten_join["13","SpecimenLabel"] <- "TCH042_C"
morphData_molten_join["14","SpecimenLabel"] <- "TCH045_C"
morphData_molten_join["15","SpecimenLabel"] <- "TCH048_C"

# Reorder columns in new joined dataset
morphData_molten_join = morphData_molten_join %>% dplyr::select(Date:AgeRange, SampleWetWeight, SampleDryWeight, Hare_Sex:Comments, Replicate, DryReplicate)

# label them for later reference
triplData <- morphData_molten_join %>% dplyr::select(Date:Comments) %>% mutate(Label = "Pseudoreplicate")

# Remove unused columns in morphData_noTripl, rename the ones still in use, 
# and label the 45 unreplicated data as "Originaldata"
notriplData <- morphData_noTripl %>% dplyr::select(Date:AgeRange, WeightFinalSample_A, SampleDryWeight_A, Hare_Sex:NumberOfChunks, TotalBodyLength_1:Comments) %>% mutate(Label = "Originaldata")
names(notriplData)[names(notriplData) == "WeightFinalSample_A"] <- "SampleWetWeight"
names(notriplData)[names(notriplData) == "SampleDryWeight_A"] <- "SampleDryWeight"

# Bind the two dataframes, notriplData and triplData
morphData_long <- rbind(notriplData,triplData)

# Bind the morphometric, C, N and P datasets, and creating a shorter dataset
morphStoicData <- cbind(morphData_long, c_data[,3:8], n_data[,3:8], p_data[,3])
colnames(morphStoicData)[49] <- "P"

# reorder variables in the stoich + morph dataset, and store in a new dataset
# for later use 
HareMorphStoich <- morphStoicData %>% dplyr::select(Date:Hare_Sex, C_rep_1:P, NumberOfChunks:SkullWidth_3, Label)

# categorize Estimated Age to create boxplots

HareMorphStoich$EstimatedAge <- as.numeric(HareMorphStoich$EstimatedAge)

HareMorphStoich <- mutate(HareMorphStoich, AgeGroup = ifelse(EstimatedAge == "0", "Young", ifelse(EstimatedAge == "1" | EstimatedAge == "2", "Juvenile", ifelse(EstimatedAge == "3" | EstimatedAge == "4", "Adult", "Old"))))


# Step 2. Remove pseudoreplicates from HareMorphStoich
# e.g., convert "TCH037_A", "TCH037_B" and "TCH037_C" back to a single "TCH037" entry

# copy pseudoreplicates from non-pseudoreplicates
pseudoreps <- subset(HareMorphStoich, HareMorphStoich$Label == "Pseudoreplicate")
pseudoreps <- droplevels(pseudoreps)

# add replication index, i.e. separate the "_A", "_B, "_C" labels from the 
# Specimen Label (e.g., "TCH037")
pseudoreps <- dplyr::select(pseudoreps, Date, Observers:AgeGroup, SpecimenLabel) %>% separate(SpecimenLabel, into = paste("L", 1:2, sep = "_")) %>% dplyr::rename_at("L_1", ~"SpecimenLabel") %>% dplyr::rename_at("L_2", ~"RepIndex") %>% dplyr::select(Date, SpecimenLabel:RepIndex, Observers:AgeGroup)

# I will use a loop to iteratively select each specimen and its 
# pseudoreplicates, store them in a temporary dataset, take the mean of each 
# numerical variable and store it in a new entry, then copy non-numerical 
# information to this entry, and finally copy this "summary" entry in a new 
# dataset that I will later merge with the non-pseudoreplicated one

# create a vector of unique specimen label values for indexing in the for loop
pseudonames <- unique(pseudoreps$SpecimenLabel)

# create a vector of names for the loop's temporary dataset 
OutputFields <- colnames(pseudoreps)

# allocate temporary empty dataframe to store clean pseudoreps
cleanpseudo <- setNames(data.frame(matrix(ncol = length(OutputFields), nrow = 0)), OutputFields)

# use a loop to isolate each individual specimen, take the mean of each 
# numerical variable, copy non-numerical variables, and store in cleanpseudo

for (i in 1:length(pseudonames)){
  tempID <- which(pseudoreps$SpecimenLabel == pseudonames[i]) # select the unique specimen labels matching the current label correpsonding to i
  
  temp <- pseudoreps[tempID,] # select the entries in pseudoreps equal to the temp ID

  temp <- rbind(temp, as.data.frame(lapply(temp, function(x) if (!is.numeric(x)) NA else mean(x)))) # for all numeric columns compute the mean, for all non numeric column enter NA

  # fix the NAs introduced in the previous step by copying information from 
  # 1st to 4th row of temp
  temp[4,"Date"] <- temp[1,"Date"] 
  temp[4,"SpecimenLabel"] <- temp[1,"SpecimenLabel"]
  temp[4,"Observers"] <- temp[1,"Observers"]
  temp[4,"Age"] <- temp[1,"Age"]
  temp[4,"EstimatedAge"] <- temp[1,"EstimatedAge"]
  temp[4,"AgeRange"] <- temp[1,"AgeRange"]
  temp[4,"Hare_Sex"] <- temp[1,"Hare_Sex"]
  temp[4,"AgeGroup"] <- temp[1,"AgeGroup"]
  
  # store the new entry containing the mean values in a new line in the predisposed dataframe
  cleanpseudo <- rbind(cleanpseudo, temp[4,])
  # browser()
}

# create a new HareMorphStoich dataset with no pseudoreplicates
temp_norep <- HareMorphStoich[-c(46:60),] # remove unnecessary rows from the rest of the dataframe
cleanpseudo <- dplyr::select(cleanpseudo, Date:SpecimenLabel, Observers:AgeGroup) # remove unnecessary colums from the newly clean pseudoreps

# combine the clean pseudoreplicates value with the non-pseudoreplicated
# dataframe
HareMorphStoich_norep <- rbind(temp_norep,cleanpseudo)
HareMorphStoich_norep <- droplevels(HareMorphStoich_norep)
HareMorphStoich_norep$Label <- NULL # remove Label variable
levels(HareMorphStoich_norep$SpecimenLabel) # check that all specimen are there


#### 6. Obtaining Molar Ratios ####

# To calculate molar ratios for C, N, P, I need the dry weight of each hare.
# This I can get by calculating the percentage of water content in my samples
# and using this to calculate the "wet" fraction of a hare body weight

# calculate water content of each sample
HareMorphStoich_norep <- add_column(HareMorphStoich_norep, SampleWaterContent = (HareMorphStoich_norep$SampleWetWeight - HareMorphStoich_norep$SampleDryWeight), .before = "Hare_Sex")

# calcualte the percentage of water in each sample
HareMorphStoich_norep <- add_column(HareMorphStoich_norep, PercWaterContent = HareMorphStoich_norep$SampleWaterContent/HareMorphStoich_norep$SampleWetWeight, .after = "SampleWaterContent")

# use the percentage of water in each sample to get a hare's dry weight with 
# the formula:
# hare dry weight = hare wet weight - (hare wet weight * percent water content)
HareMorphStoich_norep <- add_column(HareMorphStoich_norep, HareDryWeight = HareMorphStoich_norep$Hare_Weight - (HareMorphStoich_norep$Hare_Weight*HareMorphStoich_norep$PercWaterContent), .after = "Hare_Weight")

# calculate molar ratios for C and N from mean % of C and N in samples and hare dry weight
# atomic weight are the 2013 standard values from https://doi.org/10.1515/pac-2015-0305
C_atomic <- 12.011
N_atomic <- 14.007
P_atomic <- 30.974

# Carbon Molar Ratio
# C Mean
HareMorphStoich_norep <- add_column(HareMorphStoich_norep, C_dry_mean = ((HareMorphStoich_norep$HareDryWeight*HareMorphStoich_norep$C_mean)/100), .after = "C_sd")
HareMorphStoich_norep <- add_column(HareMorphStoich_norep, C_molar_mean = (HareMorphStoich_norep$C_dry_mean/C_atomic), .after = "C_dry_mean")
# C Rep 1
HareMorphStoich_norep <- add_column(HareMorphStoich_norep, C_dry_1 = ((HareMorphStoich_norep$HareDryWeight*HareMorphStoich_norep$C_rep_1)/100), .after = "C_rep_1")
HareMorphStoich_norep <- add_column(HareMorphStoich_norep, C_molar_1 = (HareMorphStoich_norep$C_dry_1/C_atomic), .after = "C_dry_1")
# C Rep 2
HareMorphStoich_norep <- add_column(HareMorphStoich_norep, C_dry_2 = ((HareMorphStoich_norep$HareDryWeight*HareMorphStoich_norep$C_rep_2)/100), .after = "C_rep_2")
HareMorphStoich_norep <- add_column(HareMorphStoich_norep, C_molar_2 = (HareMorphStoich_norep$C_dry_2/C_atomic), .after = "C_dry_2")
# C Rep 3
HareMorphStoich_norep <- add_column(HareMorphStoich_norep, C_dry_3 = ((HareMorphStoich_norep$HareDryWeight*HareMorphStoich_norep$C_rep_3)/100), .after = "C_rep_3")
HareMorphStoich_norep <- add_column(HareMorphStoich_norep, C_molar_3 = (HareMorphStoich_norep$C_dry_3/C_atomic), .after = "C_dry_3")
# C Variance
HareMorphStoich_norep <- add_column(HareMorphStoich_norep, C_molar_var = apply(HareMorphStoich_norep[,c(17, 20, 23)], 1, var), .after = "C_molar_mean")

# Nitrogen Molar Ratio
# N Mean
HareMorphStoich_norep <- add_column(HareMorphStoich_norep, N_dry_mean = ((HareMorphStoich_norep$HareDryWeight*HareMorphStoich_norep$N_mean)/100), .after = "N_sd")
HareMorphStoich_norep <- add_column(HareMorphStoich_norep, N_molar_mean = (HareMorphStoich_norep$N_dry_mean/N_atomic), .after = "N_dry_mean")
# N Rep 1
HareMorphStoich_norep <- add_column(HareMorphStoich_norep, N_dry_1 = ((HareMorphStoich_norep$HareDryWeight*HareMorphStoich_norep$N_rep_1)/100), .after = "N_rep_1")
HareMorphStoich_norep <- add_column(HareMorphStoich_norep, N_molar_1 = (HareMorphStoich_norep$N_dry_1/N_atomic), .after = "N_dry_1")
# N Rep 2
HareMorphStoich_norep <- add_column(HareMorphStoich_norep, N_dry_2 = ((HareMorphStoich_norep$HareDryWeight*HareMorphStoich_norep$N_rep_2)/100), .after = "N_rep_2")
HareMorphStoich_norep <- add_column(HareMorphStoich_norep, N_molar_2 = (HareMorphStoich_norep$N_dry_2/N_atomic), .after = "N_dry_2")
# N Rep 3
HareMorphStoich_norep <- add_column(HareMorphStoich_norep, N_dry_3 = ((HareMorphStoich_norep$HareDryWeight*HareMorphStoich_norep$N_rep_3)/100), .after = "N_rep_3")
HareMorphStoich_norep <- add_column(HareMorphStoich_norep, N_molar_3 = (HareMorphStoich_norep$N_dry_3/N_atomic), .after = "N_dry_3")
# N Variance
HareMorphStoich_norep <- add_column(HareMorphStoich_norep, N_molar_var = apply(HareMorphStoich_norep[,c(32, 35, 38)], 1, var), .after = "N_molar_mean")

# Phosphorus Molar Ratio
HareMorphStoich_norep <- add_column(HareMorphStoich_norep, P_dryweight = ((HareMorphStoich_norep$HareDryWeight*HareMorphStoich_norep$P)/100), .after = "P")
HareMorphStoich_norep <- add_column(HareMorphStoich_norep, P_molar = (HareMorphStoich_norep$P_dryweight/P_atomic), .after = "P_dryweight")


# Calculate elemental ratios (C:N, C:P, N:P)
HareMorphStoich_norep$CN_ratio <- HareMorphStoich_norep$C_molar_mean/HareMorphStoich_norep$N_molar_mean
HareMorphStoich_norep$CP_ratio <- HareMorphStoich_norep$C_molar_mean/HareMorphStoich_norep$P_molar
HareMorphStoich_norep$NP_ratio <- HareMorphStoich_norep$N_molar_mean/HareMorphStoich_norep$P_molar


#### 7. Exploratory Plots of Molar Ratios ####

# Density Distribution of the molar ratios
# HareMolarRatios <- HareMorphStoich %>% dplyr::select(Date:Hare_Sex, AgeGroup, C_molar_mean, N_molar_mean, P_molar)
HareMorphStoich_norep_long <- gather(HareMorphStoich_norep, key = Nutrient, value = Molar_Ratio, c(C_molar_mean, N_molar_mean, P_molar), factor_key = TRUE)

ggplot(HareMorphStoich_norep_long, aes(x = Molar_Ratio)) + geom_histogram(aes(y = ..density.., group = Nutrient, fill = Nutrient), bins = 35, binwidth = .5) + xlab("Molar Ratio") + ylab("") + theme_classic() + scale_fill_startrek(labels = c("C", "N", "P")) + theme(text = element_text(size = 20))

# ggsave("../Results/MolarRatiosHist.pdf", device = "pdf", dpi = 300)

# boxplot by age
labels <- c("C_molar_mean" = "Carbon", "N_molar_mean" = "Nitrogen", "P_molar" = "Phosphorus")
ggplot(HareMorphStoich_norep_long, aes(x = fct_reorder(AgeGroup, EstimatedAge), y = Molar_Ratio)) + geom_boxplot(aes(fill = AgeGroup, color = AgeGroup), alpha = .25) + scale_fill_startrek(name = "AgeGroup", guide = FALSE) + scale_color_startrek(guide = FALSE) + ylab("Molar Ratio") + xlab("") + theme_gdocs() + theme(text = element_text(size = 20), panel.grid.major = element_blank(), strip.background = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) + facet_wrap(~Nutrient, scales = "free_y", labeller = as_labeller(labels))

# boxplot by sex
ggplot(HareMorphStoich_norep_long, aes(x = Hare_Sex, y = Molar_Ratio)) + geom_boxplot(aes(fill = Hare_Sex, color = Hare_Sex), alpha = .5) + scale_fill_startrek(name = "Sex", guide = FALSE) + scale_color_startrek(guide = FALSE) + ylab("Molar Ratio") + xlab("") + theme_gdocs() + theme(text = element_text(size = 20), panel.grid.major = element_blank(), strip.background = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) + facet_wrap(~Nutrient, scales = "free_y", labeller = as_labeller(labels))



##### Allometry in Stoichiometric Ratios #####

# Following Eric's suggestion, I will first test which element has the 
# highest variance, and then fit a weight linear model to the data, using
# the inverse of the variance as weights for the data points 

# Checking which element has the highest variance
# For C:N 
var(HareMorphStoich_norep$C_molar_mean)
var(HareMorphStoich_norep$N_molar_mean)
var.test(HareMorphStoich_norep$C_molar_mean, HareMorphStoich_norep$N_molar_mean, alternative = "two.sided")

# For C:P
var(HareMorphStoich_norep$C_molar_mean)
var(HareMorphStoich_norep$P_molar)
var.test(HareMorphStoich_norep$C_molar_mean, HareMorphStoich_norep$P_molar, alternative = "two.sided")

# For N:P
var(HareMorphStoich_norep$N_molar_mean)
var(HareMorphStoich_norep$P_molar)
var.test(HareMorphStoich_norep$N_molar_mean, HareMorphStoich_norep$P_molar, alternative = "two.sided")

#### C:N ratio plots ####
# Dry Weight
ggplot(HareMorphStoich_norep, aes(x = HareDryWeight, y = CN_ratio)) + geom_point(aes(fill = Hare_Sex), size = 5, shape = 21) + stat_smooth(data = subset(HareMorphStoich_norep, Hare_Sex != "Unknown"), aes(group = Hare_Sex, colour = Hare_Sex, weight = 1/C_molar_var), method = "lm", alpha = 0.15) + scale_fill_startrek(name = "Sex") + scale_colour_startrek(guide = FALSE) + ylab("C:N Ratio") + xlab("Snowshoe Hare Dry Weight (g)") + theme_gdocs() + theme(text = element_text(size = 20), panel.grid.major = element_blank())

# ggsave("../Results/CNratioSex.pdf", device = "pdf", dpi = 300, height = 6, width = 8)

cn_model <- lm(CN_ratio ~ HareDryWeight, data = HareMorphStoich_norep, subset = HareMorphStoich_norep$Hare_Sex == "Male", weights = 1/C_molar_var)
summary(cn_model)

cn_model_f <- lm(CN_ratio ~ HareDryWeight, data = HareMorphStoich_norep, subset = HareMorphStoich_norep$Hare_Sex == "Female", weights = 1/C_molar_var)
summary(cn_model_f)

# Wet Weight
ggplot(HareMorphStoich_norep, aes(x = Hare_Weight, y = CN_ratio)) + geom_point(aes(fill = Hare_Sex), size = 5, shape = 21) + stat_smooth(data = subset(HareMorphStoich_norep, Hare_Sex != "Unknown"), aes(group = Hare_Sex, color = Hare_Sex, weight = 1/C_molar_var), method = "lm", alpha = 0.15) + scale_fill_startrek(name = "Sex") + scale_color_startrek(guide = FALSE) + ylab("C:N Ratio") + xlab("Snowshoe Hare Wet Weight (g)") + theme_gdocs() + theme(text = element_text(size = 20), panel.grid.major = element_blank())

cn_model_wet <- lm(CN_ratio ~ Hare_Weight, data = HareMorphStoich_norep, subset = HareMorphStoich_norep$Hare_Sex == "Male", weight = 1/C_molar_var)
summary(cn_model_wet)

cn_model_f_wet <- lm(CN_ratio ~ Hare_Weight, data = HareMorphStoich_norep, subset = HareMorphStoich_norep$Hare_Sex == "Female", weight = 1/C_molar_var)
summary(cn_model_f_wet)

#### C:P Ratio Plots ####
# Dry Weight
ggplot(HareMorphStoich_norep, aes(x = HareDryWeight, y = CP_ratio)) + geom_point(aes(fill = Hare_Sex), size = 5, shape = 21) + stat_smooth(data = subset(HareMorphStoich_norep, Hare_Sex != "Unknown"),aes(group = Hare_Sex, color = Hare_Sex, weight = 1/C_molar_var), method = "lm", alpha = 0.15) + scale_color_startrek(guide = FALSE) + scale_fill_startrek(name = "Sex") + ylab("C:P Ratio") + xlab("Snowshoe Hare Dry Weight (g)") + theme_gdocs() + theme(text = element_text(size = 20), panel.grid.major = element_blank())

# ggsave("../Results/CPratioSex.pdf", device = "pdf", dpi = 300, height = 6, width = 8)

cp_model <- lm(CP_ratio ~ HareDryWeight, data = HareMorphStoich_norep, subset = HareMorphStoich_norep$Hare_Sex == "Male", weights = 1/C_molar_var)
summary(cp_model)

cp_model_f <- lm(CP_ratio ~ HareDryWeight, data = HareMorphStoich_norep, subset = HareMorphStoich_norep$Hare_Sex == "Female", weights = 1/C_molar_var)
summary(cp_model_f)

# Wet Weight
ggplot(HareMorphStoich_norep, aes(x = Hare_Weight, y = CP_ratio)) + geom_point(aes(fill = Hare_Sex), size = 5, shape = 21) + stat_smooth(data = subset(HareMorphStoich_norep, Hare_Sex != "Unknown"), aes(group = Hare_Sex, color = Hare_Sex, weight = 1/C_molar_var), method = "lm", alpha = 0.15) + scale_fill_startrek(name = "Sex") + scale_color_startrek(guide = FALSE) + ylab("C:P Ratio") + xlab("Snowshoe Hare Wet Weight (g)") + theme_gdocs() + theme(text = element_text(size = 20), panel.grid.major = element_blank())

cp_model_wet <- lm(CP_ratio ~ Hare_Weight, data = HareMorphStoich_norep, subset = HareMorphStoich_norep$Hare_Sex == "Male", weights = 1/C_molar_var)
summary(cp_model)

cp_model_f_wet <- lm(CP_ratio ~ Hare_Weight, data = HareMorphStoich_norep, subset = HareMorphStoich_norep$Hare_Sex == "Female", weights = 1/C_molar_var)
summary(cp_model_f_wet)

#### N:P Ratio Plots ####
# Dry Weight
ggplot(HareMorphStoich_norep, aes(x = HareDryWeight, y = NP_ratio)) + geom_point(aes(fill = Hare_Sex), size = 5, shape = 21) + stat_smooth(data = subset(HareMorphStoich_norep, Hare_Sex != "Unknown"),aes(group = Hare_Sex, color = Hare_Sex, weight = 1/N_molar_var), method = "lm", alpha = 0.15) + scale_color_startrek(guide = FALSE) +  scale_fill_startrek(name = "Sex")  + ylab("N:P Ratio") + xlab("Snowshoe Hare Dry Weight (g)") + theme_gdocs() + theme(text = element_text(size = 20), panel.grid.major = element_blank())

np_model <- lm(NP_ratio ~ HareDryWeight, data = HareMorphStoich_norep, subset = HareMorphStoich_norep$Hare_Sex == "Male", weights = 1/N_molar_var)
summary(np_model)

np_model_f <- lm(NP_ratio ~ HareDryWeight, data = HareMorphStoich_norep, subset = HareMorphStoich_norep$Hare_Sex == "Female", weights = 1/N_molar_var)
summary(np_model_f)

# Wet Weight
ggplot(HareMorphStoich_norep, aes(x = Hare_Weight, y = NP_ratio)) + geom_point(aes(fill = Hare_Sex), size = 5, shape = 21) + stat_smooth(data = subset(HareMorphStoich_norep, Hare_Sex != "Unknown"),aes(group = Hare_Sex, color = Hare_Sex, weight = 1/N_molar_var), method = "lm", alpha = 0.15) + scale_fill_startrek(name = "Sex") + scale_color_startrek(guide = FALSE) + ylab("N:P Ratio") + xlab("Snowshoe Hare Wet Weight (g)") + theme_gdocs() + theme(text = element_text(size = 20), panel.grid.major = element_blank())

np_model_wet <- lm(NP_ratio ~ Hare_Weight, data = HareMorphStoich_norep, subset = HareMorphStoich_norep$Hare_Sex == "Male", weights = 1/N_molar_var)
summary(np_model_wet)

np_model_f_wet <- lm(NP_ratio ~ Hare_Weight, data = HareMorphStoich_norep, subset = HareMorphStoich_norep$Hare_Sex == "Female", weights = 1/N_molar_var)
summary(np_model_f_wet)

# Differences between Age Groups

HareMorphStoich_norep$EstimatedAge <- as.numeric(HareMorphStoich_norep$EstimatedAge)

HareMorphStoich_norep <- mutate(HareMorphStoich_norep, AgeGroup2 = ifelse(EstimatedAge == "0", "Young", "Adult"))

HareMorphStoich_norep$AgeGroup2 <- as.factor(HareMorphStoich_norep$AgeGroup2)

ggplot(HareMorphStoich_norep, aes(x = Hare_Weight, y = CN_ratio)) + geom_point(aes(fill = AgeGroup2), size = 5, shape = 21) + stat_smooth(method = "lm", aes(group = AgeGroup2, col = AgeGroup2, weight = 1/C_molar_var)) + scale_fill_startrek(name = "Age Group") + scale_colour_startrek(name = "Age Group")

summary(lm(CN_ratio ~ Hare_Weight, data = HareMorphStoich_norep, subset = HareMorphStoich_norep$AgeGroup2 == "Young", weights = 1/C_molar_var))

summary(lm(CN_ratio ~ Hare_Weight, data = HareMorphStoich_norep, subset = HareMorphStoich_norep$AgeGroup2 == "Adult", weights = 1/C_molar_var))


#### Scraps ####
# TCH_037
# tch037 <- subset(HareMorphStoich, HareMorphStoich$SpecimenLabel =="TCH037_A" | HareMorphStoich$SpecimenLabel =="TCH037_B" | HareMorphStoich$SpecimenLabel =="TCH037_C")
# 
# tch037 <- droplevels(tch037)
# tch037$SpecimenLabel <- as.character(tch037$SpecimenLabel)
# 
# tch037<-rbind(tch037, as.data.frame(lapply(tch037, function(x) if (!is.numeric(x)) NA else mean(x))))
# 
# tch037[4,"Date"] = tch037[1,"Date"]
# tch037[4,"SpecimenLabel"] <- "TCH_037"
# tch037$SpecimenLabel <- as.factor(tch037$SpecimenLabel)
# tch037[4,"Observers"] <- tch037[1,"Observers"]
# tch037[4,"Age"] <- tch037[1,"Age"]
# tch037[4,"EstimatedAge"] <- tch037[1,"EstimatedAge"]
# tch037[4,"AgeRange"] <- tch037[1,"AgeRange"]
# tch037[4,"Hare_Sex"] <- tch037[1,"Hare_Sex"]
# tch037[4,"AgeGroup"] <- tch037[1,"AgeGroup"]
# 
# # TCH_040
# tch040 <- subset(HareMorphStoich, HareMorphStoich$SpecimenLabel =="TCH_040_A" | HareMorphStoich$SpecimenLabel =="TCH_040_B" | HareMorphStoich$SpecimenLabel =="TCH_040_C")
# 
# tch040 <- droplevels(tch040)
# tch040$SpecimenLabel <- as.character(tch040$SpecimenLabel)
# 
# tch040<-rbind(tch040, as.data.frame(lapply(tch040, function(x) if (!is.numeric(x)) NA else mean(x))))
# 
# tch040[4,"Date"] = tch040[1,"Date"]
# tch040[4,"SpecimenLabel"] <- "TCH_040"
# tch040$SpecimenLabel <- as.factor(tch040$SpecimenLabel)
# tch040[4,"Observers"] <- tch040[1,"Observers"]
# tch040[4,"Age"] <- tch040[1,"Age"]
# tch040[4,"EstimatedAge"] <- tch040[1,"EstimatedAge"]
# tch040[4,"AgeRange"] <- tch040[1,"AgeRange"]
# tch040[4,"Hare_Sex"] <- tch040[1,"Hare_Sex"]
# tch040[4,"AgeGroup"] <- tch040[1,"AgeGroup"]
# 
# # TCH_042
# tch042 <- subset(HareMorphStoich, HareMorphStoich$SpecimenLabel =="TCH_042_A" | HareMorphStoich$SpecimenLabel =="TCH_042_B" | HareMorphStoich$SpecimenLabel =="TCH_042_C")
# 
# tch042 <- droplevels(tch042)
# tch042$SpecimenLabel <- as.character(tch042$SpecimenLabel)
# 
# tch042<-rbind(tch042, as.data.frame(lapply(tch042, function(x) if (!is.numeric(x)) NA else mean(x))))
# 
# tch042[4,1] = tch042[1,1]
# tch042[4,2] <- "TCH_042"
# tch042$SpecimenLabel <- as.factor(tch042$SpecimenLabel)
# tch042[4,3] <- tch042[1,3]
# tch042[4,6] <- tch042[1,6]
# tch042[4,8] <- tch042[1,8]
# tch042[4,13] <- tch042[1,13]
# tch042[4,63] <- tch042[1,63]
# 
# # TCH_045
# tch045 <- subset(HareMorphStoich, HareMorphStoich$SpecimenLabel =="TCH_045_A" | HareMorphStoich$SpecimenLabel =="TCH_045_B" | HareMorphStoich$SpecimenLabel =="TCH_045_C")
# 
# tch045 <- droplevels(tch045)
# tch045$SpecimenLabel <- as.character(tch045$SpecimenLabel)
# 
# tch045<-rbind(tch045, as.data.frame(lapply(tch045, function(x) if (!is.numeric(x)) NA else mean(x))))
# 
# tch045[4,1] = tch045[1,1]
# tch045[4,2] <- "TCH_045"
# tch045$SpecimenLabel <- as.factor(tch045$SpecimenLabel)
# tch045[4,3] <- tch045[1,3]
# tch045[4,6] <- tch045[1,6]
# tch045[4,8] <- tch045[1,8]
# tch045[4,13] <- tch045[1,13]
# tch045[4,63] <- tch045[1,63]
# 
# # TCH_048
# tch048 <- subset(HareMorphStoich, HareMorphStoich$SpecimenLabel =="TCH_048_A" | HareMorphStoich$SpecimenLabel =="TCH_048_B" | HareMorphStoich$SpecimenLabel =="TCH_048_C")
# 
# tch048 <- droplevels(tch048)
# tch048$SpecimenLabel <- as.character(tch048$SpecimenLabel)
# 
# tch048<-rbind(tch048, as.data.frame(lapply(tch048, function(x) if (!is.numeric(x)) NA else mean(x))))
# 
# tch048[4,1] = tch048[1,1]
# tch048[4,2] <- "TCH_048"
# tch048$SpecimenLabel <- as.factor(tch048$SpecimenLabel)
# tch048[4,3] <- tch048[1,3]
# tch048[4,6] <- tch048[1,6]
# tch048[4,8] <- tch048[1,8]
# tch048[4,13] <- tch048[1,13]
# tch048[4,63] <- tch048[1,63]
# 
# temp.set_clean<-rbind(tch037[4,], tch040[4,], tch042[4,], tch045[4,], tch048[4,])
# 
# # Create a dataset to store molar ratios
# HareMolarRatios_norep <- HareMorphStoich_norep %>% dplyr::select(Date:Hare_Sex, AgeGroup, C_molar_mean, N_molar_mean, P_molar)

# # Append C & N Variances to No Replicates Molar Ratios dataset
# HareMolarRatios_norep <- add_column(HareMolarRatios_norep, C_molar_var = HareMorphStoich_norep$C_molar_var, .after = "C_molar_mean")
# HareMolarRatios_norep <- add_column(HareMolarRatios_norep, N_molar_var = HareMorphStoich_norep$N_molar_var, .after = "N_molar_mean")
# 
# # Calculate elemental ratios (C:N, C:P, N:P)
# HareMolarRatios_norep$CN_ratio <- HareMolarRatios_norep$C_molar_mean/HareMolarRatios_norep$N_molar_mean
# HareMolarRatios_norep$CP_ratio <- HareMolarRatios_norep$C_molar_mean/HareMolarRatios_norep$P_molar
# HareMolarRatios_norep$NP_ratio <- HareMolarRatios_norep$N_molar_mean/HareMolarRatios_norep$P_molar
# 
# # Create Long version of no-rep Molar Ratios dataset
# HareMolarRatios_noreplong <- gather(HareMolarRatios_norep, key = Nutrients, value = Molar_Ratio, CN_ratio:NP_ratio, factor_key = TRUE)

# get dry weight of hares
# get water content of each sample
# ggplot(HareMorphStoich, aes( x = SampleWetWeight, y = SampleDryWeight)) + stat_smooth(method = "lm", alpha = .15, color = "black") + geom_point(aes(fill = Hare_Sex), size = 7.5, shape = 21) + scale_fill_startrek(name = "Sex") + xlab("Sample Wet Weight (g)") + ylab("Sample Dry Weight (g)") + theme_classic() + theme(text = element_text(size = 16))

# ggsave("../Results/SampleH20.pdf", device = "pdf", dpi = 300)



# ggplot(HareMorphStoich, aes(x = PercWaterContent)) + geom_histogram(col = "#003399", fill = "#3366CC", bins = 12, binwidth = .018, alpha = .75) + xlab("Water Content (%)") + ylab("Count") + theme_gdocs() + theme(panel.grid.major = element_blank(), axis.text = element_text(size = 18), axis.title = element_text(size = 20))

# ggsave("../Results/PercentH20.pdf", device = "pdf", dpi = 300)

# scale up to whole hare content using %
# HareMorphStoich <- add_column(HareMorphStoich, HareDryWeight = (HareMorphStoich$Hare_Weight - (HareMorphStoich$Hare_Weight*HareMorphStoich$WaterContent)), .after = 4)
# ggsave("../Results/MolarRatiosSex.pdf", device = "pdf", dpi = 300, height = 6, width = 8)

# Append C & N Variances to Molar Ratios dataset
# HareMolarRatios <- add_column(HareMolarRatios, C_molar_var = HareMorphStoich$C_molar_var, .after = "C_molar_mean")
# HareMolarRatios <- add_column(HareMolarRatios, N_molar_var = HareMorphStoich$N_molar_var, .after = "N_molar_mean")

# Calculate elemental ratios (C:N, C:P, N:P)
# HareMolarRatios$CN_ratio <- HareMolarRatios$C_molar_mean/HareMolarRatios$N_molar_mean
# HareMolarRatios$CP_ratio <- HareMolarRatios$C_molar_mean/HareMolarRatios$P_molar
# HareMolarRatios$NP_ratio <- HareMolarRatios$N_molar_mean/HareMolarRatios$P_molar
# 
# # ggsave("../Results/NPratioSex.pdf", device = "pdf", dpi = 300, height = 6, width = 8)
