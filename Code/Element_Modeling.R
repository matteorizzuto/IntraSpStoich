# I will run models using each of the nutrients of interest (C, N, P) 
# and their stoichiometric ratios (C:N, C:P, N:P) as response. This will
# yield 6 sets of models, which I will then investigate using AICc to 
# find the most informative

# I will separate the analyses by tipe of response, element vs. ratio

#------------------------------------#
#         Modeling Elements          #
#------------------------------------#

# Remove Sex = no reaction individuals
HareMorphStoich_norep <- subset(HareMorphStoich_norep, HareMorphStoich_norep$Hare_Sex != "no reaction")

HareMorphStoich_norep <- droplevels(HareMorphStoich_norep)


###### Using Left Hind Foot length to calculate SMI (K_n) ######

#### CARBON ####

# Allocate empy model list to create a model select table later on
C_cand.list <- list()

# Create vector holding the models' names - there is 23 model
Mod.names <- c("Intercept", #1
               "Age + K_n + Sex", #2
               "Age + K_n + Sex + Sex:K_n", #3
               "Age + K_n + Sex + Age:K_n", #4
               "Age + K_n + Sex + Sex:K_n + Age:K_n", #5
               "Age", #6
               "K_n", #7
               "Sex", #8
               "Age + Sex", #9 
               "Sex + K_n", #10
               "Age + K_n", #11
               "Age + K_n + Age:K_n", #12
               "Sex + K_n + Sex:K_n", #13
               "Age + Sex + Age:Sex", #14
               "Age + AvgBodyLength + Sex", #15 
               "Age + AvgBodyLength + Sex + Sex:AvgBodyLength", #16 
               "Age + AvgBodyLength + Sex + Age:AvgBodyLength", #17
               "Age + AvgBodyLength + Sex + Sex:AvgBodyLength + Age:AvgBodyLength", #18
               "AvgBodyLength", #19
               "Sex + AvgBodyLength", #20
               "Age + AvgBodyLength", #21
               "Age + AvgBodyLength + Age:AvgBodyLength", #22
               "Sex + AvgBodyLength + Sex:AvgBodyLength" #23
               )


# Now I fit the model set on model at the time, produce a summary for each 
# model, and check model fit graphically for each one

# Intercept only model
C_cand.list[[1]] <- glm(C_mean ~ 1, data = HareMorphStoich_norep) 
summary(C_cand.list[[1]]) 

# Individual effects model
C_cand.list[[2]] <- glm(C_mean ~ EstimatedAge + K_n + Hare_Sex, data = HareMorphStoich_norep) 
summary(C_cand.list[[2]]) 
# get r squared for this model to later include in model selection table
rsq(C_cand.list[[2]], type = 'v')
# Note: for GLMs using Gaussian error families it is ok to calculate R^2, 
# because they are essentially OLS regressions. For other types of error 
# structures, R^2 is not informative and other methods to calculate pseudo-R^2 
# are available (https://goo.gl/EA6rzC)

# check model fit via plotting
par(mfrow = c(2,2))
plot(C_cand.list[[2]])  

# Model with an interaction term between sex and body condition
C_cand.list[[3]] <- glm(C_mean ~ EstimatedAge + K_n + Hare_Sex + Hare_Sex:K_n, data = HareMorphStoich_norep)
summary(C_cand.list[[3]])
plot(C_cand.list[[3]])

# Model with an age-BCI interaction
C_cand.list[[4]] <- glm(C_mean ~ EstimatedAge + K_n + Hare_Sex + EstimatedAge:K_n, data = HareMorphStoich_norep)
summary(C_cand.list[[4]])
plot(C_cand.list[[4]])

# Model including both interaction terms
C_cand.list[[5]] <- glm(C_mean ~ EstimatedAge + K_n + Hare_Sex + Hare_Sex:K_n + EstimatedAge:K_n, data = HareMorphStoich_norep)
summary(C_cand.list[[5]])
plot(C_cand.list[[5]])

# Model with only Age as an explanatory variable
C_cand.list[[6]] <- glm(C_mean ~ EstimatedAge, data = HareMorphStoich_norep) 
summary(C_cand.list[[6]])
plot(C_cand.list[[6]])

# Model with just K_n
C_cand.list[[7]] <- glm(C_mean ~ K_n, data = HareMorphStoich_norep) 
summary(C_cand.list[[7]])
plot(C_cand.list[[7]])

# Model with just Sex
C_cand.list[[8]] <- glm(C_mean ~ Hare_Sex, data = HareMorphStoich_norep) 
summary(C_cand.list[[8]])
plot(C_cand.list[[8]])

# Model with Age + Sex
C_cand.list[[9]] <- glm(C_mean ~ EstimatedAge + Hare_Sex, data = HareMorphStoich_norep) 
summary(C_cand.list[[9]])
plot(C_cand.list[[9]])

# Model with Sex + K_n
C_cand.list[[10]] <- glm(C_mean ~ Hare_Sex + K_n, data = HareMorphStoich_norep) 
summary(C_cand.list[[10]])
plot(C_cand.list[[10]])

# Model with Age + K_n
C_cand.list[[11]] <- glm(C_mean ~ EstimatedAge + K_n, data = HareMorphStoich_norep) 
summary(C_cand.list[[11]])
plot(C_cand.list[[11]])

# Model with Age + K_n + Age:K_n
C_cand.list[[12]] <- glm(C_mean ~ EstimatedAge + K_n + EstimatedAge:K_n, data = HareMorphStoich_norep)
summary(C_cand.list[[12]])
plot(C_cand.list[[12]])

# Model with Sex + K_n + Sex:K_n
C_cand.list[[13]] <- glm(C_mean ~ Hare_Sex + K_n + Hare_Sex:K_n, data = HareMorphStoich_norep)
summary(C_cand.list[[13]])
plot(C_cand.list[[13]])

# Model with Age + Sex + Age:Sex
C_cand.list[[14]] <- glm(C_mean ~ EstimatedAge+ Hare_Sex + EstimatedAge:Hare_Sex, data = HareMorphStoich_norep)
summary(C_cand.list[[14]])
plot(C_cand.list[[14]])

# Model with AvgBodyLength
C_cand.list[[15]] <- glm(C_mean ~ EstimatedAge + AvgBodyLength + Hare_Sex, data = HareMorphStoich_norep) 
summary(C_cand.list[[15]])
plot(C_cand.list[[15]])

# Model with Sex:AvgBodLength
C_cand.list[[16]] <- glm(C_mean ~ EstimatedAge + AvgBodyLength + Hare_Sex + Hare_Sex:AvgBodyLength, data = HareMorphStoich_norep)
summary(C_cand.list[[16]])
plot(C_cand.list[[16]])

# Model with  Age:AvgBodyLength interaction
C_cand.list[[17]] <- glm(C_mean ~ EstimatedAge + AvgBodyLength + Hare_Sex + EstimatedAge:AvgBodyLength, data = HareMorphStoich_norep)
summary(C_cand.list[[17]])
plot(C_cand.list[[17]])

# Model with both interaction terms featuring Average Body Length
C_cand.list[[18]] <- glm(C_mean ~ EstimatedAge + AvgBodyLength + Hare_Sex + Hare_Sex:AvgBodyLength + EstimatedAge:AvgBodyLength, data = HareMorphStoich_norep)
summary(C_cand.list[[18]])
plot(C_cand.list[[18]])

# Model with just AvgBodyLength
C_cand.list[[19]] <- glm(C_mean ~ AvgBodyLength, data = HareMorphStoich_norep) 
summary(C_cand.list[[19]])
plot(C_cand.list[[19]])

# Model with Sex + AvgBodyLength
C_cand.list[[20]] <- glm(C_mean ~ Hare_Sex + AvgBodyLength, data = HareMorphStoich_norep) 
summary(C_cand.list[[20]])
plot(C_cand.list[[20]])

# Model with Age + AvgBodyLength
C_cand.list[[21]] <- glm(C_mean ~ EstimatedAge + AvgBodyLength, data = HareMorphStoich_norep) 
summary(C_cand.list[[21]])
plot(C_cand.list[[21]])

# Model with Age + AvgBodyLength + Age:AvgBodyLength
C_cand.list[[22]] <- glm(C_mean ~ EstimatedAge + AvgBodyLength + EstimatedAge:AvgBodyLength, data = HareMorphStoich_norep)
summary(C_cand.list[[22]])
plot(C_cand.list[[22]])

# Model with Sex + AvgBodyLength + Sex:AvgBodyLength
C_cand.list[[23]] <- glm(C_mean ~ Hare_Sex + AvgBodyLength + Hare_Sex:AvgBodyLength, data = HareMorphStoich_norep)
summary(C_cand.list[[23]])
plot(C_cand.list[[23]])

# Now, I produce a summary AICc table
C_tab_out <- aictab(cand.set = C_cand.list, modnames = Mod.names, sort = TRUE,
                    digits = 3, LL = TRUE, second.ord = TRUE)

# need to add r squared values [rsq(C_cand.list[[i]], type = 'v')] to AICc table

# save AICc table to tex file for inclusion in manuscript Table 1
# print(xtable(C_tab_out, type = "latex"), file = "C_ModSel_AICc.tex", booktabs = TRUE)

# Now let's add the r squared values to the AICc table

# First, I will combine C_cand.list and Mod.names into a dataframe

storeR2 <- data.frame(Modnames = Mod.names, R2 = as.numeric(""))

# The following loop extracts the name of one model at the time from Mod.names, 
# matches it to the name in storeR2, then selects the corresponding model from 
# C_cand.list, calculates the R^2 and stores it in the corresponding line in 
# storeR2

for (i in 1:length(Mod.names)){
# 1. create a temporary ID by matching model name between Mod.names and storeR2
  tempID <-  which(storeR2$Modnames == Mod.names[i]) 
  
# 2. match the tempID to the correct model in C_cand.list and store it in a 
#    temporary dataframe  
  temp <- C_cand.list[[tempID]]
  
# 3. calculate the R^2 of the model store in temp and copy it to storeR2  
  storeR2$R2[i] <- rsq(temp, type = "v")
  # browser()
}

# combine C_tab_out with storeR2 to produce an AICc table with R2 values,
# and sort it by Delta_AICc value in ascending order

C_AICcTab <- merge(C_tab_out, storeR2, by = "Modnames") %>% dplyr::arrange(., Delta_AICc) %>% gt() %>% cols_hide(columns = vars(ModelLik, AICcWt, Cum.Wt)) %>% cols_label(Modnames = "Model", K = "k", Delta_AICc = "Delta AICc", R2 = "R^2") %>% fmt_number(columns = vars(AICc, Delta_AICc, LL, R2), decimals = 3) 

gtsave(C_AICcTab, filename = "../Results/C_AICcTab.tex")



#### NITROGEN ####

# Allocate empty candidate models list to create model selection table later
N_cand.list <- list()

# model names are the same as C_cand.list
 
# Intercept only model
N_cand.list[[1]] <- glm(N_mean ~ 1, data = HareMorphStoich_norep) 
summary(N_cand.list[[1]])
 
# Factors-only model
N_cand.list[[2]] <- glm(N_mean ~ EstimatedAge + K_n + Hare_Sex, data = HareMorphStoich_norep)
summary(N_cand.list[[2]])
plot(N_cand.list[[2]])

# Sex-BCI interaction model
N_cand.list[[3]] <- glm(N_mean ~ EstimatedAge + K_n + Hare_Sex + Hare_Sex:K_n, data = HareMorphStoich_norep)
summary(N_cand.list[[3]])
plot(N_cand.list[[3]])

# Age-BCI interaction model
N_cand.list[[4]] <- glm(N_mean ~ EstimatedAge + K_n + Hare_Sex + EstimatedAge:K_n, data = HareMorphStoich_norep)
summary(N_cand.list[[4]])
plot(N_cand.list[[4]])

# Note: the interaction Age:K_n is likely a pretending variable due to K_n
# being one

# Sex-BCI and Age-BCI model
# Age:K_n is likely a pretending variable
N_cand.list[[5]] <- glm(N_mean ~ EstimatedAge + K_n + Hare_Sex + Hare_Sex:K_n + EstimatedAge:K_n, data = HareMorphStoich_norep)
summary(N_cand.list[[5]])
plot(N_cand.list[[5]])

# Fifth a model that considers Age only
N_cand.list[[6]] <- glm(N_mean ~ EstimatedAge, data = HareMorphStoich_norep)
summary(N_cand.list[[6]])
plot(N_cand.list[[6]])

# Sixth a model that considers K_n only
N_cand.list[[7]] <- glm(N_mean ~ K_n, data = HareMorphStoich_norep)
summary(N_cand.list[[7]])
plot(N_cand.list[[7]])

# Seventh a model that considers Sex only
N_cand.list[[8]] <- glm(N_mean ~ Hare_Sex, data = HareMorphStoich_norep)
summary(N_cand.list[[8]])
plot(N_cand.list[[8]])

# Eight model with Age + Sex
N_cand.list[[9]] <- glm(N_mean ~ EstimatedAge + Hare_Sex, data = HareMorphStoich_norep) 
summary(N_cand.list[[9]])
plot(N_cand.list[[9]])

# Ninth model with Sex + K_n
N_cand.list[[10]] <- glm(N_mean ~ Hare_Sex + K_n, data = HareMorphStoich_norep) 
summary(N_cand.list[[10]])
plot(N_cand.list[[10]])

# Tenth model with Age + K_n
N_cand.list[[11]] <- glm(N_mean ~ EstimatedAge + K_n, data = HareMorphStoich_norep) 
summary(N_cand.list[[11]])
plot(N_cand.list[[11]])

# 11th model with Age + K_n + Age:K_n
N_cand.list[[12]] <- glm(N_mean ~ EstimatedAge + K_n + EstimatedAge:K_n, data = HareMorphStoich_norep)
summary(N_cand.list[[12]])
plot(N_cand.list[[12]])

# 12th model with Sex + K_n + Sex:K_n
N_cand.list[[13]] <- glm(N_mean ~ Hare_Sex + K_n + Hare_Sex:K_n, data = HareMorphStoich_norep)
summary(N_cand.list[[13]])
plot(N_cand.list[[13]])

# 13th model with Age + Sex + Age:Sex
N_cand.list[[14]] <- glm(N_mean ~ EstimatedAge + Hare_Sex + EstimatedAge:Hare_Sex, data = HareMorphStoich_norep)
summary(N_cand.list[[14]])
plot(N_cand.list[[14]])

# Factors-only model with AvgBodyLength
N_cand.list[[15]] <- glm(N_mean ~ EstimatedAge + AvgBodyLength + Hare_Sex, data = HareMorphStoich_norep)
summary(N_cand.list[[15]])
plot(N_cand.list[[15]])

# Sex-AvgBodyLength interaction model
N_cand.list[[16]] <- glm(N_mean ~ EstimatedAge + AvgBodyLength + Hare_Sex + Hare_Sex:AvgBodyLength, data = HareMorphStoich_norep)
summary(N_cand.list[[16]])
plot(N_cand.list[[16]])

# Age:AvgBodyLength interaction model
N_cand.list[[17]] <- glm(N_mean ~ EstimatedAge + AvgBodyLength + Hare_Sex + EstimatedAge:AvgBodyLength, data = HareMorphStoich_norep)
summary(N_cand.list[[17]])
plot(N_cand.list[[17]])

# Sex-BCI and Age:AvgBodyLength model
N_cand.list[[18]] <- glm(N_mean ~ EstimatedAge + AvgBodyLength + Hare_Sex + Hare_Sex:AvgBodyLength + EstimatedAge:AvgBodyLength, data = HareMorphStoich_norep)
summary(N_cand.list[[18]])
plot(N_cand.list[[18]])

# Sixth a model that considers AvgBodyLength only
N_cand.list[[19]] <- glm(N_mean ~ AvgBodyLength, data = HareMorphStoich_norep)
summary(N_cand.list[[19]])
plot(N_cand.list[[19]])

# 19th model with Sex + AvgBodyLength
N_cand.list[[20]] <- glm(N_mean ~ Hare_Sex + AvgBodyLength, data = HareMorphStoich_norep)
summary(N_cand.list[[20]])
plot(N_cand.list[[20]])

# 20th model with Age + AvgBodyLength
N_cand.list[[21]] <- glm(N_mean ~ EstimatedAge + AvgBodyLength, data = HareMorphStoich_norep) 
summary(N_cand.list[[21]])
plot(N_cand.list[[21]])

# 21th model with Age + AvgBodyLength + Age:AvgBodyLength
N_cand.list[[22]] <- glm(N_mean ~ EstimatedAge + AvgBodyLength + EstimatedAge:AvgBodyLength, data = HareMorphStoich_norep)
summary(N_cand.list[[22]])
plot(N_cand.list[[22]])

# 22th model with Sex + AvgBodyLength + Sex:AvgBodyLength
N_cand.list[[23]] <- glm(N_mean ~ Hare_Sex + AvgBodyLength + Hare_Sex:AvgBodyLength, data = HareMorphStoich_norep)
summary(N_cand.list[[23]])
plot(N_cand.list[[23]])

# Produce the AICc table

N_tab_out <- aictab(cand.set = N_cand.list, modnames = Mod.names, sort = TRUE, 
                    digits = 3, second.ord = TRUE, LL = TRUE)

# Now let's add the r squared values to the AICc table

# First, I will combine N_cand.list and Mod.names into a dataframe
rm(list = c("storeR2", "tempID", "temp")) # remove previously created elements

storeR2 <- data.frame(Modnames = Mod.names, R2 = as.numeric(""))

# The following loop extracts the name of one model at the time from Mod.names, 
# matches it to the name in storeR2, then selects the corresponding model from 
# N_cand.list, calculates the R^2 and stores it in the corresponding line in 
# storeR2

for (i in 1:length(Mod.names)){
  # 1. create a temporary ID by matching model name between Mod.names and storeR2
  tempID <-  which(storeR2$Modnames == Mod.names[i]) 
  
  # 2. match the tempID to the correct model in P_cand.list and store it in a 
  #    temporary dataframe  
  temp <- N_cand.list[[tempID]]
  
  # 3. calculate the R^2 of the model store in temp and copy it to storeR2  
  storeR2$R2[i] <- rsq(temp, type = "v")
  # browser()
}

# combine N_tab_out with storeR2 to produce an AICc table with R2 values

N_AICcTab <- merge(N_tab_out, storeR2, by = "Modnames") %>% dplyr::arrange(., Delta_AICc) %>% gt() %>% cols_hide(columns = vars(ModelLik, AICcWt, Cum.Wt)) %>% cols_label(Modnames = "Model", K = "k", Delta_AICc = "Delta AICc", R2 = "R^2") %>% fmt_number(columns = vars(AICc, Delta_AICc, LL, R2), decimals = 3)

gtsave(N_AICcTab, filename = "../Results/N_AICcTab.tex")


##### PHOSPHORUS ####

# ALlocate empty model list to produce a model selection table later on
P_cand.list <- list()

# model names are the same as C_cand.list

# Now let's populate P_cand.list!

# Intercept model
P_cand.list[[1]] <- glm(P ~ 1,  data = HareMorphStoich_norep)
summary(P_cand.list[[1]])

# Factors-only model
P_cand.list[[2]] <- glm(P ~ EstimatedAge + K_n + Hare_Sex, data = HareMorphStoich_norep)
summary(P_cand.list[[2]])
plot(P_cand.list[[2]])

# Sex-BCI interaction model
P_cand.list[[3]] <- glm(P ~ EstimatedAge + K_n + Hare_Sex + Hare_Sex:K_n, data = HareMorphStoich_norep)
summary(P_cand.list[[3]])
plot(P_cand.list[[3]])

# Age-BCI interaction model
P_cand.list[[4]] <- glm(P ~ EstimatedAge + K_n + Hare_Sex + EstimatedAge:K_n, data = HareMorphStoich_norep)
summary(P_cand.list[[4]])
plot(P_cand.list[[4]])

# Sex-BCI and Age-BCI interaction model
P_cand.list[[5]] <- glm(P ~ EstimatedAge + K_n + Hare_Sex + Hare_Sex:K_n + EstimatedAge:K_n, data = HareMorphStoich_norep)
summary(P_cand.list[[5]])
plot(P_cand.list[[5]])

# Fifth model with just Age
P_cand.list[[6]] <- glm(P ~ EstimatedAge, data = HareMorphStoich_norep)
summary(P_cand.list[[6]])
plot(P_cand.list[[6]])

# Sixth model with just K_n
P_cand.list[[7]] <- glm(P ~ K_n, data = HareMorphStoich_norep)
summary(P_cand.list[[7]])
plot(P_cand.list[[7]])

# Seventh model with just Sex
P_cand.list[[8]] <- glm(P ~ Hare_Sex, data = HareMorphStoich_norep)
summary(P_cand.list[[8]])
plot(P_cand.list[[8]])

# Eight model with Age + Sex
P_cand.list[[9]] <- glm(P ~ EstimatedAge + Hare_Sex, data = HareMorphStoich_norep) 
summary(P_cand.list[[9]])
plot(P_cand.list[[9]])

# Ninth model with Sex + K_n
P_cand.list[[10]] <- glm(P ~ Hare_Sex + K_n, data = HareMorphStoich_norep) 
summary(P_cand.list[[10]])
plot(P_cand.list[[10]])

# Tenth model with Age + K_n
P_cand.list[[11]] <- glm(P ~ EstimatedAge + K_n, data = HareMorphStoich_norep) 
summary(P_cand.list[[11]])
plot(P_cand.list[[11]])

# 11th model with Age + K_n + Age:K_n
P_cand.list[[12]] <- glm(P ~ EstimatedAge + K_n + EstimatedAge:K_n, data = HareMorphStoich_norep)
summary(P_cand.list[[12]])
plot(P_cand.list[[12]])

# 12th model with Sex + K_n + Sex:K_n
P_cand.list[[13]] <- glm(P ~ Hare_Sex + K_n + Hare_Sex:K_n, data = HareMorphStoich_norep)
summary(P_cand.list[[13]])
plot(P_cand.list[[13]])

# 13th model with Age + Sex + Age:Sex
P_cand.list[[14]] <- glm(P ~ EstimatedAge + Hare_Sex + EstimatedAge:Hare_Sex, data = HareMorphStoich_norep)
summary(P_cand.list[[14]])
plot(P_cand.list[[14]])

# Factors-only model with AvgBodyLength
P_cand.list[[15]] <- glm(P ~ EstimatedAge + AvgBodyLength + Hare_Sex, data = HareMorphStoich_norep)
summary(P_cand.list[[15]])
plot(P_cand.list[[15]])

# Sex-AvgBodyLength interaction model
P_cand.list[[16]] <- glm(P ~ EstimatedAge + AvgBodyLength + Hare_Sex + Hare_Sex:AvgBodyLength, data = HareMorphStoich_norep)
summary(P_cand.list[[16]])
plot(P_cand.list[[16]])

# Age-AvgBodyLength interaction model
P_cand.list[[17]] <- glm(P ~ EstimatedAge + AvgBodyLength + Hare_Sex + EstimatedAge:AvgBodyLength, data = HareMorphStoich_norep)
summary(P_cand.list[[17]])
plot(P_cand.list[[17]])

# Sex-AvgBodyLength and Age-AvgBodyLength interaction model
P_cand.list[[18]] <- glm(P ~ EstimatedAge + AvgBodyLength + Hare_Sex + Hare_Sex:AvgBodyLength + EstimatedAge:AvgBodyLength, data = HareMorphStoich_norep)
summary(P_cand.list[[18]])
plot(P_cand.list[[18]])

# 18th model with just AvgBodyLength
P_cand.list[[19]] <- glm(P ~ AvgBodyLength, data = HareMorphStoich_norep)
summary(P_cand.list[[19]])
plot(P_cand.list[[19]])

# 19th model with Sex + AvgBodyLength
P_cand.list[[20]] <- glm(P ~ Hare_Sex + AvgBodyLength, data = HareMorphStoich_norep) 
summary(P_cand.list[[20]])
plot(P_cand.list[[20]])

# 20th model with Age + AvgBodyLength
P_cand.list[[21]] <- glm(P ~ EstimatedAge + AvgBodyLength, data = HareMorphStoich_norep) 
summary(P_cand.list[[21]])
plot(P_cand.list[[21]])

# 21st model with Age + AvgBodyLength + Age:AvgBodyLength
P_cand.list[[22]] <- glm(P ~ EstimatedAge + AvgBodyLength + EstimatedAge:AvgBodyLength, data = HareMorphStoich_norep)
summary(P_cand.list[[22]])
plot(P_cand.list[[22]])

# 22th model with Sex + AvgBodyLength + Sex:AvgBodyLength
P_cand.list[[23]] <- glm(P ~ Hare_Sex + AvgBodyLength + Hare_Sex:AvgBodyLength, data = HareMorphStoich_norep)
summary(P_cand.list[[23]])
plot(P_cand.list[[23]])

# Produce the AICc model selection table

P_tab_out <- aictab(cand.set = P_cand.list, modnames = Mod.names, sort = TRUE, 
                    digits = 3, second.ord = TRUE, LL = TRUE)

# Now let's add the r squared values to the AICc table

# First, I will combine P_cand.list and Mod.names into a dataframe

rm(list = c("storeR2", "tempID", "temp")) # removes previously created elements

storeR2 <- data.frame(Modnames = Mod.names, R2 = as.numeric(""))

# The following loop extracts the name of one model at the time from Mod.names, 
# matches it to the name in storeR2, then selects the corresponding model from 
# P_cand.list, calculates the R^2 and stores it in the corresponding line in 
# storeR2

for (i in 1:length(Mod.names)){
  # 1. create a temporary ID by matching model name between Mod.names and storeR2
  tempID <-  which(storeR2$Modnames == Mod.names[i]) 
  
  # 2. match the tempID to the correct model in P_cand.list and store it in a 
  #    temporary dataframe  
  temp <- P_cand.list[[tempID]]
  
  # 3. calculate the R^2 of the model store in temp and copy it to storeR2  
  storeR2$R2[i] <- rsq(temp, type = "v")
  # browser()
}

# combine P_tab_out with storeR2 to produce an AICc table with R2 values

P_AICcTab <- merge(P_tab_out, storeR2, by = "Modnames") %>% dplyr::arrange(., Delta_AICc) %>% gt() %>% cols_hide(columns = vars(ModelLik, AICcWt, Cum.Wt)) %>% cols_label(Modnames = "Model", K = "k", Delta_AICc = "Delta AICc", R2 = "R^2")%>% fmt_number(columns = vars(AICc, Delta_AICc, LL, R2), decimals = 3)

gtsave(P_AICcTab, filename = "../Results/P_AICcTab.tex") 


# For good measure, check for collinearity among explanatory variables

HareMorphStoich_ExplanOnly <- HareMorphStoich_norep[, c(7,11,17,72)] # run to check collinearity of all 4 variables

HareMorphStoich_ExplanOnly <- HareMorphStoich_ExplanOnly %>% mutate(Hare_Sex = ifelse(Hare_Sex == "Female", "2", "1"))

HareMorphStoich_ExplanOnly$Hare_Sex <- as.numeric(HareMorphStoich_ExplanOnly$Hare_Sex)

vifstep(HareMorphStoich_ExplanOnly, th = 3)

vif(HareMorphStoich_ExplanOnly) # gets exact vif values for table S3 in SI

###### Using Skull Length to calculate SMI (SkK_n) ######
# Do results change if I run the same models but with body condition calculated
# usig skull lenght rather than left hind foot length?

#### CARBON ####
# The only models that I need to rerun are the ones that originally included K_n
# The Mod.names vector, however, needs to change to account for the change in 
# body condition calculations 

Mod.names_Sk <- c("Intercept", 
               "Age + SkK_n + Sex", 
               "Age + SkK_n + Sex + Sex:SkK_n", 
               "Age + SkK_n + Sex + Age:SkK_n", 
               "Age + SkK_n + Sex + Sex:SkK_n + Age:SkK_n", 
               "Age", 
               "SkK_n", 
               "Sex", 
               "Age + Sex", 
               "Sex + SkK_n", 
               "Age + SkK_n", 
               "Age + SkK_n + Age:SkK_n", 
               "Sex + SkK_n + Sex:SkK_n", 
               "Age + Sex + Age:Sex", 
               "Age + AvgBodyLength + Sex", 
               "Age + AvgBodyLength + Sex + Sex:AvgBodyLength", 
               "Age + AvgBodyLength + Sex + Age:AvgBodyLength", 
               "Age + AvgBodyLength + Sex + Sex:AvgBodyLength + Age:AvgBodyLength", 
               "AvgBodyLength", 
               "Sex + AvgBodyLength", 
               "Age + AvgBodyLength", 
               "Age + AvgBodyLength + Age:AvgBodyLength", 
               "Sex + AvgBodyLength + Sex:AvgBodyLength"
               )

# Model with only the individual effects
C_cand.list[[2]] <- glm(C_mean ~ EstimatedAge + SkK_n + Hare_Sex, data = HareMorphStoich_norep) 
summary(C_cand.list[[2]]) 
plot(C_cand.list[[2]])

# The second model takes into account an interaction term between sex
# and body condition
C_cand.list[[3]] <- glm(C_mean ~ EstimatedAge + SkK_n + Hare_Sex + Hare_Sex:SkK_n, data = HareMorphStoich_norep)
summary(C_cand.list[[3]])
plot(C_cand.list[[3]])

# The third model includes an age-BCI interaction
C_cand.list[[4]] <- glm(C_mean ~ EstimatedAge + SkK_n + Hare_Sex + EstimatedAge:SkK_n, data = HareMorphStoich_norep)
summary(C_cand.list[[4]])
plot(C_cand.list[[4]])

# A fourth model includes both interaction terms
C_cand.list[[5]] <- glm(C_mean ~ EstimatedAge + SkK_n + Hare_Sex + Hare_Sex:SkK_n + EstimatedAge:SkK_n, data = HareMorphStoich_norep)
summary(C_cand.list[[5]])
plot(C_cand.list[[5]])

# Sixth model with just SkK_n
C_cand.list[[7]] <- glm(C_mean ~ SkK_n, data = HareMorphStoich_norep) 
summary(C_cand.list[[7]])
plot(C_cand.list[[7]])

# Ninth model with Sex + SkK_n
C_cand.list[[10]] <- glm(C_mean ~ Hare_Sex + SkK_n, data = HareMorphStoich_norep) 
summary(C_cand.list[[10]])
plot(C_cand.list[[10]])

# Tenth model with Age + SkK_n
C_cand.list[[11]] <- glm(C_mean ~ EstimatedAge + SkK_n, data = HareMorphStoich_norep) 
summary(C_cand.list[[11]])
plot(C_cand.list[[11]])

# 11th model with Age + SkK_n + Age:SkK_n
C_cand.list[[12]] <- glm(C_mean ~ EstimatedAge + SkK_n + EstimatedAge:SkK_n, data = HareMorphStoich_norep)
summary(C_cand.list[[12]])
plot(C_cand.list[[12]])

# 12th model with Sex + SkK_n + Sex:SkK_n
C_cand.list[[13]] <- glm(C_mean ~ Hare_Sex + SkK_n + Hare_Sex:SkK_n, data = HareMorphStoich_norep)
summary(C_cand.list[[13]])
plot(C_cand.list[[13]])

SkC_tab_out <- aictab(cand.set = C_cand.list, modnames = Mod.names_Sk, sort = TRUE, 
                    digits = 3, second.ord = TRUE, LL = TRUE)

# Now let's add the r squared values to the AICc table

# First, I will combine P_cand.list and Mod.names into a dataframe

rm(list = c("storeR2", "tempID", "temp")) # removes previously created elements

storeR2 <- data.frame(Modnames = Mod.names_Sk, R2 = as.numeric(""))

# The following loop extracts the name of one model at the time from Mod.names, 
# matches it to the name in storeR2, then selects the corresponding model from 
# C_cand.list, calculates the R^2 and stores it in the corresponding line in 
# storeR2

for (i in 1:length(Mod.names_Sk)){
  # 1. create a temporary ID by matching model name between Mod.names and storeR2
  tempID <-  which(storeR2$Modnames == Mod.names_Sk[i]) 
  
  # 2. match the tempID to the correct model in C_cand.list and store it in a 
  #    temporary dataframe  
  temp <- C_cand.list[[tempID]]
  
  # 3. calculate the R^2 of the model store in temp and copy it to storeR2  
  storeR2$R2[i] <- rsq(temp, type = "v")
  # browser()
}

# combine C_tab_out with storeR2 to produce an AICc table with R2 values

SkC_AICcTab <- merge(SkC_tab_out, storeR2, by = "Modnames") %>% dplyr::arrange(., Delta_AICc) %>% gt() %>% cols_hide(columns = vars(ModelLik, AICcWt, Cum.Wt)) %>% cols_label(Modnames = "Model", K = "k", Delta_AICc = "Delta AICc", R2 = "R^2")%>% fmt_number(columns = vars(AICc, Delta_AICc, LL, R2), decimals = 3)

gtsave(SkC_AICcTab, filename = "../Results/SkC_AICcTab.tex") 


#### NITROGEN ####
# same goes for Nitrogen

# Factors-only model
N_cand.list[[2]] <- glm(N_mean ~ EstimatedAge + SkK_n + Hare_Sex, data = HareMorphStoich_norep)
summary(N_cand.list[[2]])
plot(N_cand.list[[2]])

# Sex-BCI interaction model
N_cand.list[[3]] <- glm(N_mean ~ EstimatedAge + SkK_n + Hare_Sex + Hare_Sex:SkK_n, data = HareMorphStoich_norep)
summary(N_cand.list[[3]])
plot(N_cand.list[[3]])

# Age-BCI interaction model
# Age:K_n is likely a pretending variable
N_cand.list[[4]] <- glm(N_mean ~ EstimatedAge + SkK_n + Hare_Sex + EstimatedAge:SkK_n, data = HareMorphStoich_norep)
summary(N_cand.list[[4]])
plot(N_cand.list[[4]])

# Sex-BCI and Age-BCI model
# Age:K_n is likely a pretending variable
N_cand.list[[5]] <- glm(N_mean ~ EstimatedAge + SkK_n + Hare_Sex + Hare_Sex:SkK_n + EstimatedAge:SkK_n, data = HareMorphStoich_norep)
summary(N_cand.list[[5]])
plot(N_cand.list[[5]])

# Sixth a model that considers SkK_n only
N_cand.list[[7]] <- glm(N_mean ~ SkK_n, data = HareMorphStoich_norep)
summary(N_cand.list[[7]])
plot(N_cand.list[[7]])

# Ninth model with Sex + SkK_n
N_cand.list[[10]] <- glm(N_mean ~ Hare_Sex + SkK_n, data = HareMorphStoich_norep) 
summary(N_cand.list[[10]])
plot(N_cand.list[[10]])

# Tenth model with Age + SkK_n
N_cand.list[[11]] <- glm(N_mean ~ EstimatedAge + SkK_n, data = HareMorphStoich_norep)
summary(N_cand.list[[11]])
plot(N_cand.list[[11]])

# 11th model with Age + SkK_n + Age:SkK_n
N_cand.list[[12]] <- glm(N_mean ~ EstimatedAge + SkK_n + EstimatedAge:SkK_n, data = HareMorphStoich_norep)
summary(N_cand.list[[12]])
plot(N_cand.list[[12]])

# 12th model with Sex + SkK_n + Sex:SkK_n
N_cand.list[[13]] <- glm(N_mean ~ Hare_Sex + SkK_n + Hare_Sex:SkK_n, data = HareMorphStoich_norep)
summary(N_cand.list[[13]])
plot(N_cand.list[[13]])


# produce the AICc model selectio table
SkN_tab_out <- aictab(cand.set = N_cand.list, modnames = Mod.names_Sk, sort = TRUE, 
                    digits = 3, second.ord = TRUE, LL = TRUE)

# Now let's add the r squared values to the AICc table

# First, I will combine N_cand.list and Mod.names into a dataframe

rm(list = c("storeR2", "tempID", "temp")) # removes previously created elements

storeR2 <- data.frame(Modnames = Mod.names_Sk, R2 = as.numeric(""))

# The following loop extracts the name of one model at the time from Mod.names, 
# matches it to the name in storeR2, then selects the corresponding model from 
# N_cand.list, calculates the R^2 and stores it in the corresponding line in 
# storeR2

for (i in 1:length(Mod.names_Sk)){
  # 1. create a temporary ID by matching model name between Mod.names and storeR2
  tempID <-  which(storeR2$Modnames == Mod.names_Sk[i]) 
  
  # 2. match the tempID to the correct model in N_cand.list and store it in a 
  #    temporary dataframe  
  temp <- N_cand.list[[tempID]]
  
  # 3. calculate the R^2 of the model store in temp and copy it to storeR2  
  storeR2$R2[i] <- rsq(temp, type = "v")
  # browser()
}

# combine N_tab_out with storeR2 to produce an AICc table with R2 values

SkN_AICcTab <- merge(SkN_tab_out, storeR2, by = "Modnames") %>% dplyr::arrange(., Delta_AICc) %>% gt() %>% cols_hide(columns = vars(ModelLik, AICcWt, Cum.Wt)) %>% cols_label(Modnames = "Model", K = "k", Delta_AICc = "Delta AICc", R2 = "R^2")%>% fmt_number(columns = vars(AICc, Delta_AICc, LL, R2), decimals = 3) 

gtsave(SkN_AICcTab, filename = "../Results/SkN_AICcTab.tex") 


#### PHOSPHORUS ####
# finally, same goes for Phosphorus

# Factors-only model
P_cand.list[[2]] <- glm(P ~ EstimatedAge + SkK_n + Hare_Sex, data = HareMorphStoich_norep)
summary(P_cand.list[[2]])
plot(P_cand.list[[2]])

# Sex-BCI interaction model
P_cand.list[[3]] <- glm(P ~ EstimatedAge + SkK_n + Hare_Sex + Hare_Sex:SkK_n, data = HareMorphStoich_norep)
summary(P_cand.list[[3]])
plot(P_cand.list[[3]])

# Age-BCI interaction model
P_cand.list[[4]] <- glm(P ~ EstimatedAge + SkK_n + Hare_Sex + EstimatedAge:SkK_n, data = HareMorphStoich_norep)
summary(P_cand.list[[4]])
plot(P_cand.list[[4]])

# Sex-BCI and Age-BCI interaction model
P_cand.list[[5]] <- glm(P ~ EstimatedAge + SkK_n + Hare_Sex + Hare_Sex:SkK_n + EstimatedAge:SkK_n, data = HareMorphStoich_norep)
summary(P_cand.list[[5]])
plot(P_cand.list[[5]])

# Sixth model with just SkK_n
P_cand.list[[7]] <- glm(P ~ SkK_n, data = HareMorphStoich_norep)
summary(P_cand.list[[7]])
plot(P_cand.list[[7]])

# Ninth model with Sex + SkK_n
P_cand.list[[10]] <- glm(P ~ Hare_Sex + SkK_n, data = HareMorphStoich_norep) 
summary(P_cand.list[[10]])
plot(P_cand.list[[10]])

# Tenth model with Age + SkK_n
P_cand.list[[11]] <- glm(P ~ EstimatedAge + SkK_n, data = HareMorphStoich_norep) 
summary(P_cand.list[[11]])
plot(P_cand.list[[11]])

# 11th model with Age + SkK_n + Age:SkK_n
P_cand.list[[12]] <- glm(P ~ EstimatedAge + SkK_n + EstimatedAge:SkK_n, data = HareMorphStoich_norep)
summary(P_cand.list[[12]])
plot(P_cand.list[[12]])

# 12th model with Sex + SkK_n + Sex:SkK_n
P_cand.list[[13]] <- glm(P ~ Hare_Sex + SkK_n + Hare_Sex:SkK_n, data = HareMorphStoich_norep)
summary(P_cand.list[[13]])
plot(P_cand.list[[13]])

# create the AICc model selection table
SkP_tab_out <- aictab(cand.set = P_cand.list, modnames = Mod.names_Sk, sort = TRUE, 
                    digits = 3, second.ord = TRUE, LL = TRUE)

# produce the AICc model selectio table
SkN_tab_out <- aictab(cand.set = N_cand.list, modnames = Mod.names_Sk, sort = TRUE, 
                      digits = 3, second.ord = TRUE, LL = TRUE)

# Now let's add the r squared values to the AICc table

# First, I will combine P_cand.list and Mod.names into a dataframe

rm(list = c("storeR2", "tempID", "temp")) # removes previously created elements

storeR2 <- data.frame(Modnames = Mod.names_Sk, R2 = as.numeric(""))

# The following loop extracts the name of one model at the time from Mod.names, 
# matches it to the name in storeR2, then selects the corresponding model from 
# P_cand.list, calculates the R^2 and stores it in the corresponding line in 
# storeR2

for (i in 1:length(Mod.names_Sk)){
  # 1. create a temporary ID by matching model name between Mod.names and storeR2
  tempID <-  which(storeR2$Modnames == Mod.names_Sk[i]) 
  
  # 2. match the tempID to the correct model in P_cand.list and store it in a 
  #    temporary dataframe  
  temp <- P_cand.list[[tempID]]
  
  # 3. calculate the R^2 of the model store in temp and copy it to storeR2  
  storeR2$R2[i] <- rsq(temp, type = "v")
  # browser()
}

# combine P_tab_out with storeR2 to produce an AICc table with R2 values

SkP_AICcTab <- merge(SkP_tab_out, storeR2, by = "Modnames") %>% dplyr::arrange(., Delta_AICc) %>% gt() %>% cols_hide(columns = vars(ModelLik, AICcWt, Cum.Wt)) %>% cols_label(Modnames = "Model", K = "k", Delta_AICc = "Delta AICc", R2 = "R^2")%>% fmt_number(columns = vars(AICc, Delta_AICc, LL, R2), decimals = 3)

gtsave(SkP_AICcTab, filename = "../Results/SkP_AICcTab.tex") 


# For good measure, check for collinearity between explanatory variables

HareMorphStoich_ExplanOnly <- HareMorphStoich_norep[, c(9,11,17,72)]

HareMorphStoich_ExplanOnly <- HareMorphStoich_ExplanOnly %>% mutate(Hare_Sex = ifelse(Hare_Sex == "Female", "2", "1"))

HareMorphStoich_ExplanOnly$Hare_Sex <- as.numeric(HareMorphStoich_ExplanOnly$Hare_Sex)

vifstep(HareMorphStoich_ExplanOnly, th = 3)
