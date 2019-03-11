# I will run the same set of models used for the Elements, but this time 
# the response variables will be the stoichiometric ratios C:N, C:P, N:P

#------------------------------------#
#         Modeling Ratios            #
#------------------------------------#

###### Using Left Hind Foot length to calculate SMI (SkK_n) ######

# First, allocate an empty candidate model list
CN_cand.list <- list()

# Second, I will use the same vector of model names as for the K_n-based 
# element modeling, i.e. Mod.names

#### C:N ratio ####

# Third, run the models and populate the candidate models list 

# The models are exactly the same as those used in Element_Modeling.R
# Hence, I will not mention what variables each model considers

CN_cand.list[[1]] <- glm(CN_ratio ~ 1, data = HareMorphStoich_norep)
summary(CN_cand.list[[1]])
plot(CN_cand.list[[1]])

CN_cand.list[[2]] <- glm(CN_ratio ~ EstimatedAge + K_n + Hare_Sex, data = HareMorphStoich_norep)
summary(CN_cand.list[[2]])
plot(CN_cand.list[[2]])

CN_cand.list[[3]] <- glm(CN_ratio ~ EstimatedAge + K_n + Hare_Sex + Hare_Sex:K_n, data = HareMorphStoich_norep)
summary(CN_cand.list[[3]])
plot(CN_cand.list[[3]])

CN_cand.list[[4]] <- glm(CN_ratio ~ EstimatedAge + K_n + Hare_Sex + EstimatedAge:K_n, data = HareMorphStoich_norep)
summary(CN_cand.list[[4]])
plot(CN_cand.list[[4]])

CN_cand.list[[5]] <- glm(CN_ratio ~ EstimatedAge + K_n + Hare_Sex + Hare_Sex:K_n + EstimatedAge:K_n, data = HareMorphStoich_norep)
summary(CN_cand.list[[5]])
plot(CN_cand.list[[5]])

CN_cand.list[[6]] <- glm(CN_ratio ~ EstimatedAge, data = HareMorphStoich_norep)
summary(CN_cand.list[[6]])
plot(CN_cand.list[[6]])

CN_cand.list[[7]] <- glm(CN_ratio ~ K_n, data = HareMorphStoich_norep)
summary(CN_cand.list[[7]])
plot(CN_cand.list[[7]])

CN_cand.list[[8]] <- glm(CN_ratio ~ Hare_Sex, data = HareMorphStoich_norep)
summary(CN_cand.list[[8]])
plot(CN_cand.list[[8]])

# Eight model with Age + Sex
CN_cand.list[[9]] <- glm(CN_ratio ~ EstimatedAge + Hare_Sex, data = HareMorphStoich_norep) 
summary(CN_cand.list[[9]])
plot(CN_cand.list[[9]])

# Ninth model with Sex + K_n
CN_cand.list[[10]] <- glm(CN_ratio ~ Hare_Sex + K_n, data = HareMorphStoich_norep) 
summary(CN_cand.list[[10]])
plot(CN_cand.list[[10]])

# Tenth model with Age + K_n
CN_cand.list[[11]] <- glm(CN_ratio ~ EstimatedAge + K_n, data = HareMorphStoich_norep) 
summary(CN_cand.list[[11]])
plot(CN_cand.list[[11]])

# 11th model with Age + K_n + Age:K_n
CN_cand.list[[12]] <- glm(CN_ratio ~ EstimatedAge + K_n + EstimatedAge:K_n, data = HareMorphStoich_norep)
summary(CN_cand.list[[12]])
plot(CN_cand.list[[12]])

# 12th model with Sex + K_n + Sex:K_n
CN_cand.list[[13]] <- glm(CN_ratio ~ Hare_Sex + K_n + Hare_Sex:K_n, data = HareMorphStoich_norep)
summary(CN_cand.list[[13]])
plot(CN_cand.list[[13]])

# 13th model with Age + Sex + Age:Sex
CN_cand.list[[14]] <- glm(CN_ratio ~ EstimatedAge+ Hare_Sex + EstimatedAge:Hare_Sex, data = HareMorphStoich_norep)
summary(CN_cand.list[[14]])
plot(CN_cand.list[[14]])

# 14th model with AvgBodyLength in place of K_n, only additive effects
CN_cand.list[[15]] <- glm(CN_ratio ~ EstimatedAge + AvgBodyLength + Hare_Sex, data = HareMorphStoich_norep)
summary(CN_cand.list[[15]])
plot(CN_cand.list[[15]])

# 15th model with Sex and AvgBodyLength interatcion
CN_cand.list[[16]] <- glm(CN_ratio ~ EstimatedAge + AvgBodyLength + Hare_Sex + Hare_Sex:AvgBodyLength, data = HareMorphStoich_norep)
summary(CN_cand.list[[16]])
plot(CN_cand.list[[16]])

# 16th model with Age and AvgBodyLength interaction
CN_cand.list[[17]] <- glm(CN_ratio ~ EstimatedAge + AvgBodyLength + Hare_Sex + EstimatedAge:AvgBodyLength, data = HareMorphStoich_norep)
summary(CN_cand.list[[17]])
plot(CN_cand.list[[17]])

# 17th model with both interactions
CN_cand.list[[18]] <- glm(CN_ratio ~ EstimatedAge + AvgBodyLength + Hare_Sex + Hare_Sex:AvgBodyLength + EstimatedAge:AvgBodyLength, data = HareMorphStoich_norep)
summary(CN_cand.list[[18]])
plot(CN_cand.list[[18]])

# 18th model just AvgBodyLength
CN_cand.list[[19]] <- glm(CN_ratio ~ AvgBodyLength, data = HareMorphStoich_norep)
summary(CN_cand.list[[19]])
plot(CN_cand.list[[19]])

# 19th model with Sex + AvgBodyLength
CN_cand.list[[20]] <- glm(CN_ratio ~ Hare_Sex + AvgBodyLength, data = HareMorphStoich_norep) 
summary(CN_cand.list[[20]])
plot(CN_cand.list[[20]])

# 20th model with Age + AvgBodyLength
CN_cand.list[[21]] <- glm(CN_ratio ~ EstimatedAge + AvgBodyLength, data = HareMorphStoich_norep) 
summary(CN_cand.list[[21]])
plot(CN_cand.list[[21]])

# 21st model with Age + AvgBodyLength + Age:AvgBodyLength
CN_cand.list[[22]] <- glm(CN_ratio ~ EstimatedAge + AvgBodyLength + EstimatedAge:AvgBodyLength, data = HareMorphStoich_norep)
summary(CN_cand.list[[22]])
plot(CN_cand.list[[22]])

# 22nd model with Sex + AvgBodyLength + Sex:AvgBodyLength
CN_cand.list[[23]] <- glm(CN_ratio ~ Hare_Sex + AvgBodyLength + Hare_Sex:AvgBodyLength, data = HareMorphStoich_norep)
summary(CN_cand.list[[23]])
plot(CN_cand.list[[23]])


# Now let's produce the AICc model selection table for CN
CN_tab_out <- aictab(cand.set = CN_cand.list, modnames = Mod.names, sort = TRUE, digits = 3, second.ord = TRUE, LL = TRUE) 


# Now let's add the r squared values to the AICc table

# First, I will combine CN_cand.list and Mod.names into a dataframe

rm(list = c("storeR2", "tempID", "temp")) # removes previously created elements

storeR2 <- data.frame(Modnames = Mod.names, R2 = as.numeric(""))

# The following loop extracts the name of one model at the time from Mod.names, 
# matches it to the name in storeR2, then selects the corresponding model from 
# CN_cand.list, calculates the R^2 and stores it in the corresponding line in 
# storeR2

for (i in 1:length(Mod.names)){
  # 1. create a temporary ID by matching model name between Mod.names and storeR2
  tempID <-  which(storeR2$Modnames == Mod.names[i]) 
  
  # 2. match the tempID to the correct model in CN_cand.list and store it in a 
  #    temporary dataframe  
  temp <- CN_cand.list[[tempID]]
  
  # 3. calculate the R^2 of the model store in temp and copy it to storeR2  
  storeR2$R2[i] <- rsq(temp, type = "v")
  # browser()
}

# combine CN_tab_out with storeR2 to produce an AICc table with R2 values

CN_AICcTab <- merge(CN_tab_out, storeR2, by = "Modnames") %>% dplyr::arrange(., Delta_AICc) %>% gt() %>% cols_hide(columns = vars(ModelLik, AICcWt, Cum.Wt)) %>% cols_label(Modnames = "Model", K = "k", Delta_AICc = "Delta AICc", R2 = "R^2")%>% fmt_number(columns = vars(AICc, Delta_AICc, LL, R2), decimals = 3)

gtsave(CN_AICcTab, filename = "../Results/CN_AICcTab.tex") 



#### C:P ratio ####

# First, let's allocate the empty candidate list
CP_cand.list <- list()

# the vector of names doesn't need to change

# Second, let's run the models

CP_cand.list[[1]] <- glm(CP_ratio ~ 1, data = HareMorphStoich_norep)
summary(CP_cand.list[[1]])
plot(CP_cand.list[[1]])

CP_cand.list[[2]] <- glm(CP_ratio ~ EstimatedAge + K_n + Hare_Sex, data = HareMorphStoich_norep)
plot(CP_cand.list[[2]])

CP_cand.list[[3]] <- glm(CP_ratio ~ EstimatedAge + K_n + Hare_Sex + Hare_Sex:K_n, data = HareMorphStoich_norep)
summary(CP_cand.list[[3]])
plot(CP_cand.list[[3]])

CP_cand.list[[4]] <- glm(CP_ratio ~ EstimatedAge + K_n + Hare_Sex + EstimatedAge:K_n, data = HareMorphStoich_norep)
summary(CP_cand.list[[4]])
plot(CP_cand.list[[4]])

CP_cand.list[[5]] <- glm(CP_ratio ~ EstimatedAge + K_n + Hare_Sex + Hare_Sex:K_n + EstimatedAge:K_n, data = HareMorphStoich_norep)
summary(CP_cand.list[[5]])
plot(CP_cand.list[[5]])

CP_cand.list[[6]] <- glm(CP_ratio ~ EstimatedAge, data = HareMorphStoich_norep)
summary(CP_cand.list[[6]])
plot(CP_cand.list[[6]])

CP_cand.list[[7]] <- glm(CP_ratio ~ K_n, data = HareMorphStoich_norep)
summary(CP_cand.list[[7]])
plot(CP_cand.list[[7]])

CP_cand.list[[8]] <- glm(CP_ratio ~ Hare_Sex, data = HareMorphStoich_norep)
summary(CP_cand.list[[8]])
plot(CP_cand.list[[8]])

# Eight model with Age + Sex
CP_cand.list[[9]] <- glm(CP_ratio ~ EstimatedAge + Hare_Sex, data = HareMorphStoich_norep) 
summary(CP_cand.list[[9]])
plot(CP_cand.list[[9]])

# Ninth model with Sex + K_n
CP_cand.list[[10]] <- glm(CP_ratio ~ Hare_Sex + K_n, data = HareMorphStoich_norep) 
summary(CP_cand.list[[10]])
plot(CP_cand.list[[10]])

# Tenth model with Age + K_n
CP_cand.list[[11]] <- glm(CP_ratio ~ EstimatedAge + K_n, data = HareMorphStoich_norep) 
summary(CP_cand.list[[11]])
plot(CP_cand.list[[11]])

# 11th model with Age + K_n + Age:K_n
CP_cand.list[[12]] <- glm(CP_ratio ~ EstimatedAge + K_n + EstimatedAge:K_n, data = HareMorphStoich_norep)
summary(CP_cand.list[[12]])
plot(CP_cand.list[[12]])

# 12th model with Sex + K_n + Sex:K_n
CP_cand.list[[13]] <- glm(CP_ratio ~ Hare_Sex + K_n + Hare_Sex:K_n, data = HareMorphStoich_norep)
summary(CP_cand.list[[13]])
plot(CP_cand.list[[13]])

# 13th model with Age + Sex + Age:Sex
CP_cand.list[[14]] <- glm(CP_ratio ~ EstimatedAge+ Hare_Sex + EstimatedAge:Hare_Sex, data = HareMorphStoich_norep)
summary(CP_cand.list[[14]])
plot(CP_cand.list[[14]])

# 14th model with AvgBodyLength in place of K_n
CP_cand.list[[15]] <- glm(CP_ratio ~ EstimatedAge + AvgBodyLength + Hare_Sex, data = HareMorphStoich_norep)
summary(CP_cand.list[[15]])
plot(CP_cand.list[[15]])

# 15th model with interaction Sex:AvgBodyLength
CP_cand.list[[16]] <- glm(CP_ratio ~ EstimatedAge + AvgBodyLength + Hare_Sex + Hare_Sex:AvgBodyLength, data = HareMorphStoich_norep)
summary(CP_cand.list[[16]])
plot(CP_cand.list[[16]])

# 16th model with Age:AvgBodyLength interaction
CP_cand.list[[17]] <- glm(CP_ratio ~ EstimatedAge + AvgBodyLength + Hare_Sex + EstimatedAge:AvgBodyLength, data = HareMorphStoich_norep)
summary(CP_cand.list[[17]])
plot(CP_cand.list[[17]])

# 17th model with both interactions
CP_cand.list[[18]] <- glm(CP_ratio ~ EstimatedAge + AvgBodyLength + Hare_Sex + Hare_Sex:AvgBodyLength + EstimatedAge:AvgBodyLength, data = HareMorphStoich_norep)
summary(CP_cand.list[[18]])
plot(CP_cand.list[[18]])

# 18thmodel with just AvgBodyLength
CP_cand.list[[19]] <- glm(CP_ratio ~ AvgBodyLength, data = HareMorphStoich_norep)
summary(CP_cand.list[[19]])
plot(CP_cand.list[[19]])

# 19th model with Sex + AvgBodyLength
CP_cand.list[[20]] <- glm(CP_ratio ~ Hare_Sex + AvgBodyLength, data = HareMorphStoich_norep) 
summary(CP_cand.list[[20]])
plot(CP_cand.list[[20]])

# 20th model with Age + AvgBodyLength
CP_cand.list[[21]] <- glm(CP_ratio ~ EstimatedAge + AvgBodyLength, data = HareMorphStoich_norep) 
summary(CP_cand.list[[21]])
plot(CP_cand.list[[21]])

# 21st model with Age + AvgBodyLength + Age:AvgBodyLength
CP_cand.list[[22]] <- glm(CP_ratio ~ EstimatedAge + AvgBodyLength + EstimatedAge:AvgBodyLength, data = HareMorphStoich_norep)
summary(CP_cand.list[[22]])
plot(CP_cand.list[[22]])

# 22nd model with Sex + AvgBodyLength + Sex:AvgBodyLength
CP_cand.list[[23]]<- glm(CP_ratio ~ Hare_Sex + AvgBodyLength + Hare_Sex:AvgBodyLength, data = HareMorphStoich_norep)
summary(CP_cand.list[[23]])
plot(CP_cand.list[[23]])


# Now, let's produce the AICc model selection table for CP
CP_tab_out <- aictab(cand.set = CP_cand.list, modnames = Mod.names, sort = TRUE, 
                     digits = 3, second.ord = TRUE, LL = TRUE)


# Now let's add the r squared values to the AICc table

# First, I will combine CP_cand.list and Mod.names into a dataframe

rm(list = c("storeR2", "tempID", "temp")) # removes previously created elements

storeR2 <- data.frame(Modnames = Mod.names, R2 = as.numeric(""))

# The following loop extracts the name of one model at the time from Mod.names, 
# matches it to the name in storeR2, then selects the corresponding model from 
# CP_cand.list, calculates the R^2 and stores it in the corresponding line in 
# storeR2

for (i in 1:length(Mod.names)){
  # 1. create a temporary ID by matching model name between Mod.names and storeR2
  tempID <-  which(storeR2$Modnames == Mod.names[i]) 
  
  # 2. match the tempID to the correct model in CP_cand.list and store it in a 
  #    temporary dataframe  
  temp <- CP_cand.list[[tempID]]
  
  # 3. calculate the R^2 of the model store in temp and copy it to storeR2  
  storeR2$R2[i] <- rsq(temp, type = "v")
  # browser()
}

# combine CP_tab_out with storeR2 to produce an AICc table with R2 values

CP_AICcTab <- merge(CP_tab_out, storeR2, by = "Modnames") %>% dplyr::arrange(., Delta_AICc) %>% gt() %>% cols_hide(columns = vars(ModelLik, AICcWt, Cum.Wt)) %>% cols_label(Modnames = "Model", K = "k", Delta_AICc = "Delta AICc", R2 = "R^2")%>% fmt_number(columns = vars(AICc, Delta_AICc, LL, R2), decimals = 3) 

gtsave(CP_AICcTab, filename = "../Results/CP_AICcTab.tex") 


#### N:P ratio ####

# First, let's allocate the empty candidate model list
NP_cand.list <- list()

# the model names vector does not need to change 

# Second let's run some models!

NP_cand.list[[1]] <- glm(NP_ratio ~ 1, data = HareMorphStoich_norep)
summary(NP_cand.list[[1]])
plot(NP_cand.list[[1]])

NP_cand.list[[2]] <- glm(NP_ratio ~ EstimatedAge + K_n + Hare_Sex, data = HareMorphStoich_norep)
summary(NP_cand.list[[2]])
plot(NP_cand.list[[2]])

NP_cand.list[[3]] <- glm(NP_ratio ~ EstimatedAge + K_n + Hare_Sex + Hare_Sex:K_n, data = HareMorphStoich_norep)
summary(NP_cand.list[[3]])
plot(NP_cand.list[[3]])

NP_cand.list[[4]] <- glm(NP_ratio ~ EstimatedAge + K_n + Hare_Sex + EstimatedAge:K_n, data = HareMorphStoich_norep)
summary(NP_cand.list[[4]])
plot(NP_cand.list[[4]])

NP_cand.list[[5]] <- glm(NP_ratio ~ EstimatedAge + K_n + Hare_Sex + Hare_Sex:K_n + EstimatedAge:K_n, data = HareMorphStoich_norep)
summary(NP_cand.list[[5]])
plot(NP_cand.list[[5]])

NP_cand.list[[6]] <- glm(NP_ratio ~ EstimatedAge, data = HareMorphStoich_norep)
summary(NP_cand.list[[6]])
plot(NP_cand.list[[6]])

NP_cand.list[[7]] <- glm(NP_ratio ~ K_n, data = HareMorphStoich_norep)
summary(NP_cand.list[[7]])
plot(NP_cand.list[[7]])

NP_cand.list[[8]] <- glm(NP_ratio ~ Hare_Sex, data = HareMorphStoich_norep)
summary(NP_cand.list[[8]])
plot(NP_cand.list[[8]])

# Eight model with Age + Sex
NP_cand.list[[9]] <- glm(NP_ratio ~ EstimatedAge + Hare_Sex, data = HareMorphStoich_norep) 
summary(NP_cand.list[[9]])
plot(NP_cand.list[[9]])

# Ninth model with Sex + K_n
NP_cand.list[[10]] <- glm(NP_ratio ~ Hare_Sex + K_n, data = HareMorphStoich_norep) 
summary(NP_cand.list[[10]])
plot(NP_cand.list[[10]])

# Tenth model with Age + K_n
NP_cand.list[[11]] <- glm(NP_ratio ~ EstimatedAge + K_n, data = HareMorphStoich_norep) 
summary(NP_cand.list[[11]])
plot(NP_cand.list[[11]])

# 11th model with Age + K_n + Age:K_n
NP_cand.list[[12]] <- glm(NP_ratio ~ EstimatedAge + K_n + EstimatedAge:K_n, data = HareMorphStoich_norep)
summary(NP_cand.list[[12]])
plot(NP_cand.list[[12]])

# 12th model with Sex + K_n + Sex:K_n
NP_cand.list[[13]] <- glm(NP_ratio ~ Hare_Sex + K_n + Hare_Sex:K_n, data = HareMorphStoich_norep)
summary(NP_cand.list[[13]])
plot(NP_cand.list[[13]])

# 13th model with Age + Sex + Age:Sex
NP_cand.list[[14]] <- glm(NP_ratio ~ EstimatedAge+ Hare_Sex + EstimatedAge:Hare_Sex, data = HareMorphStoich_norep)
summary(NP_cand.list[[14]])
plot(NP_cand.list[[14]])

# 14th model with AvgBodyLength in place of K_n
NP_cand.list[[15]] <- glm(NP_ratio ~ EstimatedAge + AvgBodyLength + Hare_Sex, data = HareMorphStoich_norep)
summary(NP_cand.list[[15]])
plot(NP_cand.list[[15]])

# 15th model wit Sex and AvgBodyLength interaction
NP_cand.list[[16]] <- glm(NP_ratio ~ EstimatedAge + AvgBodyLength + Hare_Sex + Hare_Sex:AvgBodyLength, data = HareMorphStoich_norep)
summary(NP_cand.list[[16]])
plot(NP_cand.list[[16]])

# 16th model with Age and AvgBodyLength interaction
NP_cand.list[[17]] <- glm(NP_ratio ~ EstimatedAge + AvgBodyLength + Hare_Sex + EstimatedAge:AvgBodyLength, data = HareMorphStoich_norep)
summary(NP_cand.list[[17]])
plot(NP_cand.list[[17]])

# 17th model with both interactions
NP_cand.list[[18]] <- glm(NP_ratio ~ EstimatedAge + AvgBodyLength + Hare_Sex + Hare_Sex:AvgBodyLength + EstimatedAge:AvgBodyLength, data = HareMorphStoich_norep)
summary(NP_cand.list[[18]])
plot(NP_cand.list[[18]])

# 18th model with only AvgBodyLength
NP_cand.list[[19]] <- glm(NP_ratio ~ AvgBodyLength, data = HareMorphStoich_norep)
summary(NP_cand.list[[19]])
plot(NP_cand.list[[19]])

# 19th model with Sex + AvgBodyLength
NP_cand.list[[20]] <- glm(NP_ratio ~ Hare_Sex + AvgBodyLength, data = HareMorphStoich_norep) 
summary(NP_cand.list[[20]])
plot(NP_cand.list[[20]])

# 20th model with Age + AvgBodyLength
NP_cand.list[[21]] <- glm(NP_ratio ~ EstimatedAge + AvgBodyLength, data = HareMorphStoich_norep) 
summary(NP_cand.list[[21]])
plot(NP_cand.list[[21]])

# 21st model with Age + AvgBodyLength + Age:AvgBodyLength
NP_cand.list[[22]] <- glm(NP_ratio ~ EstimatedAge + AvgBodyLength + EstimatedAge:AvgBodyLength, data = HareMorphStoich_norep)
summary(NP_cand.list[[22]])
plot(NP_cand.list[[22]])

# 22nd model with Sex + AvgBodyLength + Sex:AvgBodyLength
NP_cand.list[[23]] <- glm(NP_ratio ~ Hare_Sex + AvgBodyLength + Hare_Sex:AvgBodyLength, data = HareMorphStoich_norep)
summary(NP_cand.list[[23]])
plot(NP_cand.list[[23]])

# Now let's produce the AICc model selection table for NP
NP_tab_out <- aictab(cand.set = NP_cand.list, modnames = Mod.names, sort = TRUE, 
                     digits = 3, second.ord = TRUE, LL = TRUE)


# Now let's add the r squared values to the AICc table

# First, I will combine NP_cand.list and Mod.names into a dataframe

rm(list = c("storeR2", "tempID", "temp")) # removes previously created elements

storeR2 <- data.frame(Modnames = Mod.names, R2 = as.numeric(""))

# The following loop extracts the name of one model at the time from Mod.names, 
# matches it to the name in storeR2, then selects the corresponding model from 
# NP_cand.list, calculates the R^2 and stores it in the corresponding line in 
# storeR2

for (i in 1:length(Mod.names)){
  # 1. create a temporary ID by matching model name between Mod.names and storeR2
  tempID <-  which(storeR2$Modnames == Mod.names[i]) 
  
  # 2. match the tempID to the correct model in NP_cand.list and store it in a 
  #    temporary dataframe  
  temp <- NP_cand.list[[tempID]]
  
  # 3. calculate the R^2 of the model store in temp and copy it to storeR2  
  storeR2$R2[i] <- rsq(temp, type = "v")
  # browser()
}

# combine NP_tab_out with storeR2 to produce an AICc table with R2 values

NP_AICcTab <- merge(NP_tab_out, storeR2, by = "Modnames") %>% dplyr::arrange(., Delta_AICc) %>% gt() %>% cols_hide(columns = vars(ModelLik, AICcWt, Cum.Wt)) %>% cols_label(Modnames = "Model", K = "k", Delta_AICc = "Delta AICc", R2 = "R^2")%>% fmt_number(columns = vars(AICc, Delta_AICc, LL, R2), decimals = 3)

gtsave(NP_AICcTab, filename = "../Results/NP_AICcTab.tex") 



###### Using Skull Length to calculate SMI (SkK_n) ######
# Do results change if I run the same models but with body condition calculated
# usig skull lenght rather than left hind foot length?

#### C:N ratio ####
# The only models that I need to rerun are the ones that originally included K_n

# I will use the Mod.names_Sk vector of names produce in the Element modelling 
# script

CN_cand.list[[2]] <- glm(CN_ratio ~ EstimatedAge + SkK_n + Hare_Sex, data = HareMorphStoich_norep)
summary(CN_cand.list[[2]])
plot(CN_cand.list[[2]])

CN_cand.list[[3]] <- glm(CN_ratio ~ EstimatedAge + SkK_n + Hare_Sex + Hare_Sex:SkK_n, data = HareMorphStoich_norep)
summary(CN_cand.list[[3]])
plot(CN_cand.list[[3]])

CN_cand.list[[4]] <- glm(CN_ratio ~ EstimatedAge + SkK_n + Hare_Sex + EstimatedAge:SkK_n, data = HareMorphStoich_norep)
summary(CN_cand.list[[4]])
plot(CN_cand.list[[4]])

CN_cand.list[[5]] <- glm(CN_ratio ~ EstimatedAge + SkK_n + Hare_Sex + Hare_Sex:SkK_n + EstimatedAge:SkK_n, data = HareMorphStoich_norep)
summary(CN_cand.list[[5]])
plot(CN_cand.list[[5]])

CN_cand.list[[7]] <- glm(CN_ratio ~ SkK_n, data = HareMorphStoich_norep)
summary(CN_cand.list[[7]])
plot(CN_cand.list[[7]])

# Ninth model with Sex + SkK_n
CN_cand.list[[10]] <- glm(CN_ratio ~ Hare_Sex + SkK_n, data = HareMorphStoich_norep)
summary(CN_cand.list[[10]])
plot(CN_cand.list[[10]])

# Tenth model with Age + SkK_n
CN_cand.list[[11]] <- glm(CN_ratio ~ EstimatedAge + SkK_n, data = HareMorphStoich_norep) 
summary(CN_cand.list[[11]])
plot(CN_cand.list[[11]])

# 11th model with Age + SkK_n + Age:SkK_n
CN_cand.list[[12]] <- glm(CN_ratio ~ EstimatedAge + SkK_n + EstimatedAge:SkK_n, data = HareMorphStoich_norep)
summary(CN_cand.list[[12]])
plot(CN_cand.list[[12]])

# 12th model with Sex + SkK_n + Sex:SkK_n
CN_cand.list[[13]] <- glm(CN_ratio ~ Hare_Sex + SkK_n + Hare_Sex:SkK_n, data = HareMorphStoich_norep)
summary(CN_cand.list[[13]])
plot(CN_cand.list[[13]])


# Now let's produce the new AICc model selection table for CN
SkCN_tab_out <- aictab(cand.set = CN_cand.list, modnames = Mod.names_Sk, 
                       sort = TRUE, digits = 3, second.ord = TRUE, LL = TRUE)


# Now let's add the r squared values to the AICc table

# First, I will combine CN_cand.list and Mod.names into a dataframe

rm(list = c("storeR2", "tempID", "temp")) # removes previously created elements

storeR2 <- data.frame(Modnames = Mod.names_Sk, R2 = as.numeric(""))

# The following loop extracts the name of one model at the time from Mod.names, 
# matches it to the name in storeR2, then selects the corresponding model from 
# CN_cand.list, calculates the R^2 and stores it in the corresponding line in 
# storeR2

for (i in 1:length(Mod.names_Sk)){
  # 1. create a temporary ID by matching model name between Mod.names and storeR2
  tempID <-  which(storeR2$Modnames == Mod.names_Sk[i]) 
  
  # 2. match the tempID to the correct model in CN_cand.list and store it in a 
  #    temporary dataframe  
  temp <- CN_cand.list[[tempID]]
  
  # 3. calculate the R^2 of the model store in temp and copy it to storeR2  
  storeR2$R2[i] <- rsq(temp, type = "v")
  # browser()
}

# combine SkCN_tab_out with storeR2 to produce an AICc table with R2 values

SkCN_AICcTab <- merge(SkCN_tab_out, storeR2, by = "Modnames") %>% dplyr::arrange(., Delta_AICc) %>% gt() %>% cols_hide(columns = vars(ModelLik, AICcWt, Cum.Wt)) %>% cols_label(Modnames = "Model", K = "k", Delta_AICc = "Delta AICc", R2 = "R^2")%>% fmt_number(columns = vars(AICc, Delta_AICc, LL, R2), decimals = 3)

gtsave(SkCN_AICcTab, filename = "../Results/SkCN_AICcTab.tex") 



#### C:P ratio ####
# same goes for CP ratio, only the model with body condition need changing

CP_cand.list[[2]] <- glm(CP_ratio ~ EstimatedAge + SkK_n + Hare_Sex, data = HareMorphStoich_norep)
summary(CP_cand.list[[2]])
plot(CP_cand.list[[2]])

CP_cand.list[[3]] <- glm(CP_ratio ~ EstimatedAge + SkK_n + Hare_Sex + Hare_Sex:SkK_n, data = HareMorphStoich_norep)
summary(CP_cand.list[[3]])
plot(CP_cand.list[[3]])

CP_cand.list[[4]] <- glm(CP_ratio ~ EstimatedAge + SkK_n + Hare_Sex + EstimatedAge:SkK_n, data = HareMorphStoich_norep)
summary(CP_cand.list[[4]])
plot(CP_cand.list[[4]])

CP_cand.list[[5]] <- glm(CP_ratio ~ EstimatedAge + SkK_n + Hare_Sex + Hare_Sex:SkK_n + EstimatedAge:SkK_n, data = HareMorphStoich_norep)
summary(CP_cand.list[[5]])
plot(CP_cand.list[[5]])

CP_cand.list[[7]] <- glm(CP_ratio ~ SkK_n, data = HareMorphStoich_norep)
summary(CP_cand.list[[7]])
plot(CP_cand.list[[7]])

# Ninth model with Sex + SkK_n
CP_cand.list[[10]] <- glm(CP_ratio ~ Hare_Sex + SkK_n, data = HareMorphStoich_norep) 
summary(CP_cand.list[[10]])
plot(CP_cand.list[[10]])

# Tenth model with Age + SkK_n
CP_cand.list[[11]] <- glm(CP_ratio ~ EstimatedAge + SkK_n, data = HareMorphStoich_norep) 
summary(CP_cand.list[[11]])
plot(CP_cand.list[[11]])

# 11th model with Age + SkK_n + Age:SkK_n
CP_cand.list[[12]] <- glm(CP_ratio ~ EstimatedAge + SkK_n + EstimatedAge:SkK_n, data = HareMorphStoich_norep)
summary(CP_cand.list[[12]])
plot(CP_cand.list[[12]])

# 12th model with Sex + SkK_n + Sex:SkK_n
CP_cand.list[[13]] <- glm(CP_ratio ~ Hare_Sex + SkK_n + Hare_Sex:SkK_n, data = HareMorphStoich_norep)
summary(CP_cand.list[[13]])
plot(CP_cand.list[[13]])

# Produce the new AICc model selection table for CP
SkCP_tab_out <- aictab(cand.set = CP_cand.list, modnames = Mod.names_Sk, 
                       sort = TRUE, digits = 3, second.ord = TRUE, LL = TRUE)


# Now let's add the r squared values to the AICc table

# First, I will combine CP_cand.list and Mod.names into a dataframe

rm(list = c("storeR2", "tempID", "temp")) # removes previously created elements

storeR2 <- data.frame(Modnames = Mod.names_Sk, R2 = as.numeric(""))

# The following loop extracts the name of one model at the time from Mod.names, 
# matches it to the name in storeR2, then selects the corresponding model from 
# CP_cand.list, calculates the R^2 and stores it in the corresponding line in 
# storeR2

for (i in 1:length(Mod.names_Sk)){
  # 1. create a temporary ID by matching model name between Mod.names and storeR2
  tempID <-  which(storeR2$Modnames == Mod.names_Sk[i]) 
  
  # 2. match the tempID to the correct model in CP_cand.list and store it in a 
  #    temporary dataframe  
  temp <- CP_cand.list[[tempID]]
  
  # 3. calculate the R^2 of the model store in temp and copy it to storeR2  
  storeR2$R2[i] <- rsq(temp, type = "v")
  # browser()
}

# combine SkCP_tab_out with storeR2 to produce an AICc table with R2 values

SkCP_AICcTab <- merge(SkCP_tab_out, storeR2, by = "Modnames") %>% dplyr::arrange(., Delta_AICc) %>% gt() %>% cols_hide(columns = vars(ModelLik, AICcWt, Cum.Wt)) %>% cols_label(Modnames = "Model", K = "k", Delta_AICc = "Delta AICc", R2 = "R^2")%>% fmt_number(columns = vars(AICc, Delta_AICc, LL, R2), decimals = 3)

gtsave(SkCP_AICcTab, filename = "../Results/SkCP_AICcTab.tex") 



#### N:P ratio ####
# same goes for NP, only models with body codition need editing

NP_cand.list[[2]] <- glm(NP_ratio ~ EstimatedAge + SkK_n + Hare_Sex, data = HareMorphStoich_norep)
summary(NP_cand.list[[2]])
plot(NP_cand.list[[2]])

NP_cand.list[[3]] <- glm(NP_ratio ~ EstimatedAge + SkK_n + Hare_Sex + Hare_Sex:SkK_n, data = HareMorphStoich_norep)
summary(NP_cand.list[[3]])
plot(NP_cand.list[[3]])

NP_cand.list[[4]] <- glm(NP_ratio ~ EstimatedAge + SkK_n + Hare_Sex + EstimatedAge:SkK_n, data = HareMorphStoich_norep)
summary(NP_cand.list[[4]])
plot(NP_cand.list[[4]])

NP_cand.list[[5]] <- glm(NP_ratio ~ EstimatedAge + SkK_n + Hare_Sex + Hare_Sex:SkK_n + EstimatedAge:SkK_n, data = HareMorphStoich_norep)
summary(NP_cand.list[[5]])
plot(NP_cand.list[[5]])

NP_cand.list[[7]] <- glm(NP_ratio ~ SkK_n, data = HareMorphStoich_norep)
summary(NP_cand.list[[7]])
plot(NP_cand.list[[7]])

# Ninth model with Sex + SkK_n
NP_cand.list[[10]] <- glm(NP_ratio ~ Hare_Sex + SkK_n, data = HareMorphStoich_norep) 
summary(NP_cand.list[[10]])
plot(NP_cand.list[[10]])

# Tenth model with Age + SkK_n
NP_cand.list[[11]] <- glm(NP_ratio ~ EstimatedAge + SkK_n, data = HareMorphStoich_norep) 
summary(NP_cand.list[[11]])
plot(NP_cand.list[[11]])

# 11th model with Age + SkK_n + Age:SkK_n
NP_cand.list[[12]] <- glm(NP_ratio ~ EstimatedAge + SkK_n + EstimatedAge:SkK_n, data = HareMorphStoich_norep)
summary(NP_cand.list[[12]])
plot(NP_cand.list[[12]])

# 12th model with Sex + SkK_n + Sex:SkK_n
NP_cand.list[[13]] <- glm(NP_ratio ~ Hare_Sex + SkK_n + Hare_Sex:SkK_n, data = HareMorphStoich_norep)
summary(NP_cand.list[[13]])
plot(NP_cand.list[[13]])

# Produce the new AICc model selection table for CP
SkNP_tab_out <- aictab(cand.set = NP_cand.list, modnames = Mod.names_Sk, 
                       sort = TRUE, digits = 3, second.ord = TRUE, LL = TRUE)


# Now let's add the r squared values to the AICc table

# First, I will combine CP_cand.list and Mod.names into a dataframe

rm(list = c("storeR2", "tempID", "temp")) # removes previously created elements

storeR2 <- data.frame(Modnames = Mod.names_Sk, R2 = as.numeric(""))

# The following loop extracts the name of one model at the time from Mod.names, 
# matches it to the name in storeR2, then selects the corresponding model from 
# CP_cand.list, calculates the R^2 and stores it in the corresponding line in 
# storeR2

for (i in 1:length(Mod.names_Sk)){
  # 1. create a temporary ID by matching model name between Mod.names and storeR2
  tempID <-  which(storeR2$Modnames == Mod.names_Sk[i]) 
  
  # 2. match the tempID to the correct model in CP_cand.list and store it in a 
  #    temporary dataframe  
  temp <- NP_cand.list[[tempID]]
  
  # 3. calculate the R^2 of the model store in temp and copy it to storeR2  
  storeR2$R2[i] <- rsq(temp, type = "v")
  # browser()
}

# combine SkCP_tab_out with storeR2 to produce an AICc table with R2 values

SkNP_AICcTab <- merge(SkNP_tab_out, storeR2, by = "Modnames") %>% dplyr::arrange(., Delta_AICc) %>% gt() %>% cols_hide(columns = vars(ModelLik, AICcWt, Cum.Wt)) %>% cols_label(Modnames = "Model", K = "k", Delta_AICc = "Delta AICc", R2 = "R^2") %>% fmt_number(columns = vars(AICc, Delta_AICc, LL, R2), decimals = 3)

gtsave(SkNP_AICcTab, filename = "../Results/SkNP_AICcTab.tex") 
