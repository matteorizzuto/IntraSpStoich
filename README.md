# Code and data to reproduce Rizzuto et al. analyses of variability in organismal elemental composition of snowshoe hares
This repository contains code, data, and results for Rizzuto et al. Patterns and potential drivers of intraspecific variability in the body elemental composition of a terrestrial consumer, the snowshoe hare (_Lepus americanus_).

* Code contains the R scripts necessary to replicate our analyses
  * MasterScript.R will load required R packages and run each of the other scripts in this folder in in the correct order
  * StoichAnalyses.R is the first script that should be run, it prepares the elemental composition data for the following analyses
  * HareBodyCondition.R should be run next, it calculates the body condition of the study individuals and produces several objects used in the subsequent modeling
  * Element_Modeling.R should come next, it fits our set of 22 models to the elemental concentration data and saves results as AICc tables in latex format
  * Ratios_Modeling.R runs next, it fits the set of 22 models to the stoichiometric ratios data and saves results as AICc tables in latex format
  * ManuscriptFigures.R should come last, it produces all figures for the manuscript by calling upon the objects produced by the previous scripts

* Data contains all data necessary to replicate our analyses
  * HH_MorphRawData.csv, contains the morphometric data for our snowshoe hare sample
  * DNA_SexAssignements.csv, contains the sex of each individual snowshoe hare, as determined via DNA analyses
  * SSH_C_data.csv, contains data on Carbon content of the 50 snowshoe hares, as % dry weight
  * SSH_N_data.csv, contains data on Nitrogen content of the 50 snowshoe hares, as % dry weight
  * SSH_P_data.csv, contains data on Phosphorus content of the 50 snowshoe hares, as % dry weight
