# This is the master script for the analyses and modelling of intraspecific 
# differences in the ecological stoichiometry of snowshoe hares.

# This script loads the required packages, then calls 4 separate individual 
# scripts, each dedicated to a specific task. These are:
#   1. StoichAnalyses.R, imports, cleans, and merges data into a single dataset
#   2. HareBodyCondition.R, estimates body codition using the Scaled Mass Index (Peig & Green, 2009 Oikos; Peigh & Green, 2010 Func Ecol)
#   3. Element_Modeling.R, runs models of elemental variability
#   4. Ratios_Modeling.R, runs models of ratio variability
#   5. RatiosBySexandAge.R, produces plots and figures for the paper


# 1. Setup

# install a few custom packages from their online repo
# as they are not yet available on CRAN
 
# devtools::install_github("vankesteren/firatheme")
# installs a ggplot2 theme that uses the beautiful Fira Sans font for text 
# to install Fira Theme, go here: https://mozilla.github.io/Fira/

devtools::install_github("thomasp85/patchwork")
# the patchwork packages allows for easy combination of multiple ggplot2 
# graphs into a multipanel figure, useful when faceting is not an option

devtools::install_github("rstudio/gt")
# easy production of information-rich tables for publication

# ipak function: function to check if packages are installed, install them if 
# they are not, and then load them in the current R session
# from: https://gist.github.com/chadr/71d758f65261a72ab7dc

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# create a vector of packages names 
packages <- c("tidyverse", "AICcmodavg", "xtable", "rsq", "usdm", "smatr", 
              "plotly", "reshape2", "ggsci", "ggthemes", "ggrepel","ggpmisc",
              "ggpol", "gridExtra", "extrafont", "extrafontdb", "ggpubr", 
              "patchwork", "gt", "firatheme", "patchwork")

# check, install, load packages
ipak(packages) 

# Import fonts to use with Fira Sans theme (this will ask to import fonts, reply saying y/n)
# font_import(prompt = FALSE)


# 2. Call the script that imports and prepares the stoichiometric data
source("StoichAnalyses.R")

# 3. Call the script to estimate Body Condition from the morphometric data
source("HareBodyCondition.R")

# 4. Call the script that runs the set of models for intraspecific variability in elemental content
source("Element_Modeling.R")

# 5. Call the script that runs the same set of models but for the stoichiometric ratios
source("Ratios_Modeling.R")

# 6. Call the script that produce plots and figures
source("ManuscriptFigures.R")

