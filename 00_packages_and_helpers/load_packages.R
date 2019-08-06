# Load packages -----------------------------------------------------------

library(haven)
library(stringr)
library(tidyverse)
library(blockTools)
library(DeclareDesign)
library(mice)
library(crayon)
library(knitr)
library(kableExtra)
library(texreg)
library(broom)
library(lavaan)
library(ggridges)
library(ggpubr)

# Check file path ---------------------------------------------------------

# if(!grepl("boxcryptor",getwd(),TRUE))
#   stop(crayon::bgRed(white("You have not opened the Rproject on the Boxcryptor file path. Try again!")))
