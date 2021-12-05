# Elaborado por: Tania Reina,Hugo Sabogal
# Fecha de elaboracion: 5/12/2021
# Ultima modificacion: 5/11/2021
# Version de R: 4.1.1


rm(list = ls()) # limpia el entorno de R
require(pacman)
p_load(tidyverse, # llamar y/o instalar las librerias de la clase
       broom, # tidy-coefficients
       mfx, # marginal effects
       margins,  # marginal effects
       estimatr, # robust standard errors
       lmtest, # HAC (Newey-West) standard errors
       fixest, # hdfe regressions (feols)
       modelsummary, # Coefplot with modelplot
       stargazer # export tables to latex 
)  


