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
       modelsummary, #g Coefplot with modelplot
       stargazer, # export tables to latex
       outreg
)  

#========================punto1============================================#
print("espero que tania no me abadone en el trabajo de R")


#========================Punto2============================================#

#====Punto2.1========#

map_muse = readRDS("data/output/f_mapmuse.rds")

lm(fallecido ~ year + condicion + dist_hospi + dist_vias + cod_mpio + actividad + month + dist_cpoblado + genero + tipo_accidente , data = map_muse)

ols = lm(fallecido ~ year + condicion + dist_hospi + dist_vias + cod_mpio + actividad + month + dist_cpoblado + genero + tipo_accidente , data = map_muse)

#====punto2.2=======#

tabla_coef = tidy(ols , conf.int = TRUE)
tabla_coef
ggplot(tabla_coef , aes(x = estimate, y = term)) + theme_light() + 
  geom_vline(aes(xintercept = 0),color="red",linetype="dashed",width=1) + 
  geom_errorbar(width=.5, aes(xmin=conf.low, xmax=conf.high) , col="yellow" , show.legend = F) + 
  geom_point(size = 3,show.legend = F , col="blue") +
  theme(axis.text = element_text(color = "black", size = 15)) + 
  labs(y="",x="coeficientes")

#======punto2.3======#

logit = glm(fallecido ~ year + condicion + dist_hospi + dist_vias + cod_mpio + actividad + month + dist_cpoblado + genero + tipo_accidente , data = map_muse , family = binomial(link="logit"))
logit


probit = glm(fallecido ~ year + condicion + dist_hospi + dist_vias + cod_mpio + actividad + month + dist_cpoblado + genero + tipo_accidente , data = map_muse , family = binomial(link="probit"))
probit

#======punto2.4======#

library(outreg)
regresiones = list('Logit' = logit , 'Probit' = probit , "OLS" = ols)

outreg(regresiones)

tabla = outreg(regresiones, digits = 3L, alpha = c(0.1, 0.05, 0.01), 
              bracket = c("se"), starred = c("coef"), robust = FALSE, small = TRUE,
              constlast = FALSE, norepeat = TRUE)

stargazer(ols, probit, logit,
          type= 'text',
          dep.var.labels = c('','Number of flights',''), 
          df = FALSE,
          digits = 3, 
          out = paste0('modelos.text'))

#en formato word

stargazer(ols, probit, logit,
          type= 'text',
          dep.var.labels = c('','Number of flights',''), 
          df = FALSE,
          digits = 3, 
          out = paste0('modelos.doc'))


#======punto2.5======#

logit_m = margins(logit)
probit_m = margins(probit)

ggplot(logit_m, aes(x = fallecido, y = dydx_dist_hospi)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method="lm" , se=T) + theme_bw()

ggplot(map_muse, aes(logit_m = fallecido, y = dydx_dist_hospi)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method="loess" , se=T) + theme_bw()







