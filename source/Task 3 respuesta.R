# Elaborado por: Tania Reina,Hugo Sabogal
# Fecha de elaboracion: 5/12/2021
# Ultima modificacion: 5/11/2021
# Version de R: 4.1.1


rm(list = ls()) 
require(pacman)
p_load(tidyverse, 
       broom, 
       mfx, 
       margins,  
       estimatr,
       lmtest, 
       fixest, 
       modelsummary, 
       stargazer, 
       outreg,
       rio,
       readxl,
       htmltools,
       XML,
       rvest)  

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






#========================================punto 3==========================================#

#======punto3.1======#
departamentos_col = "https://es.wikipedia.org/wiki/Departamentos_de_Colombia"

departamentos_col_html = read_html(departamentos_col)

#======punto3.2======#

titulo_pag = departamentos_col_html %>% html_nodes(xpath = '//*[@id="firstHeading"]') %>% html_text()

#======punto3.3======#

tabla = departamentos_col_html %>% html_nodes('table')

tabla[4] %>% html_table(header = T,fill=T)


