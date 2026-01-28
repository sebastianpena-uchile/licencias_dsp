
# 1. cargar librerias -----------------------------------------------------


#Para cambiar el repositorio
options(repos=structure(c(CRAN="https://cran.dcc.uchile.cl/"))) 

# Limpiar entorno
rm(list = ls()) # limpiar completamente el entorno global environment
gc() # limpiar la memoria virtual utilizada por R
rm() # limpiar un objeto específico

#si no tiene pacman, lo instala
if(!require(pacman)){install.packages("pacman")}

##### Instalar paquetes requeridos (OPCION PRINCIPAL)

if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(psych)){install.packages("psych")}
if(!require(Hmisc)){install.packages("Hmisc")}
if(!require(summarytools)){install.packages("summarytools")}
if(!require(epitools)){install.packages("epitools")}
if(!require(gt)){install.packages("gt")}
if(!require(webshot2)){install.packages("webshot2")}
if(!require(readr)){install.packages("readr")}

##### Cargar paquetes (OTRA ALTERNATIVA)
try(pacman::p_load(tidyverse,   # Probablemente el paquete conjunto de paquetes más últil que usarán en R
                   foreign,         # Paquete import datos
                   Hmisc,           # Paquete con funciones variadas
                   psych,
                   epitools,
                   gt,
                   des,
                   webshot2,
                   readr,# Paquete con algunas funciones comúnmente utilizadas (https://personality-project.org/r/psych/intro.pdf)
                   install = F))    # solo cargar, no instalar




# 2. cargar datos ---------------------------------------------------------

LM_ACEPTACION <- read_delim("data/clean/LM_2014_2022_1.ACEPTACION.csv", 
                                        delim = ";", escape_double = FALSE, trim_ws = TRUE)



# 3. Tablas ---------------------------------------------------------------


