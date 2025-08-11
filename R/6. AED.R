
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

##### Cargar paquetes (OTRA ALTERNATIVA)
try(pacman::p_load(tidyverse,   # Probablemente el paquete conjunto de paquetes más últil que usarán en R
                   foreign,         # Paquete import datos
                   Hmisc,           # Paquete con funciones variadas
                   psych,           # Paquete con algunas funciones comúnmente utilizadas (https://personality-project.org/r/psych/intro.pdf)
                   install = F))    # solo cargar, no instalar






# 2. Cargar datos ---------------------------------------------------------








load("data/clean/rechazo_db_totales.Rdata")








# 3. Tables ---------------------------------------------------------------


#rechazadas total y (%) por año totales

# Filtrar solo las filas necesarias
filtro <- rechazo_total_2013_2023$cei10_familia == "Trastornos mentales y del comportamiento"

# Crear la tabla depurada
tabla_rechazo <- data.frame(
  Año = rechazo_total_2013_2023$anho[filtro],
  Licencias_Rechazadas = rechazo_total_2013_2023$Recházase[filtro],
  porcentaje = (rechazo_total_2013_2023$Recházase[filtro] / rechazo_total_2013_2023$TOTAL[filtro]) * 100,
  Total_de_Licencias = rechazo_total_2013_2023$TOTAL[filtro]
)

# Opcional: redondear el porcentaje a 2 decimales
tabla_rechazo$porcentaje <- round(tabla_rechazo$porcentaje, 2)

# Mostrar tabla
tabla_rechazo

#Plot

library(ggplot2)

ggplot(tabla_rechazo, aes(x = Año)) +
  # Barras para Total de Licencias
  geom_col(aes(y = `Total_de_Licencias`, fill = "Total de Licencias"), alpha = 0.6) +
  # Línea para Licencias Rechazadas
  geom_line(aes(y = `Licencias_Rechazadas`, color = "Licencias Rechazadas"), linewidth = 1.2) +
  geom_point(aes(y = `Licencias_Rechazadas`, color = "Licencias Rechazadas"), size = 2) +
  # Títulos y leyendas
  labs(
    title = "Licencias Rechazadas vs Total de Licencias",
    subtitle = "Trastornos mentales y del comportamiento (2013-2023)",
    x = "Año",
    y = "Cantidad de Licencias",
    fill = "Indicador",
    color = "Indicador"
  ) +
  # Colores personalizados
  scale_fill_manual(values = c("Total de Licencias" = "steelblue")) +
  scale_color_manual(values = c("Licencias Rechazadas" = "red")) +
  scale_x_continuous(breaks = unique(tabla_rechazo$Año)) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#grabar figura

ggsave(
  filename = "figures/rechazadas_vs_totales.png",
  bg = "white",
  width = 10,
  height = 6,
  dpi = 300
)
