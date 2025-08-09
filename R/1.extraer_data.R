
# cargar librerias --------------------------------------------------------



library(tidyverse)
library(readxl)




# 2. cargar datos ---------------------------------------------------------


# Vector de años que quieres procesar
years <- 2013:2023 

for (i in years) {
  # Construir la ruta al archivo
  file_path <- paste0("data/raw/rechazos ", i, ".xlsx")
  
  # Leer y asignar con nombre dinámico
  assign(paste0("rechazo_", i), read_excel(file_path), envir = .GlobalEnv)
}




# 3. Grabar ambiente ------------------------------------------------------



save(rechazo_2013,
     rechazo_2014,
     rechazo_2015,
     rechazo_2016,
     rechazo_2017,
     rechazo_2018,
     rechazo_2019,
     rechazo_2020,
     rechazo_2021,
     rechazo_2022,
     rechazo_2023, file = "data/raw/rechazo_db_raw.Rdata")


# 4. Limpiar todo y guardar -----------------------------------------------




rm(list = ls())
gc()
