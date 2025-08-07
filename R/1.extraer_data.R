
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





