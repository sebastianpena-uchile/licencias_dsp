
# 1. cargar librerias -----------------------------------------------------

library(tidyverse)






# 2. cargar datos ---------------------------------------------------------







load("data/raw/rechazo_db_raw_ordered.Rdata")







# 3. crear variables ------------------------------------------------------



# # 2.1. Crear años ----------------------------------------------------




year <- 2013:2023

for (i in year) {
  # Buscar objetos cuyo nombre termine con el año i
  objs <- ls(pattern = paste0(i, "$"), envir = .GlobalEnv)
  
  for (obj in objs) {
    df <- get(obj, envir = .GlobalEnv)   # Obtener el data frame
    df <- cbind(anho = i, df)            # Añadir columna al inicio
    assign(obj, df, envir = .GlobalEnv)  # Guardar de nuevo con el mismo nombre
  }
}

rm(df)

# # 2.2 Familia CIE 10 ----------------------------------------------------

cei10_familia<-c("Ciertas enfermedades infecciosas y parasitarias",
  "Ciertas enfermedades infecciosas y parasitarias",
  "Ciertas enfermedades infecciosas y parasitarias",
  "Trastornos mentales y del comportamiento",
  "Trastornos mentales y del comportamiento",
  "Trastornos mentales y del comportamiento",
  "Enfermedades del sistema respiratorio",
  "Enfermedades del sistema respiratorio",
  "Enfermedades del sistema respiratorio",
  "Enfermedades del sistema digestivo",
  "Enfermedades del sistema digestivo",
  "Enfermedades del sistema digestivo",
  "Enfermedades del sistema osteomuscular y del tejido conectivo",
  "Enfermedades del sistema osteomuscular y del tejido conectivo",
  "Enfermedades del sistema osteomuscular y del tejido conectivo",
  "Traumatismos, envenenamientos y algunas otras consecuencias de causa externa",
  "Traumatismos, envenenamientos y algunas otras consecuencias de causa externa",
  "Traumatismos, envenenamientos y algunas otras consecuencias de causa externa",
  "Otras familias de diagnósticos",
  "Otras familias de diagnósticos",
  "Otras familias de diagnósticos",
  "Total")

year <- 2016:2023

for (i in year) {
  # Buscar objetos cuyo nombre termine con el año i
  objs <- ls(pattern = paste0(i, "$"), envir = .GlobalEnv)
  
  for (obj in objs) {
    df <- get(obj, envir = .GlobalEnv)   # Obtener el data frame
    df <- df %>%
      mutate(cei10_familia = cei10_familia) %>%
      relocate(cei10_familia, .after = anho)  # Añadir columna al inicio
    assign(obj, df, envir = .GlobalEnv)  # Guardar de nuevo con el mismo nombre
  }
}

rm(df)




# 3. grabar dfs -----------------------------------------------------------



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
     rechazo_2023, file = "data/clean/rechazo_db_clean.Rdata")







