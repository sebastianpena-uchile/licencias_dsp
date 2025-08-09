
# 1. cargar librerias -----------------------------------------------------


library(tidyverse)






# 2. cargar datos ---------------------------------------------------------






load("data/raw/rechazo_db_raw.Rdata")





# 2. Editar db ------------------------------------------------------------


# ## 2.1 preparar dbs -----------------------------------------------------

#2016 falta la fila de total
rechazo_2013 <- rechazo_2013 %>%
  rename(
    `Amplíase`   = Ampliese,
    `Autorízase` = Autorizese,
    `Recházase`  = Rechazese,
    `Redúcese`   = Reducese
  ) %>%
  mutate(
    `Pendiente de resolución` = NA,  # columna nueva, todo NA
    TOTAL = `Amplíase` + `Autorízase` + `Recházase` + `Redúcese`
  ) %>%
  select(-any_of("Total"))






#2023 problemas con las filas de hombres y mujeres
rechazo_2023 <- rechazo_2023 %>%
  mutate(`Familia de diagnóstico (CIE-10)` = case_when(
    `...2` == "Hombres" ~ "Hombres",
    `...2` == "Mujeres" ~ "Mujeres",
    TRUE ~ `Familia de diagnóstico (CIE-10)`
  )) %>%
  select(-`...2`)


#eliminar filas de totales


rechazo_2013 <- rechazo_2013 %>%
  slice(-1)
rechazo_2014 <- rechazo_2014 %>%
  slice(-8)
rechazo_2015 <- rechazo_2015 %>%
  slice(-8)


#eliminar fila de totales para 2016-2023

year <- 2016:2023

for (i in year) {
  # Buscar objetos cuyo nombre termine con el año i
  objs <- ls(pattern = paste0(i, "$"), envir = .GlobalEnv)
  
  for (obj in objs) {
    df <- get(obj, envir = .GlobalEnv)   # Obtener el data frame
    df <- df %>%
      slice(-22) # aliminar fila de totales
    assign(obj, df, envir = .GlobalEnv)  # Guardar de nuevo con el mismo nombre
  }
}

rm(df)


#cambiar nombre observaciones

familias_filtrar <- c(
  "Ciertas enfermedades infecciosas y parasitarias",
  "Trastornos mentales y del comportamiento",
  "Enfermedades del sistema respiratorio",
  "Enfermedades del sistema digestivo",
  "Enfermedades del sistema osteomuscular y del tejido conectivo",
  "Traumatismos, envenenamientos y algunas otras consecuencias de causa externa",
  "Otras familias de diagnósticos"
)

year <- 2013:2015

for (i in year) {
  # Buscar objetos cuyo nombre termine con el año i
  objs <- ls(pattern = paste0(i, "$"), envir = .GlobalEnv)
  
  for (obj in objs) {
    df <- get(obj, envir = .GlobalEnv)   # Obtener el data frame
    df$...1<- familias_filtrar # asignar valores correctos
    assign(obj, df, envir = .GlobalEnv)  # Guardar de nuevo con el mismo nombre
  }
}

rm(df)


# 3. Grabar objetos ----------------------------------------------------------




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
     rechazo_2023, file = "data/raw/rechazo_db_raw_ordered.Rdata")



# 4. Limpiar todo y guardar -----------------------------------------------




rm(list = ls())
gc()
