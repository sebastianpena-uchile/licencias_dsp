
# 1. cargar librerias -----------------------------------------------------


library(tidyverse)






# 2. cargar datos ---------------------------------------------------------






load("data/raw/rechazo_db_raw.Rdata")





# 2. Editar db ------------------------------------------------------------


# ## 2.1 preparar dbs -----------------------------------------------------

#2016 falta la fila de total
rechazo_2013 <- rechazo_2013 %>%
  rename(
    `Amplíese`   = Ampliese,
    `Autorícese` = Autorizese,
    `Rechácese`  = Rechazese,
    `Redúcese`   = Reducese
  ) %>%
  mutate(
    `Pendiente de resolución` = NA,  # columna nueva, todo NA
    TOTAL = `Amplíese` + `Autorícese` + `Rechácese` + `Redúcese`
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





# #2.1 Depurar 2013 -------------------------------------------------------
#problemas filas hombres y mujeres









# # # 2.1.1 Crear años ----------------------------------------------------




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









