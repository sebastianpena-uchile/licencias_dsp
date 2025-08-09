
# 1. cargar librerias -----------------------------------------------------



library(dplyr)
library(purrr)
library(stringr)
library(tibble)




# 2. cargar datos ---------------------------------------------------------




load("data/clean/rechazo_db_clean.Rdata")




# 3. df totales -----------------------------------------------------------


# Vector con las familias que quieres conservar
familias_filtrar <- c(
  "Ciertas enfermedades infecciosas y parasitarias",
  "Trastornos mentales y del comportamiento",
  "Enfermedades del sistema respiratorio",
  "Enfermedades del sistema digestivo",
  "Enfermedades del sistema osteomuscular y del tejido conectivo",
  "Traumatismos, envenenamientos y algunas otras consecuencias de causa externa",
  "Otras familias de diagnósticos"
)

combinar_filtrado <- function(years, familias) {
  objs <- ls(
    pattern = paste0("(", paste(years, collapse = "|"), ")$"),
    envir = .GlobalEnv
  )
  if (length(objs) == 0) return(tibble())
  
  dfs <- mget(objs, envir = .GlobalEnv)
  
  imap_dfr(
    dfs,
    ~ .x %>%
      filter(`Familia de diagnóstico (CIE-10)` %in% familias) %>%     # Filtra solo las familias deseadas
      mutate(.origen = .y,
             anho_origen = as.integer(str_extract(.y, "\\d{4}$")))
  )
}

# Ejecutar para 2016:2023
rechazo_2016_2023 <- combinar_filtrado(2016:2023, familias_filtrar)


#eliminar columnas extras

rechazo_2016_2023 <- rechazo_2016_2023 %>%
  select(-`Familia de diagnóstico (CIE-10)`, -.origen, -anho_origen)

#cambiar nombre variable 2013-2015

rechazo_2013 <- rechazo_2013 %>%
  rename(cei10_familia = ...1)
rechazo_2014 <- rechazo_2014 %>%
  rename(cei10_familia = ...1)
rechazo_2015 <- rechazo_2015 %>%
  rename(cei10_familia = ...1)

#unir dfs

rechazo_total_2013_2023 <- bind_rows(rechazo_2013, 
                                     rechazo_2014,
                                     rechazo_2015,
                                     rechazo_2016_2023)






# 4. Desagregado por sexo -------------------------------------------------






# Vector con las familias que quieres conservar
sexo <- c(
  "Mujeres",
  "Hombres"
)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)

combinar_filtrado_sexo <- function(years, sexo) {
  # Solo objetos que terminen en "_YYYY"
  patron <- paste0("_(?:", paste(years, collapse = "|"), ")$")
  objs <- ls(envir = .GlobalEnv)
  objs <- objs[str_detect(objs, patron)]
  
  if (length(objs) == 0) return(tibble())
  
  dfs <- mget(objs, envir = .GlobalEnv)
  
  imap_dfr(
    dfs,
    ~{
      col_objetivo <- "Familia de diagnóstico (CIE-10)"
      
      if (!col_objetivo %in% names(.x)) {
        warning(glue::glue("Aviso: '{.y}' no tiene la columna '{col_objetivo}'. Se omite."))
        return(tibble())
      }
      
      .x %>%
        filter(.data[[col_objetivo]] %in% sexo) %>%
        mutate(
          .origen = .y,
          anho_origen = as.integer(str_extract(.y, "\\d{4}$"))
        )
    }
  )
}


rechazo_2016_2023_por_sexo <- combinar_filtrado_sexo(2016:2023, sexo)




# 5. Grabar objetos -------------------------------------------------------




save(rechazo_2016_2023, 
     rechazo_2016_2023_por_sexo,
     file = "data/clean/rechazo_db_totales.Rdata")





# 6. limpiar todo ---------------------------------------------------------







rm(list = ls())
gc()




