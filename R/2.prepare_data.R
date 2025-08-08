
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




