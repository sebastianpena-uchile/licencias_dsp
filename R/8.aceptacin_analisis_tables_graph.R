
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
if(!require(scales)){install.packages("scales")}

##### Cargar paquetes (OTRA ALTERNATIVA)
try(pacman::p_load(tidyverse,   # Probablemente el paquete conjunto de paquetes más últil que usarán en R
                   foreign,         # Paquete import datos
                   Hmisc,           # Paquete con funciones variadas
                   psych,
                   epitools,
                   gt,
                   des,
                   webshot2,
                   readr,
                   scales,# Paquete con algunas funciones comúnmente utilizadas (https://personality-project.org/r/psych/intro.pdf)
                   install = F))    # solo cargar, no instalar




# 2. cargar datos ---------------------------------------------------------

LM_ACEPTACION <- read_delim("data/clean/LM_2014_2022_1.ACEPTACION.csv", 
                                        delim = ";", escape_double = FALSE, trim_ws = TRUE)



# 3. clean data -----------------------------------------------------------


"change #N/D por ceros"

LM_ACEPTACION_CLEAN <- LM_ACEPTACION %>%
  mutate(across(everything(), ~ ifelse(. == "#N/D", 0, .)))


# 4. Tablas ---------------------------------------------------------------


#LM aceptadas FONASA, ISAPRE y TOTAL


LM_ACEPTACION_CLEAN %>%                                 # usa el df limpio (no lo modifica si no reasignas)
  slice(c(1, 2)) %>%                                    # toma solo las filas 1 y 2 (por posición)
  select(grupo = 1, stub = Seguro, `2015`:`2022`) %>%    # grupo: 1ª col (sección); stub: nombre de fila desde "Seguro"; años
  mutate(
    stub = toupper(stub),                               # 🔹 nombres de fila en MAYÚSCULAS
    across(`2015`:`2022`, as.numeric)                   # asegura que los años sean numéricos
  ) %>%
  mutate(                                                # 🔹 agrega fila TOTAL (y así queda formateada igual que el cuerpo)
    stub = as.character(stub),
    `2015` = as.numeric(`2015`), `2016` = as.numeric(`2016`), `2017` = as.numeric(`2017`), `2018` = as.numeric(`2018`),
    `2019` = as.numeric(`2019`), `2020` = as.numeric(`2020`), `2021` = as.numeric(`2021`), `2022` = as.numeric(`2022`)
  ) %>%
  bind_rows(
    summarise(.,
              grupo = first(grupo),                              # mantiene el mismo grupo
              stub  = "Total",                                   # nombre de la última fila
              across(`2015`:`2022`, ~ sum(.x, na.rm = TRUE))     # suma por columna-año
    )
  ) %>%
  mutate(                                                # 🔹 formatea números con . miles y , decimales (aplica a TODO, incluido Total)
    across(`2015`:`2022`, ~ scales::number(.x, accuracy = 1, big.mark = ".", decimal.mark = ","))
  ) %>%
  gt(
    rowname_col = "stub",                                # usa "stub" como nombre visible de las filas
    groupname_col = "grupo"                              # crea sección (row group) con el texto de "grupo"
  ) %>%
  tab_header(                                            # 🔹 título y subtítulo de la tabla
    title = "Tabla 1: Número de Licencias Médicas años 2015-2022",
    subtitle = "SUSESO, 2026"
  ) %>%
  tab_row_group(                                         # cambia SOLO el título visible de la sección
    label = "Licencias médicas aceptadas según seguro de salud",
    rows = TRUE
  ) %>%
  tab_style(                                             # 🔹 negrita encabezados de columnas (años)
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = `2015`:`2022`)
  ) %>%
  tab_style(                                             # 🔹 negrita nombres de filas (stub)
    style = cell_text(weight = "bold"),
    locations = cells_stub()
  ) %>%
  tab_style(                                             # 🔹 negrita título de la sección
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  tab_style(                                             # 🔹 negrita la última fila ("Total") completa
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = stub == "Total")
  )
