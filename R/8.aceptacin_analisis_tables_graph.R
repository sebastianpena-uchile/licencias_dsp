
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
if(!require(gtExtras)){install.packages("gtExtras")}

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
                   scales,
                   gtExtras,# Paquete con algunas funciones comúnmente utilizadas (https://personality-project.org/r/psych/intro.pdf)
                   install = F))    # solo cargar, no instalar




# 2. cargar datos ---------------------------------------------------------

LM_ACEPTACION <- read_delim("data/clean/LM_2014_2022_1.ACEPTACION.csv", 
                                        delim = ";", escape_double = FALSE, trim_ws = TRUE)



# 3. clean data -----------------------------------------------------------


"change #N/D por ceros"

LM_ACEPTACION_CLEAN <- LM_ACEPTACION %>%
  mutate(across(everything(), ~ ifelse(. == "#N/D", 0, .)))


# 4. Tablas ---------------------------------------------------------------


# #LM aceptadas FONASA, ISAPRE y TOTAL
# 
# 
# ##Bloque 1
# 
# tabla1_seguro <- LM_ACEPTACION_CLEAN %>%                                 # usa el df limpio (no lo modifica si no reasignas)
#   slice(c(1, 2)) %>%                                    # toma solo las filas 1 y 2 (por posición)
#   select(grupo = 1, stub = Seguro, `2015`:`2022`) %>%    # grupo: 1ª col (sección); stub: nombre de fila desde "Seguro"; años
#   mutate(
#     stub = toupper(stub),                               # 🔹 nombres de fila en MAYÚSCULAS
#     across(`2015`:`2022`, as.numeric)                   # asegura que los años sean numéricos
#   ) %>%
#   mutate(                                                # 🔹 agrega fila TOTAL (y así queda formateada igual que el cuerpo)
#     stub = as.character(stub),
#     `2015` = as.numeric(`2015`), `2016` = as.numeric(`2016`), `2017` = as.numeric(`2017`), `2018` = as.numeric(`2018`),
#     `2019` = as.numeric(`2019`), `2020` = as.numeric(`2020`), `2021` = as.numeric(`2021`), `2022` = as.numeric(`2022`)
#   ) %>%
#   bind_rows(
#     summarise(.,
#               grupo = first(grupo),                              # mantiene el mismo grupo
#               stub  = "Total",                                   # nombre de la última fila
#               across(`2015`:`2022`, ~ sum(.x, na.rm = TRUE))     # suma por columna-año
#     )
#   ) %>%
#   mutate(                                                # 🔹 formatea números con . miles y , decimales (aplica a TODO, incluido Total)
#     across(`2015`:`2022`, ~ scales::number(.x, accuracy = 1, big.mark = ".", decimal.mark = ","))
#   ) %>%
#   gt(
#     rowname_col = "stub",                                # usa "stub" como nombre visible de las filas
#     groupname_col = "grupo"                              # crea sección (row group) con el texto de "grupo"
#   ) %>%
#   tab_header(                                            # 🔹 título y subtítulo de la tabla
#     title = "Tabla 1: Número de Licencias Médicas Aceptadas años 2015-2022",
#     subtitle = "SUSESO, 2026"
#   ) %>%
#   tab_row_group(                                         # cambia SOLO el título visible de la sección
#     label = "Licencias médicas aceptadas según seguro de salud",
#     rows = TRUE
#   ) %>%
#   tab_style(                                             # 🔹 negrita encabezados de columnas (años)
#     style = cell_text(weight = "bold"),
#     locations = cells_column_labels(columns = `2015`:`2022`)
#   ) %>%
#   tab_style(                                             # 🔹 negrita nombres de filas (stub)
#     style = cell_text(weight = "bold"),
#     locations = cells_stub()
#   ) %>%
#   tab_style(                                             # 🔹 negrita título de la sección
#     style = cell_text(weight = "bold"),
#     locations = cells_row_groups()
#   ) %>%
#   tab_style(                                             # 🔹 negrita la última fila ("Total") completa
#     style = cell_text(weight = "bold"),
#     locations = cells_body(rows = stub == "Total")
#   )
# 
# 
# ##BLoque 2
# 
# tabla2_seguro_sexo <- LM_ACEPTACION_CLEAN %>%
#   slice(3:8) %>%                                   # selecciona solo las filas relevantes (sexo)
#   
#   # selecciona columnas y renombra para gt
#   select(grupo = Sexo,                              # grupo será Sexo (row group)
#          stub  = Seguro,                            # stub será Seguro (filas)
#          `2015`:`2022`) %>%                         # columnas de años
#   
#   mutate(
#     grupo = as.character(grupo),                    # asegura tipo carácter
#     grupo = dplyr::case_when(                       # normaliza nombres de Sexo
#       grupo == "hombre"          ~ "Hombre",
#       grupo == "mujer"           ~ "Mujer",
#       grupo == "sin_informacion" ~ "Sin Información",
#       TRUE ~ grupo
#     ),
#     stub  = toupper(as.character(stub)),            # Seguro en mayúsculas
#     across(`2015`:`2022`, as.numeric)               # asegura valores numéricos
#   ) %>%
#   
#   {                                                 # bloque auxiliar sin romper el pipe
#     base <- .                                       # guarda la base original (detalle)
#     
#     sub <- base %>%                                 # calcula subtotales por Sexo
#       group_by(grupo) %>%
#       summarise(
#         stub = "Subtotal",                          # nombre de la fila subtotal
#         across(`2015`:`2022`, ~ sum(.x, na.rm = TRUE)),
#         .groups = "drop"
#       )
#     
#     tot <- base %>%                                 # calcula total global (solo desde base)
#       summarise(
#         grupo = "Totales",                          # nombre del bloque final
#         stub  = "Total",                            # nombre de la fila total
#         across(`2015`:`2022`, ~ sum(.x, na.rm = TRUE))
#       )
#     
#     bind_rows(base, sub, tot)                       # une detalle + subtotales + total
#   } %>%
#   
#   mutate(
#     orden_grupo = ifelse(                           # ordena grupos: Totales al final
#       grupo == "Totales", 9999,
#       as.integer(factor(grupo,
#                         levels = unique(grupo[grupo != "Totales"])))
#     ),
#     orden_stub = case_when(                         # ordena filas dentro del grupo
#       stub == "Subtotal" ~ 9998,                    # subtotal al final del grupo
#       stub == "Total"    ~ 9999,                    # total al final absoluto
#       TRUE ~ as.integer(
#         factor(stub,
#                levels = unique(stub[!stub %in% c("Subtotal","Total")]))
#       )
#     )
#   ) %>%
#   arrange(orden_grupo, grupo, orden_stub) %>%       # aplica el orden definido
#   select(-orden_grupo, -orden_stub) %>%             # elimina columnas auxiliares
#   
#   mutate(
#     across(`2015`:`2022`,                           # formatea números
#            ~ scales::number(.x,
#                             accuracy = 1,
#                             big.mark = ".",
#                             decimal.mark = ","))
#   ) %>%
#   
#   gt(
#     rowname_col  = "stub",                           # stub como nombre de filas
#     groupname_col = "grupo"                          # grupo como bloques (Sexo)
#   ) %>%
#   
#   cols_label(                                       # elimina encabezados visibles (X)
#     `2015` = "", `2016` = "", `2017` = "", `2018` = "",
#     `2019` = "", `2020` = "", `2021` = "", `2022` = ""
#   ) %>%
#   
#   tab_style(                                        # negrita en nombres de filas
#     style = cell_text(weight = "bold"),
#     locations = cells_stub()
#   ) %>%
#   tab_style(                                        # negrita en títulos de grupo (Sexo)
#     style = cell_text(weight = "bold"),
#     locations = cells_row_groups()
#   ) %>%
#   tab_style(                                        # negrita en Subtotal y Total
#     style = cell_text(weight = "bold"),
#     locations = cells_body(rows = stub %in% c("Subtotal", "Total"))
#   )
# 
# #Union de las tablas
# 
# gt_merge_stack(
#   tabla1_seguro,
#   tabla2_seguro_sexo
# )


# ------------------------------------------------------
# Títulos internos
# ------------------------------------------------------
titulo_bloque1 <- "Licencias médicas autorizadas según seguro de salud"
titulo_bloque2 <- "Licencias médicas autorizadas según sexo y seguro de salud"

# ======================================================
# BLOQUE 1 – FONASA / ISAPRE / TOTAL  (arriba)
# ======================================================
bloque1 <- LM_ACEPTACION_CLEAN %>%
  slice(1:2) %>%
  select(stub = Seguro, `2015`:`2022`) %>%
  mutate(
    grupo = titulo_bloque1,
    stub  = toupper(as.character(stub)),
    across(`2015`:`2022`, as.numeric)
  ) %>%
  bind_rows(
    summarise(.,
              grupo = first(grupo),
              stub  = "Total",
              across(`2015`:`2022`, ~ sum(.x, na.rm = TRUE))
    )
  )

# ======================================================
# BLOQUE 2 – SEXO → SEGURO + SUBTOTALES + TOTAL GLOBAL (abajo)
# ======================================================
bloque2_detalle <- LM_ACEPTACION_CLEAN %>%
  slice(3:8) %>%
  select(grupo = Sexo, stub = Seguro, `2015`:`2022`) %>%
  mutate(
    grupo = as.character(grupo),
    grupo = case_when(
      grupo == "hombre"          ~ "Hombre",
      grupo == "mujer"           ~ "Mujer",
      grupo == "sin_informacion" ~ "Sin Información",
      TRUE ~ grupo
    ),
    stub = toupper(as.character(stub)),
    across(`2015`:`2022`, as.numeric)
  )

bloque2_subtotales <- bloque2_detalle %>%
  group_by(grupo) %>%
  summarise(
    stub = "Subtotal",
    across(`2015`:`2022`, ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  )

bloque2_totales <- bloque2_detalle %>%
  summarise(
    grupo = "Totales",
    stub  = "Total",
    across(`2015`:`2022`, ~ sum(.x, na.rm = TRUE))
  )

# ------------------------------------------------------
# FILA “ANCLA” para que exista el TÍTULO INTERIOR del BLOQUE 2
# (gt exige al menos 1 fila para un row group)
# ------------------------------------------------------
fila_titulo_bloque2 <- tibble(
  grupo = titulo_bloque2,
  stub  = "\u200B",               # carácter invisible (no deja texto)
  `2015` = NA_real_, `2016` = NA_real_, `2017` = NA_real_, `2018` = NA_real_,
  `2019` = NA_real_, `2020` = NA_real_, `2021` = NA_real_, `2022` = NA_real_
)

# ======================================================
# UNIÓN FINAL + ORDEN (no cambia la lógica)
# ======================================================
tabla_final <- bind_rows(
  bloque1,
  fila_titulo_bloque2,   # <- aquí aparece el título interior del bloque 2
  bloque2_detalle,
  bloque2_subtotales,
  bloque2_totales
) %>%
  mutate(
    grupo = factor(
      as.character(grupo),
      levels = c(
        titulo_bloque1,
        titulo_bloque2,
        "Hombre", "Mujer", "Sin Información",
        "Totales"
      )
    ),
    orden_stub = case_when(
      stub == "Subtotal" ~ 9998,
      stub == "Total"    ~ 9999,
      TRUE ~ 1
    )
  ) %>%
  arrange(grupo, orden_stub, stub) %>%
  select(-orden_stub)

# ======================================================
# SALIDA GT (una sola vez) — re-agrego título/subtítulo
# ======================================================
tabla_final %>%
  gt(rowname_col = "stub", groupname_col = "grupo") %>%
  tab_header(
    title = "Número de Licencias Médicas Aceptadas 2015–2022",
    subtitle = "SUSESO, 2026"
  ) %>%
  sub_missing(columns = `2015`:`2022`, missing_text = "") %>%
  fmt_number(columns = `2015`:`2022`, decimals = 0, sep_mark = ".", dec_mark = ",") %>%
  
  # --- hace la fila “ancla” lo más invisible posible (sin tocar nada más) ---
  tab_style(
    style = list(
      cell_text(color = "transparent", size = "0px"),
      cell_borders(sides = "all", weight = px(0), color = "transparent")
    ),
    locations = list(
      cells_stub(rows = grupo == titulo_bloque2),
      cells_body(rows = grupo == titulo_bloque2)
    )
  ) %>%
  
  # --- estilos que ya venías usando ---
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = `2015`:`2022`)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_stub()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = stub %in% c("Subtotal", "Total"))
  )
