
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
if(!require(tibble)){install.packages("tibble")}
if(!require(officer)){install.packages("officer")}
if(!require(gto)){install.packages("gto")}
if(!require(tinytex)){install.packages("tinytex")}


##### Cargar paquetes (OTRA ALTERNATIVA)
try(pacman::p_load(tidyverse,   # Probablemente el paquete conjunto de paquetes más últil que usarán en R
                   foreign,         # Paquete import datos
                   Hmisc,           # Paquete con funciones variadas
                   psych,
                   epitools,
                   gt,
                   webshot2,
                   readr,
                   scales,
                   gtExtras,
                   tibble,
                   officer,
                   gto,
                   tinytex,# Paquete con algunas funciones comúnmente utilizadas (https://personality-project.org/r/psych/intro.pdf)
                   install = F))    # solo cargar, no instalar




# 2. cargar datos ---------------------------------------------------------

LM_ACEPTACION <- read_delim("data/clean/LM_2014_2022_1.ACEPTACION.csv", 
                                        delim = ";", escape_double = FALSE, trim_ws = TRUE)



# 3. clean data -----------------------------------------------------------


"change #N/D por ceros"

LM_ACEPTACION_CLEAN <- LM_ACEPTACION %>%
  mutate(across(everything(), ~ ifelse(. == "#N/D", 0, .)))


# 4. Tablas ---------------------------------------------------------------

# ======================================================
# 0) LIBRERÍAS
# ======================================================

library(dplyr)   # Carga dplyr para manipulación de datos (pipes, mutate, summarise, etc.)
library(tibble)  # Carga tibble para crear tibbles fácilmente
library(gt)      # Carga gt para construir tablas formateadas

# ======================================================
# 1) TÍTULOS INTERNOS (ROW GROUPS “CABECERA” DE CADA BLOQUE)
# ======================================================

titulo_bloque1 <- "Licencias médicas autorizadas según seguro de salud"                     # Título del bloque 1
titulo_bloque2 <- "Licencias médicas autorizadas según sexo y seguro de salud"              # Título del bloque 2
titulo_bloque3 <- "Licencias médicas autorizadas según rango de edad y seguro de salud"     # Título del bloque 3
titulo_bloque4 <- "Licencias médicas autorizadas según region y seguro de salud (ambos sexos)" # Título del bloque 4

# ======================================================
# 2) ETIQUETAS “TOTALES” ÚNICAS (EVITA QUE gt FUSIONE GRUPOS)
#    (VISUALMENTE SE VE IGUAL, INTERNAMENTE NO)
# ======================================================

totales_b2 <- "Totales"                      # Texto para totales del bloque 2
totales_b3 <- paste0("Totales", "\u200B")    # Texto para totales del bloque 3 (mismo look, distinto internamente)
totales_b4 <- paste0("Totales", "\u200B", "\u200B") # Texto para totales del bloque 4 (mismo look, distinto internamente)

# ======================================================
# 3) BLOQUE 1 (FILAS 1:2) + TOTAL DEL BLOQUE 1
# ======================================================

bloque1 <- LM_ACEPTACION_CLEAN %>%                                       # Parte desde tu tabla base
  slice(1:2) %>%                                                         # Toma SOLO filas 1 y 2
  select(stub = Seguro, `2015`:`2022`) %>%                               # Renombra Seguro->stub y toma años 2015–2022
  mutate(
    bloque_id = 1,                                                       # Identificador del bloque (para ordenar luego)
    grupo     = titulo_bloque1,                                          # Row group (título interno del bloque 1)
    grupo_tipo  = 1,                                                     # Tipo de grupo: 0=título, 1=normal, 2=totales
    grupo_orden = 1,                                                     # Orden del grupo dentro del bloque
    stub      = toupper(as.character(stub)),                             # Pasa stub a mayúsculas (FONASA/ISAPRE)
    across(`2015`:`2022`, as.numeric)                                    # Convierte años a numérico
  ) %>%
  bind_rows(                                                             # Agrega una fila extra al final
    summarise(.,                                                         # Resume el bloque para crear el total
              bloque_id   = first(bloque_id),                                    # Mantiene el id del bloque
              grupo       = first(grupo),                                        # Mantiene el row group del bloque 1
              grupo_tipo  = 2,                                                   # Marca esta fila como “totales”
              grupo_orden = 9999,                                                # Hace que el total quede al final
              stub        = "Total",                                             # Etiqueta de la fila total
              across(`2015`:`2022`, ~ sum(.x, na.rm = TRUE))                     # Suma por año ignorando NA
    )
  )

# ======================================================
# 4) BLOQUE 2 (FILAS 3:8) SEXO -> SEGURO + SUBTOTALES + TOTAL GLOBAL
# ======================================================

bloque2_detalle <- LM_ACEPTACION_CLEAN %>%                               # Parte desde la tabla base
  slice(3:8) %>%                                                         # Toma SOLO filas 3 a 8
  select(grupo = Sexo, stub = Seguro, `2015`:`2022`) %>%                 # grupo=Sexo, stub=Seguro, y años
  mutate(
    bloque_id = 2,                                                       # Identificador bloque 2
    grupo = case_when(                                                   # Normaliza etiquetas de Sexo
      as.character(grupo) == "hombre"          ~ "Hombre",
      as.character(grupo) == "mujer"           ~ "Mujer",
      as.character(grupo) == "sin_informacion" ~ "Sin Información",
      TRUE ~ as.character(grupo)
    ),
    grupo_tipo  = 1,                                                     # Grupo normal (no título, no totales)
    grupo_orden = 1,                                                     # Orden base del grupo (luego ordena por texto)
    stub = toupper(as.character(stub)),                                  # Seguro en mayúsculas
    across(`2015`:`2022`, as.numeric)                                    # Años numéricos
  )

bloque2_subtotales <- bloque2_detalle %>%                                # Usa el detalle del bloque 2
  group_by(bloque_id, grupo) %>%                                         # Agrupa por bloque 2 y por sexo
  summarise(
    grupo_tipo  = 1,                                                     # Sigue siendo grupo normal
    grupo_orden = 1,                                                     # Orden normal
    stub = "Subtotal",                                                   # Etiqueta de subtotal
    across(`2015`:`2022`, ~ sum(.x, na.rm = TRUE)),                      # Suma por año dentro de cada sexo
    .groups = "drop"                                                     # Quita agrupación
  )

# Total global SOLO UNA VEZ para todo el bloque 2
bloque2_total_global <- bloque2_detalle %>%                              # Usa el detalle del bloque 2
  summarise(
    bloque_id   = 2,                                                     # Bloque 2
    grupo       = totales_b2,                                            # Grupo “Totales” (único)
    grupo_tipo  = 2,                                                     # Marca como totales
    grupo_orden = 9999,                                                  # Orden al final del bloque
    stub        = "Total",                                               # Etiqueta fila total
    across(`2015`:`2022`, ~ sum(.x, na.rm = TRUE))                       # Suma global del bloque 2
  )

fila_titulo_bloque2 <- tibble(                                           # Crea fila ancla para mostrar el título interno
  bloque_id   = 2,                                                       # Bloque 2
  grupo       = titulo_bloque2,                                          # Grupo = título interno del bloque 2
  grupo_tipo  = 0,                                                       # Tipo 0 = título
  grupo_orden = 0,                                                       # Orden primero dentro del bloque
  stub        = "\u200B",                                                # Texto invisible (para no “ensuciar”)
  `2015` = NA_real_, `2016` = NA_real_, `2017` = NA_real_, `2018` = NA_real_,
  `2019` = NA_real_, `2020` = NA_real_, `2021` = NA_real_, `2022` = NA_real_
)

# ======================================================
# 5) BLOQUE 3 (FILAS 9:24) EDAD -> (ROW_GROUP POR SEGURO) + SUBTOTALES + TOTAL GLOBAL
#    - AQUÍ SÍ QUEDA SUBDIVIDIDO POR SEGURO PORQUE grupo = Seguro (NO factor)
# ======================================================

bloque3_detalle <- LM_ACEPTACION_CLEAN %>%                               # Parte desde la tabla base
  slice(9:24) %>%                                                        # Toma SOLO filas 9 a 24 (como indicaste)
  select(grupo = Seguro, stub = Edad, `2015`:`2022`) %>%                 # grupo=Seguro (row_group), stub=Edad
  mutate(
    bloque_id = 3,                                                       # Identificador bloque 3
    grupo = toupper(as.character(grupo)),                                # Seguro en mayúsculas => row_group por Seguro
    grupo_tipo  = 1,                                                     # Grupo normal
    grupo_orden = 1,                                                     # Orden normal
    stub  = as.character(stub),                                          # Edad como texto (rangos)
    across(`2015`:`2022`, as.numeric)                                    # Años numéricos
  )

bloque3_subtotales <- bloque3_detalle %>%                                # Usa el detalle del bloque 3
  group_by(bloque_id, grupo) %>%                                         # Agrupa por bloque 3 y por Seguro
  summarise(
    grupo_tipo  = 1,                                                     # Grupo normal
    grupo_orden = 1,                                                     # Orden normal
    stub = "Subtotal",                                                   # Etiqueta subtotal
    across(`2015`:`2022`, ~ sum(.x, na.rm = TRUE)),                      # Suma por año dentro de cada Seguro
    .groups = "drop"                                                     # Quita agrupación
  )

# Total global SOLO UNA VEZ para todo el bloque 3
bloque3_total_global <- bloque3_detalle %>%                              # Usa el detalle del bloque 3
  summarise(
    bloque_id   = 3,                                                     # Bloque 3
    grupo       = totales_b3,                                            # Grupo “Totales” único del bloque 3
    grupo_tipo  = 2,                                                     # Tipo totales
    grupo_orden = 9999,                                                  # Orden al final del bloque
    stub        = "Total",                                               # Etiqueta fila total
    across(`2015`:`2022`, ~ sum(.x, na.rm = TRUE))                       # Suma global bloque 3
  )

fila_titulo_bloque3 <- tibble(                                           # Fila ancla para mostrar título del bloque 3
  bloque_id   = 3,                                                       # Bloque 3
  grupo       = titulo_bloque3,                                          # Grupo = título interno bloque 3
  grupo_tipo  = 0,                                                       # Tipo 0 = título
  grupo_orden = 0,                                                       # Primero dentro del bloque
  stub        = "\u200B",                                                # Texto invisible
  `2015` = NA_real_, `2016` = NA_real_, `2017` = NA_real_, `2018` = NA_real_,
  `2019` = NA_real_, `2020` = NA_real_, `2021` = NA_real_, `2022` = NA_real_
)

# ======================================================
# 6) BLOQUE 4 (FILAS 25:58) REGION -> (ROW_GROUP POR SEGURO) + SUBTOTALES + TOTAL GLOBAL
#    - MISMA LÓGICA QUE BLOQUE 3, PERO stub = Region
# ======================================================

bloque4_detalle <- LM_ACEPTACION_CLEAN %>%                               # Parte desde la tabla base
  slice(25:58) %>%                                                       # Toma SOLO filas 25 a 58 (como indicaste)
  select(grupo = Seguro, stub = Region, `2015`:`2022`) %>%               # grupo=Seguro (row_group), stub=Region
  mutate(
    bloque_id = 4,                                                       # Identificador bloque 4
    grupo = paste0(toupper(as.character(grupo)), "\u200B"),              # Seguro en mayúsculas => row_group por Seguro (separado del bloque 3)
    grupo_tipo  = 1,                                                     # Grupo normal
    grupo_orden = 1,                                                     # Orden normal
    stub  = as.character(stub),                                          # Región como texto
    across(`2015`:`2022`, as.numeric)                                    # Años numéricos
  ) %>%
  group_by(grupo) %>%                                                    # Agrupa por Seguro (dentro del bloque 4)
  mutate(stub_orden = row_number()) %>%                                  # Guarda el orden original de Region para cada Seguro
  ungroup()                                                              # Quita el agrupamiento

bloque4_subtotales <- bloque4_detalle %>%                                # Usa el detalle del bloque 4
  group_by(bloque_id, grupo) %>%                                         # Agrupa por bloque 4 y por Seguro
  summarise(
    grupo_tipo  = 1,                                                     # Grupo normal
    grupo_orden = 1,                                                     # Orden normal
    stub = "Subtotal",                                                   # Etiqueta subtotal
    across(`2015`:`2022`, ~ sum(.x, na.rm = TRUE)),                      # Suma por año dentro de cada Seguro
    .groups = "drop"                                                     # Quita agrupación
  )

# Total global SOLO UNA VEZ para todo el bloque 4
bloque4_total_global <- bloque4_detalle %>%                              # Usa el detalle del bloque 4
  summarise(
    bloque_id   = 4,                                                     # Bloque 4
    grupo       = totales_b4,                                            # Grupo “Totales” único del bloque 4
    grupo_tipo  = 2,                                                     # Tipo totales
    grupo_orden = 9999,                                                  # Orden al final del bloque
    stub        = "Total",                                               # Etiqueta fila total
    across(`2015`:`2022`, ~ sum(.x, na.rm = TRUE))                       # Suma global bloque 4
  )

fila_titulo_bloque4 <- tibble(                                           # Fila ancla para mostrar título del bloque 4
  bloque_id   = 4,                                                       # Bloque 4
  grupo       = titulo_bloque4,                                          # Grupo = título interno bloque 4
  grupo_tipo  = 0,                                                       # Tipo 0 = título
  grupo_orden = 0,                                                       # Primero dentro del bloque
  stub        = "\u200B",                                                # Texto invisible
  `2015` = NA_real_, `2016` = NA_real_, `2017` = NA_real_, `2018` = NA_real_,
  `2019` = NA_real_, `2020` = NA_real_, `2021` = NA_real_, `2022` = NA_real_
)

# ======================================================
# 7) UNIÓN FINAL + ORDEN
#    - Ordena por bloque_id (1,2,3,4)
#    - Dentro del bloque: título (grupo_tipo=0) primero
#    - Luego grupos normales (grupo_tipo=1) (p.ej., Seguro)
#    - Totales (grupo_tipo=2) al final
#    - Subtotal/Total al final dentro de cada grupo
# ======================================================

tabla_final <- bind_rows(                                                # Une todo en una sola tabla
  bloque1,                                                               # Bloque 1 completo
  fila_titulo_bloque2, bloque2_detalle, bloque2_subtotales, bloque2_total_global,  # Bloque 2 completo
  fila_titulo_bloque3, bloque3_detalle, bloque3_subtotales, bloque3_total_global,  # Bloque 3 completo
  fila_titulo_bloque4, bloque4_detalle, bloque4_subtotales, bloque4_total_global   # Bloque 4 completo
) %>%
  mutate(
    orden_stub = case_when(                                              # Orden interno de filas dentro del grupo
      stub == "Subtotal" ~ 9998,                                         # Subtotal casi al final
      stub == "Total"    ~ 9999,                                         # Total al final
      TRUE ~ 1                                                          # El resto arriba
    ),
    stub_orden2 = if_else(is.na(stub_orden), 0L, stub_orden)              # Usa el orden original de Region (solo existe en bloque 4)
  ) %>%
  arrange(
    as.integer(bloque_id),                                               # Orden por bloque 1->2->3->4 (forzado)
    grupo_tipo,                                                          # Título (0) -> normal (1) -> totales (2)
    grupo_orden,                                                         # Orden auxiliar (0 para títulos, 9999 totales)
    grupo,                                                               # Orden alfabético de grupos normales (seguros/sexos)
    orden_stub,                                                          # Subtotal/Total al final
    stub_orden2,                                                         # Mantiene orden original (bloque 4) sin afectar el resto
    stub                                                                 # Orden alfabético del stub (edad/region/seguro)
  ) %>%
  select(-bloque_id, -grupo_tipo, -grupo_orden, -orden_stub, -stub_orden, -stub_orden2) # Elimina columnas auxiliares

# ======================================================
# 8) TABLA gt (FORMATO)
# ======================================================

tabla_final %>%                                                          # Usa la tabla final ordenada
  gt(rowname_col = "stub", groupname_col = "grupo") %>%                  # stub como rowname, grupo como row_group
  tab_header(
    title = "Número de Licencias Médicas Autorizadas 2015–2022",         # Título principal
    subtitle = "SUSESO, 2026"                                            # Subtítulo
  ) %>%
  sub_missing(columns = `2015`:`2022`, missing_text = "") %>%            # NA se imprime vacío
  fmt_number(
    columns = `2015`:`2022`,                                             # Formatea años
    decimals = 0,                                                        # Sin decimales
    sep_mark = ".",                                                      # Separador de miles
    dec_mark = ","                                                       # Separador decimal (no afecta con decimals=0)
  ) %>%
  tab_style(                                                             # Estilo para “ocultar” filas ancla (títulos internos)
    style = list(
      cell_text(color = "transparent", size = "0px"),                    # Texto invisible
      cell_borders(sides = "all", weight = px(0), color = "transparent") # Bordes invisibles
    ),
    locations = list(
      cells_stub(rows = grupo %in% c(titulo_bloque2, titulo_bloque3, titulo_bloque4)),   # Oculta stub de filas ancla
      cells_body(rows = grupo %in% c(titulo_bloque2, titulo_bloque3, titulo_bloque4))    # Oculta cuerpo de filas ancla
    )
  ) %>%
  tab_style(                                                             # Negrita en encabezados de columnas
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = `2015`:`2022`)
  ) %>%
  tab_style(                                                             # Negrita en la columna stub
    style = cell_text(weight = "bold"),
    locations = cells_stub()
  ) %>%
  tab_style(                                                             # Negrita en títulos de row groups
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  tab_style(                                                             # Estilo para diferenciar las filas de subtotales y totales
    style = list(
      cell_fill(color = "#F2F2F2"),                                      # Gris claro para subtotales
      cell_text(weight = "bold")                                         # Texto en negrita
    ),
    locations = cells_body(rows = stub == "Subtotal")                    # Aplica solo a filas Subtotal
  ) %>%
  tab_style(                                                             # Estilo para diferenciar las filas de subtotales y totales
    style = list(
      cell_fill(color = "#D9D9D9"),                                      # Gris más oscuro para totales
      cell_text(weight = "bold")                                         # Texto en negrita
    ),
    locations = cells_body(rows = stub == "Total")                       # Aplica solo a filas Total
  )


#Pasar a word


# #graficos ---------------------------------------------------------------

years <- as.character(2015:2022)

LM_ACEPTACION_CLEAN <- LM_ACEPTACION_CLEAN %>%
  mutate(across(all_of(years), ~ readr::parse_number(as.character(.x))))

# ======================================================
# GRAFICOS ggplot2 (4 bloques) - basado en tu subdivisión
# ======================================================

# ======================================================
# INSTALACION INTELIGENTE DE PAQUETES (SOLO SI FALTAN)
# ======================================================

pkgs_needed <- c(
  "dplyr",
  "tidyr",
  "ggplot2",
  "scales",
  "stringr",
  "forcats",
  "ggrepel",
  "readr"
)

pkgs_missing <- pkgs_needed[!pkgs_needed %in% installed.packages()[,"Package"]]

if (length(pkgs_missing) > 0) {
  install.packages(pkgs_missing, repos = "https://cloud.r-project.org")
}

# ======================================================
# CARGA DE LIBRERIAS
# ======================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(stringr)
library(forcats)
library(ggrepel)
library(readr)

# ======================================================
# 1) Titulos / subtitulos por bloque
# ======================================================

titulo_bloque1 <- "Licencias médicas autorizadas según seguro de salud"
titulo_bloque2 <- "Licencias médicas autorizadas según sexo y seguro de salud"
titulo_bloque3 <- "Licencias médicas autorizadas según rango de edad y seguro de salud"
titulo_bloque4 <- "Licencias médicas autorizadas según region y seguro de salud (ambos sexos)"

subtitulo_general <- "Serie anual 2015–2022 • Fuente: SUSESO"

# ======================================================
# 2) Estilo “presentación”
# ======================================================

theme_set(
  theme_minimal(base_size = 12) +
    theme(
      plot.title.position = "plot",
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 12, color = "grey30"),
      plot.caption = element_text(size = 9, color = "grey40"),
      legend.position = "top",
      legend.title = element_blank(),
      panel.grid.minor = element_blank()
    )
)

fmt_num <- label_number(big.mark = ".", decimal.mark = ",")
years <- as.character(2015:2022)

# ---- helper robusto
to_long <- function(df, id_cols) {
  df %>%
    mutate(across(all_of(years), ~ readr::parse_number(as.character(.x)))) %>%
    pivot_longer(cols = all_of(years), names_to = "anio", values_to = "n") %>%
    mutate(
      anio = as.integer(anio),
      n = as.numeric(n)
    )
}

# ======================================================
# BLOQUE 1: Seguro
# ======================================================

b1 <- LM_ACEPTACION_CLEAN %>%
  slice(1:2) %>%
  select(Seguro, `2015`:`2022`) %>%
  mutate(Seguro = str_to_upper(as.character(Seguro))) %>%
  to_long(id_cols = "Seguro")

p1 <- ggplot(b1, aes(anio, n, color = Seguro)) +
  geom_line(linewidth = 1.1, alpha = 0.95) +
  geom_point(size = 2, alpha = 0.9) +
  scale_x_continuous(breaks = 2015:2022) +
  scale_y_continuous(labels = fmt_num) +
  labs(
    title = titulo_bloque1,
    subtitle = subtitulo_general,
    x = NULL, y = "Número de licencias"
  )

# ======================================================
# BLOQUE 2: Sexo + Seguro
# ======================================================

b2 <- LM_ACEPTACION_CLEAN %>%
  slice(3:8) %>%
  select(Sexo, Seguro, `2015`:`2022`) %>%
  mutate(
    Sexo = case_when(
      as.character(Sexo) == "hombre"          ~ "Hombre",
      as.character(Sexo) == "mujer"           ~ "Mujer",
      as.character(Sexo) == "sin_informacion" ~ "Sin Información",
      TRUE ~ as.character(Sexo)
    ),
    Seguro = str_to_upper(as.character(Seguro))
  ) %>%
  to_long(id_cols = c("Sexo", "Seguro"))

p2 <- ggplot(b2, aes(anio, n, color = Seguro)) +
  geom_line(linewidth = 1.05, alpha = 0.95) +
  geom_point(size = 1.9, alpha = 0.9) +
  facet_wrap(~ Sexo, ncol = 2, scales = "free_y") +
  scale_x_continuous(breaks = 2015:2022) +
  scale_y_continuous(labels = fmt_num) +
  labs(
    title = titulo_bloque2,
    subtitle = paste0(subtitulo_general, " • Escala libre por panel"),
    x = NULL, y = "Número de licencias"
  )

# ======================================================
# BLOQUE 3: Edad + Seguro (porcentaje con etiquetas)
# ======================================================

b3 <- LM_ACEPTACION_CLEAN %>%
  slice(9:24) %>%
  select(Seguro, Edad, `2015`:`2022`) %>%
  mutate(
    Seguro = str_to_upper(as.character(Seguro)),
    Edad = as.character(Edad)
  ) %>%
  to_long(id_cols = c("Seguro", "Edad")) %>%
  group_by(Seguro, anio) %>%
  mutate(p = n / sum(n, na.rm = TRUE)) %>%
  ungroup()

anios_etiqueta <- 2015:2022
min_p_etiqueta <- 0.03

b3_lab <- b3 %>%
  filter(anio %in% anios_etiqueta, p >= min_p_etiqueta) %>%
  group_by(Seguro, anio) %>%
  mutate(ypos = cumsum(p) - p / 2) %>%
  ungroup() %>%
  mutate(lbl = scales::percent(p, accuracy = 1, decimal.mark = ","))

p3 <- ggplot(b3, aes(anio, p, fill = Edad)) +
  geom_area(alpha = 0.95, linewidth = 0.2) +
  facet_wrap(~ Seguro, ncol = 2) +
  scale_x_continuous(breaks = 2015:2022) +
  scale_y_continuous(labels = label_percent(accuracy = 1, decimal.mark = ",")) +
  labs(
    title = titulo_bloque3,
    subtitle = paste0(subtitulo_general, " • Participación dentro de cada seguro"),
    x = NULL, y = "Participación (%)"
  ) +
  ggrepel::geom_text_repel(
    data = b3_lab,
    aes(anio, ypos, label = lbl),
    inherit.aes = FALSE,
    size = 2.8,
    direction = "y",
    segment.size = 0.15,
    min.segment.length = 0,
    box.padding = 0.15,
    max.overlaps = 50,
    seed = 123
  )

# ======================================================
# BLOQUE 4: Region + Seguro (UNICO CAMBIO: ORDEN NORTE -> SUR ARRIBA -> ABAJO)
# ======================================================

b4_wide <- LM_ACEPTACION_CLEAN %>%
  slice(25:58) %>%
  select(Seguro, Region, `2015`:`2022`) %>%
  mutate(
    Seguro = str_to_upper(as.character(Seguro)),
    Region = str_replace_all(as.character(Region), "\\s+", " ")
  )

region_levels_ns <- c(
  "Arica y Parinacota",
  "Tarapacá",
  "Antofagasta",
  "Atacama",
  "Coquimbo",
  "Valparaíso",
  "Metropolitana de Santiago",
  "O'Higgins",
  "Maule",
  "Ñuble",
  "Biobío",
  "La Araucanía",
  "Los Ríos",
  "Los Lagos",
  "Aysén",
  "Magallanes"
)

region_in_data <- unique(b4_wide$Region)

region_levels_final <- c(
  intersect(region_levels_ns, region_in_data),
  setdiff(region_in_data, region_levels_ns)
)

b4 <- b4_wide %>%
  to_long(id_cols = c("Seguro", "Region")) %>%
  mutate(Region = factor(Region, levels = region_levels_final))

p4 <- ggplot(b4, aes(x = anio, y = Region, fill = n)) +
  geom_tile() +
  facet_wrap(~ Seguro, ncol = 2, scales = "free_y") +
  scale_x_continuous(breaks = 2015:2022) +
  scale_y_discrete(limits = rev(levels(b4$Region))) +   # <<< CAMBIO CLAVE
  scale_fill_gradientn(
    colours = c("darkgreen", "gold", "red3"),
    values = scales::rescale(c(0, 0.6, 1)),
    labels = fmt_num,
    guide = guide_colorbar(
      barwidth = grid::unit(28, "lines"),
      barheight = grid::unit(0.9, "cm"),
      ticks = TRUE
    )
  ) +
  labs(
    title = titulo_bloque4,
    subtitle = subtitulo_general,
    x = NULL, y = NULL, fill = "N"
  ) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 9)
  )

# ======================================================
# MOSTRAR
# ======================================================

p1
p2
p3
p4

# ======================================================
# EXPORTAR (NO CAMBIAR NOMBRES)
# ======================================================

ggsave("01_bloque1_seguro.png", p1, width = 10, height = 5, dpi = 320)
ggsave("02_bloque2_sexo_seguro.png", p2, width = 10, height = 5.5, dpi = 320)
ggsave("03_bloque3_edad_seguro_pct.png", p3, width = 10, height = 6.2, dpi = 320)
ggsave("04_bloque4_region_seguro_top.png", p4, width = 10, height = 7, dpi = 320)
