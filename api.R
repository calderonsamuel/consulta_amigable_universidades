library(tidyverse)
library(httr)
library(rvest)
library(janitor)

set_url_query2 <- function(year, ...) {

  base_url <- parse_url("https://apps5.mineco.gob.pe/transparencia/mensual/Navegar_6.aspx")
  base_url$query <- list(
    `_tgt` = "xls",
    `_uhc` = "yes",
    cpage = 1L,
    psize = 400L,
    y = year,
    `0` = NULL,
    `1` = "E",
    `2` = 10L,
    ...)

  build_url(base_url)
}


get_dl_data <- function(get_data, id) {
  get_data %>%
    read_html() %>%
    html_element("body") %>%
    html_element(id) %>%
    html_table()
}

api_ue_data <- function(year, ...) {
  new_url <- set_url_query2(year, ...)

  dl_data <- GET(new_url, timeout(20))

  data_names <- get_dl_data(dl_data, ".MapTable") %>%
    row_to_names(2) %>%
    names()

  get_dl_data(dl_data, ".Data") %>%
    set_names(data_names) %>%
    mutate(año = year)
}

# Unidades ejecutoras

ue_data <- list(560L, 561L, 562L) %>%
  map_dfr(~api_ue_data(year = 2019L, `3` = .x, `4` = 1693L)) %>%
  mutate(cod_ue = str_replace(`Unidad Ejecutora`, ".*-(.{4}).*", "\\1")) %>%
  mutate(across(.fns = as.character))

ue_data_2020 <- list(560L, 561L, 562L) %>%
  map_dfr(~api_ue_data(year = 2020L, `3` = .x, `4` = 1693L)) %>%
  mutate(cod_ue = str_replace(`Unidad Ejecutora`, ".*-(.{4}).*", "\\1")) %>%
  mutate(across(.fns = as.character))

ue_data_2021 <- list(560L, 561L, 562L) %>%
  map_dfr(~api_ue_data(year = 2021L, `3` = .x, `4` = 1693L)) %>%
  mutate(cod_ue = str_replace(`Unidad Ejecutora`, ".*-(.{4}).*", "\\1")) %>%
  mutate(across(.fns = as.character))

unidad_ejecutora <-bind_rows(ue_data, ue_data_2020, ue_data_2021)

# Unidad ejecutora por categoria presupuestal

cp_2019 <- ue_data %>%
  select(`Unidad Ejecutora`, cod_ue) %>%
  mutate(data = map(cod_ue, ~api_ue_data(year = 2019L, `4` = .x, `30` = 9001L))) %>%
  unnest(data) %>%
  mutate(across(.fns = as.character))

cp_2020 <- ue_data %>%
  select(`Unidad Ejecutora`, cod_ue) %>%
  mutate(data = map(cod_ue, ~api_ue_data(year = 2020L, `4` = .x, `30` = 9001L))) %>%
  unnest(data) %>%
  mutate(across(.fns = as.character))

cp_2021 <- ue_data %>%
  select(`Unidad Ejecutora`, cod_ue) %>%
  mutate(data = map(cod_ue, ~api_ue_data(year = 2021L, `4` = .x, `30` = 9001L))) %>%
  unnest(data) %>%
  mutate(across(.fns = as.character))

categoria_presupuestal <- bind_rows(cp_2019, cp_2020, cp_2021)

# Categoria presupuestal por fuente de financiamiento

fuente <- categoria_presupuestal %>%
  mutate(cod_cp = str_extract(`Categoría Presupuestal`, "\\d{4}")) %>%
  select(`Unidad Ejecutora`, cod_ue, `Categoría Presupuestal`, cod_cp, año) %>%
  mutate(data = pmap(list(year = año, `4` = cod_ue, `30` = cod_cp, `14` = 1L), api_ue_data)) %>%
  select(`Unidad Ejecutora`, `Categoría Presupuestal`, data)

fuente_financiamiento_ue <- fuente %>%
  mutate(data = map(data, ~mutate(.x, across(.fns = as.character)))) %>%
  unnest(data)

# Categoria presupuestal por generica

generica_ue <- categoria_presupuestal %>%
  mutate(cod_cp = str_extract(`Categoría Presupuestal`, "\\d{4}")) %>%
  select(`Unidad Ejecutora`, cod_ue, `Categoría Presupuestal`, cod_cp, año) %>%
  mutate(data = pmap(list(year = año, `4` = cod_ue, `30` = cod_cp, `24` = 1L), api_ue_data)) %>%
  select(`Unidad Ejecutora`, `Categoría Presupuestal`, data) %>%
  mutate(data = map(data, ~mutate(.x, across(.fns = as.character)))) %>%
  unnest(data)

# Unidad ejecutora por producto

productos <- unidad_ejecutora %>%
  select(`Unidad Ejecutora`, año, cod_ue) %>%
  mutate(data = map2(año, cod_ue, ~api_ue_data(year = .x, `4` = .y, `31` = 1L))) %>%
  select(`Unidad Ejecutora`, cod_ue, data) %>%
  mutate(data = map(data, ~mutate(.x, across(.fns = as.character)))) %>%
  unnest(data)

# Categoria presupuestal por productos

cp_por_prod <- categoria_presupuestal %>%
  mutate(cod_cp = str_extract(`Categoría Presupuestal`, "\\d{4}")) %>%
  select(`Unidad Ejecutora`, cod_ue, `Categoría Presupuestal`, cod_cp, año) %>%
  mutate(data = pmap(list(year = año, `4` = cod_ue, `30` = cod_cp, `31` = 1L), api_ue_data)) %>%
  select(`Unidad Ejecutora`, `Categoría Presupuestal`, data) %>%
  mutate(data = map(data, ~mutate(.x, across(.fns = as.character)))) %>%
  unnest(data)

# Categoria 0066: formacion universitaria de pregrado por sub producto

actividades_0066 <- cp_por_prod %>%
  mutate(cod_ue = str_replace(`Unidad Ejecutora`, ".*-(.{4}).*", "\\1")) %>%
  mutate(cod_cp = str_extract(`Categoría Presupuestal`, "\\d{4}")) %>%
  mutate(cod_prod = str_extract(`Producto / Proyecto`, "\\d{7}")) %>%
  filter(cod_cp == "0066") %>%
  select(`Unidad Ejecutora`, cod_ue, `Categoría Presupuestal`, cod_cp, `Producto / Proyecto`, cod_prod, año) %>%
  mutate(data = pmap(list(year = año, `4` = cod_ue, `30` = cod_cp, `31` = cod_prod, `32` = 1L), api_ue_data)) %>%
  select(`Unidad Ejecutora`, `Categoría Presupuestal`,  `Producto / Proyecto`, data) %>%
  mutate(data = map(data, ~mutate(.x, across(.fns = as.character)))) %>%
  unnest(data)

# producto por actividad

actividades <- productos %>%
  mutate(cod_prod = str_extract(`Producto / Proyecto`, "\\d{7}")) %>%
  select(`Unidad Ejecutora`, año, `Producto / Proyecto`, cod_ue, cod_prod) %>%
  mutate(data = pmap(list(year = año, `4` = cod_ue, `31` = cod_prod, `32` = 1L), api_ue_data)) %>%
  select(`Unidad Ejecutora`, `Producto / Proyecto`, data) %>%
  mutate(data = map(data, ~mutate(.x, across(.fns = as.character)))) %>%
  unnest(data)

## Resumen -----

clean_ue <- unidad_ejecutora %>%
  mutate(across(PIA:`Avance %`, parse_number)) %>%
  select(-starts_with("cod"), -`Avance %`)

clean_cp <- categoria_presupuestal %>%
  mutate(across(PIA:`Avance %`, parse_number)) %>%
  select(-starts_with("cod"), -`Avance %`)

clean_ff_ue <- fuente_financiamiento_ue %>%
  mutate(across(PIA:`Avance %`, parse_number)) %>%
  select(-starts_with("cod"), -`Avance %`)

clean_generica_ue <- generica_ue %>%
  mutate(across(PIA:`Avance %`, parse_number)) %>%
  select(-starts_with("cod"), -`Avance %`)

clean_productos <- productos %>%
  mutate(across(PIA:`Avance %`, parse_number)) %>%
  select(-starts_with("cod"), -`Avance %`)

clean_0066 <- actividades_0066 %>%
  mutate(across(PIA:`Avance %`, parse_number)) %>%
  select(-starts_with("cod"), -`Avance %`)

clean_actividades <- actividades %>%
  mutate(across(PIA:`Avance %`, parse_number)) %>%
  select(-starts_with("cod"), -`Avance %`)

# Tabla 1

clean_cp %>%
  group_by(año, `Categoría Presupuestal`) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(avance = Devengado/PIM*100) %>%
  pivot_longer(cols = PIA:avance, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = año, values_from = valor) %>%
  writexl::write_xlsx("categoria_presupuestal.xlsx")

# Tabla 2

clean_ff_ue %>%
  group_by(año, `Fuente de Financiamiento`) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(avance = Devengado/PIM*100) %>%
  pivot_longer(cols = PIA:avance, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = año, values_from = valor) %>%
  writexl::write_xlsx("fuete_financiamiento.xlsx")

# Tabla 3

clean_generica_ue %>%
  group_by(año, Genérica) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(avance = Devengado/PIM*100) %>%
  pivot_longer(cols = PIA:avance, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = año, values_from = valor) %>%
  writexl::write_xlsx("generica.xlsx")


# Tabla 4

clean_productos %>%
  group_by(año, `Producto / Proyecto`) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(avance = Devengado/PIM*100) %>%
  pivot_longer(cols = PIA:avance, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = año, values_from = valor) %>%
  writexl::write_xlsx("producto.xlsx")

# Tabla 5

clean_0066 %>%
  group_by(año, `Producto / Proyecto`) %>%
  summarise(across(where(is.numeric), .fns = sum, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(avance = Devengado/PIM*100) %>%
  pivot_longer(cols = PIA:avance, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = año, values_from = valor) %>%
  writexl::write_xlsx("cp0066_subproducto.xlsx")

# Ayuda memoria


get_presupuesto_anual <- function(universidad, year){

  clean_ue %>%
    filter(str_starts(`Unidad Ejecutora`, universidad)) %>%
    filter(año == year)
}

get_cp <- function(universidad, year) {

  clean_cp %>%
      filter(str_starts(`Unidad Ejecutora`, universidad)) %>%
      filter(año == year) %>%
      select(`Categoría Presupuestal`, PIM, Devengado) %>%
      mutate(
        porcentaje = PIM/sum(PIM)*100,
        avance = Devengado/PIM*100
      )
}

get_ff <- function(universidad, year) {

  clean_ff_ue %>%
    filter(str_starts(`Unidad Ejecutora`, universidad)) %>%
    filter(año == year) %>%
    select(`Categoría Presupuestal`, `Fuente de Financiamiento`, PIM, Devengado) %>%
    group_by(`Categoría Presupuestal`) %>%
    mutate(
      porcentaje = PIM/sum(PIM)*100,
      avance = Devengado/PIM*100
    )
}

get_generica <- function(universidad, year) {

  clean_generica_ue %>%
      filter(str_starts(`Unidad Ejecutora`, universidad)) %>%
      filter(año == year) %>%
      select(`Categoría Presupuestal`, Genérica, PIM, Devengado) %>%
      group_by(`Categoría Presupuestal`) %>%
      mutate(
        porcentaje = PIM/sum(PIM)*100,
        avance = Devengado/PIM*100
      ) %>%
    arrange(`Categoría Presupuestal`, desc(porcentaje))
}

get_actividades <- function(universidad, year) {

    clean_actividades %>%
      filter(str_starts(`Unidad Ejecutora`, universidad)) %>%
      filter(año == year) %>%
      select(`Producto / Proyecto`, `Actividad / Acción de Inversión / Obra`, PIM, Devengado) %>%
      group_by(`Producto / Proyecto`) %>%
      mutate(
        porcentaje = (PIM/sum(PIM)*100) %>% round(1),
        avance = (Devengado/PIM*100) %>% round(1)
      ) %>%
    arrange(`Producto / Proyecto`, desc(porcentaje)) %>%
    ungroup()
}

clean_ue %>% View()

universidad <- "001-1707"
year <- "2021"

get_presupuesto_anual(universidad, year) %>% View()
get_cp(universidad, year) %>% View()
get_ff(universidad, year) %>% View()
get_generica(universidad, year) %>% View()
get_actividades(universidad, year) %>%
  mutate(
    producto = str_remove(`Producto / Proyecto`, "^.{9}"),
    actividad = str_remove(`Actividad / Acción de Inversión / Obra`, "^.{9}")
  ) %>%
  select(producto, actividad, PIM:avance) %>%
  mutate(
    across(producto:actividad, str_to_sentence),
    producto = if_else(producto == "Sin producto", "sin producto asignado", str_c("del proyecto ", producto)),
    texto = str_glue("{actividad}, {producto}. Representó el {porcentaje}% del presupuesto del proyecto, con una asignación de S/ {PIM} y una ejecución presupuestal de {avance}%.")
  ) %>%
  pull(texto) %>%
  paste0(collapse = "\n") %>%
  writeLines()


## test

# TOOD: completar

number <- 123

digitos <- str_length(number)

cientos <- c("cien", "doscientos", "trescientos", "cuatrocientos", "quinientos",
             "seiscientos", "setecientos", "ochocientos", "novecientos")

centena <- cientos[number %/% 10^(digitos-1)]
