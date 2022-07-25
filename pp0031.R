library(httr2)
library(rvest)
library(tidyverse)

api_gasto <- function(actualizacion = "mensual") {
  tipo_act <- match.arg(actualizacion, c("mensual", "diaria"))
  dplyr::case_when(
    tipo_act == "mensual" ~ "https://apps5.mineco.gob.pe/transparencia/mensual/Navegar_6.aspx",
    tipo_act == "diaria" ~ "https://apps5.mineco.gob.pe/transparencia/Navegador/Navegar_7.aspx"
  )
}
# api general
set_url_query <- function(year, actualizacion = "diaria", ...) {
  base_url <- api_gasto(actualizacion = actualizacion) |>
    request() |>
    req_url_query(`_tgt` = "json", `_uhc` = "yes", cpage = 1, psize = 1000, y = year)

  base_url |>
    req_url_query(...)
}

req_html <- function(request) {
  request |>
    req_perform() |>
    resp_body_html() |>
    html_element("body")
}

resp_tbl <- function(response, target = "data") {
  id <- match.arg(target, c("data", "header", "history"))
  id <- if (target == "data") ".Data" else if (target == "header")".MapTable" else ".History"

  response |>
    html_element(id) |>
    html_table()
}

resp_get_tbl <- function(response){
  column_names <- resp_tbl(response, "header") |>
    janitor::row_to_names(2) |>
    janitor::clean_names() |>
    names() |>
    suppressWarnings()

  resp_tbl(response, "data") |>
    setNames(column_names) |>
    dplyr::mutate(dplyr::across(.fns = as.character))
}

resp_get_history <- function(response) {
  # Por alguna razón retorna una tabla vacía
  response |>
    resp_tbl("history")
}

separate_cod_desc <- function(.data, .num_col = 2L) {
  col_name <- names(.data[, .num_col])
  cod_col <- glue::glue("cod_{col_name}")
  desc_col <- glue::glue("desc_{col_name}")

  .data |>
    tidyr::separate(col = .num_col, into = c(cod_col, desc_col), sep = ": ", extra = "merge")
}

api_wrapper <- function(year, actualizacion = "diaria", ...) {
  set_url_query(year = year, actualizacion = actualizacion, ...) |>
    req_html() |>
    resp_get_tbl() |>
    dplyr::mutate(year = year) |>
    dplyr::relocate(year) |>
    janitor::remove_empty("cols") |>
    dplyr::distinct(dplyr::across()) |>
    dplyr::filter(!is.na(readr::parse_number(pia))) |>
    separate_cod_desc() |>
    dplyr::across(pia:avance_percent, readr::parse_number)
    suppressWarnings()
}

set_en_que_se_gasta <- function(categoria_presupuestal = NULL,
                                producto = NULL,
                                actividad = NULL,
                                meta = NULL,
                                funcion = NULL,
                                division_funcional = NULL,
                                grupo_funcional = NULL
                                ) {
  params  <-  list(
    "30" = categoria_presupuestal,
    "31" = producto,
    "32" = actividad,
    "13" = meta,
    "8" = funcion,
    "33" = division_funcional,
    "34" = grupo_funcional
  )

  purrr::discard(params, is.null)
}

set_quien_gasta <- function(nivel = NULL,
                            sector = NULL,
                            pliego = NULL,
                            unidad_ejecutora = NULL,
                            goblocal_o_mancomunidad = NULL,
                            mancomunidad = NULL,
                            departamento = NULL,
                            provincia = NULL,
                            municipalidad = NULL
                            ) {
  params <- list(
    "1" = nivel,
    "2" = sector,
    "3" = pliego,
    "4" = unidad_ejecutora,
    "5" = departamento,
    "6" = provincia,
    "7" = municipalidad,
    "37" = goblocal_o_mancomunidad,
    "36" = mancomunidad
  )

  purrr::discard(params, is.null)
}

## Análisis de PP 0031 ----

set_en_que_se_gasta(categoria_presupuestal = "0031", producto = "")


years <- 2012:2022

# total del PP
respuesta <- purrr::map_dfr(years, api_wrapper)



testing <- api_wrapper(2022, "30" = "0031", "31"="")

testing |>
  filter(cod_proyecto == "3000001")
