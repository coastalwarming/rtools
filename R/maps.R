#' Determine a coastline and find its linear sequence
#'
#' @description
#' Given a raster mask, determine the coastline and number each of the coastal pixels sequentially (currently only supports the France-Iberia-Morocco region)
#'
#' @param mask A raster sea-land mask, with sea pixels = 0 and land pixels = 1 (fractional values for transition pixels are allowed but not required).
#' @param sea_pixels Logical indicating if the shoreline is composed of the sea pixels closest to land (`sea_pixels = TRUE`) or land pixels closest to sea (`sea_pixels = FALSE`).
#' @param water Numerical from (0,1] (fractional values allowed) used as cutpoint for sea (`<= water`) and land (`> water`) pixels.
#' @param PLOT Locical; if `TRUE`, the progress of the computation is shown as a plot that is continuously updated
#'
#' @return
#' A list with two elements: `$map` is a raster with the same extent as the input mask, and with all cells set to NA except those corresponding to the coastline, which are numbered sequentially; `$pix` is a tibble with all the coastline cells identified and the following columns: `i` = sequential index, `x` = lon, `y` = lat, and `cell` = cell number.
#'
#' @export
#'
#' @examples
#' # Load a raster mask
#' mask <- terra::rast(system.file("extdata/ERA5_mask.nc", package = "rtools"))
#'
#' # Determine the coastline and find its N-to-S linear sequence
#' coastline_iberia(mask)
coastline_iberia <- function(mask, sea_pixels = TRUE, water = 0.1, PLOT = TRUE) {
  # mask <- terra::rast("inst/extdata/ERA5_mask.nc")

  # determine binary land
  land <- as.numeric(mask > water)

  # retain only the atlantic
  # HARDCODED FOR IBERIA
  land[
    terra::rowFromY(land, 42):terra::nrow(land),
    terra::colFromX(land, -5.3):terra::ncol(land)] <- 1

  # reclassify
  land[land[] == 0] <- NA
  land[land[] == 1] <- 0

  # find coastline
  coastline_map <- terra::boundaries(land, inner = !sea_pixels, directions = 8, falseval = NA)
  coastline_map[!is.na(coastline_map)] <- 0

  # ensure that only one shoreline remains (the one covering the most pixels)
  main_coastline <- coastline_map %>% terra::patches()
  keep <- main_coastline %>%
    terra::extract(y = which(!is.na(main_coastline[]))) %>%
    tibble::tibble() %>%
    dplyr::group_by(patches) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::arrange(dplyr::desc(n)) %>%
    dplyr::pull(patches) %>%
    dplyr::first()
  coastline_map[main_coastline[] != keep] <- NA

  # find first pixel
  first_cell <- coastline_map %>%
    terra::extract(y = which(!is.na(coastline_map[])), xy = TRUE) %>%
    tibble::tibble() %>%
    dplyr::mutate(cell = terra::cells(coastline_map)) %>%
    dplyr::arrange(dplyr::desc(y), dplyr::desc(x)) %>%
    dplyr::slice(1) %>%
    dplyr::pull(cell)

  # set value for pixel #1
  # HARDCODED FOR IBERIA
  coastline_map[first_cell] <- 1

  # walk along the shoreline
  while (any(coastline_map[!is.na(coastline_map[])] == 0)) {
    last_cell <- coastline_map %>%
      terra::extract(y = which(!is.na(coastline_map[])), xy = TRUE) %>%
      tibble::tibble() %>%
      dplyr::mutate(cell = terra::cells(coastline_map)) %>%
      dplyr::rename(i = 3) %>%
      dplyr::arrange(desc(i)) %>%
      dplyr::mutate(
        near_zeros = purrr::map_lgl(cell, ~coastline_map %>%
                                      terra::adjacent(.x, directions = "queen") %>%
                                      as.numeric() %>%
                                      terra::extract(coastline_map, .) %>%
                                      tibble::tibble() %>%
                                      dplyr::pull(1) %>%
                                      "=="(0) %>%
                                      any()
                                    )
      ) %>%
      dplyr::filter(near_zeros) %>%
      dplyr::slice(1)

    next_i <- last_cell$i + 1

    last_cell <- last_cell$cell

    next_cell <- tibble::tibble(
      rook = c(rep(TRUE, 4), rep(FALSE, 4)),
      dir  = c("n", "w", "e", "s", "nw", "ne", "sw", "se"),
      pref = c( 4 ,  6 ,  2 ,  7 ,   5 ,   1 ,   8 ,   3 ),
      cell = cbind(
        terra::adjacent(coastline_map, cell = last_cell, directions = "rook"),
        terra::adjacent(coastline_map, cell = last_cell, directions = "bishop")
      ) %>%
        as.numeric(),
      val = terra::extract(coastline_map, cell)$lsm
    ) %>%
      dplyr::arrange(pref) %>%
      tidyr::drop_na() %>%
      dplyr::filter(val == 0) %>%
      dplyr::slice(1) %>%
      dplyr::pull(cell)

    coastline_map[next_cell] <- next_i
    if (PLOT) terra::plot(coastline_map)
  }

  # pixel list
  coastline_pixels <- coastline_map %>%
    terra::extract(y = which(!is.na(coastline_map[])), xy = TRUE) %>%
    tibble::tibble() %>%
    dplyr::mutate(cell = terra::cells(coastline_map)) %>%
    dplyr::rename(i = 3) %>%
    dplyr::arrange(i) %>%
    dplyr::filter(i > 0) %>%
    dplyr::relocate(i)

  # return
  list(
    map = coastline_map,
    pix = coastline_pixels
  )
}
