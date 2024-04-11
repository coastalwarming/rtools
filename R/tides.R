#' Get tide heights for any location (all computations in UTC)
#'
#' @param lat Latitude (numeric)
#' @param lon Longitude (numeric)
#' @param first_day A Date or a string that can be converted with as.Date
#' @param last_day A Date or a string that can be converted with as.Date
#' @param freq_mins Get tide heights every x mins (numeric)
#' @param tz TZ offset (in hours; defaults to zero, which returns times in UTC0)
#' @param mode One of "lo" (low tides), "hi" (high tides), "lohi" or "hilo" (low and high tides) or "all" (tide height every `freq_mins`) to indicate which tide elements to output
#' @param FES_HANDLER A file path pointing to "fes2014/ocean_tide.ini" or a fes_handler
#'
#' @return
#' A tibble with 4 columns:
#' * `$time`, timestamps (always UTC + `tz`)
#' * `$h`, tide elevation (in meters) relative to altimeter-derived Mean Water Level
#' * `$lo`, logical, `TRUE` at low tides
#' * `$hi`, logical, `TRUE` at high tides
#'
#' @export
#'
#' @examples
#'
#' # Saint Peter Saint Paul Archipelago
#' (t <- tides(
#'   lat = 0.917,
#'   lon = -29.346,
#'   first_day = "2023-10-14",
#'   last_day  = "2023-10-19",
#'   freq_mins = 10,
#'   FES_HANDLER = "/Users/ruiseabra/RS Dropbox/Rui Seabra/RS/bio/datasets/Rfes2014/src/fes2014/data/fes2014/ocean_tide.ini"
#' ))
#' plot(t$time, t$h, type = "l")
#'
#' # Only low tides
#' # Leixões (Portugal)
#' (t <- tides(
#'   lat = 41.1833,
#'   lon = -8.7000,
#'   first_day = Sys.Date(),
#'   last_day  = Sys.Date() + 7,
#'   freq_mins = 10,
#'   mode = "lo",
#'   FES_HANDLER = "/Users/ruiseabra/RS Dropbox/Rui Seabra/RS/bio/datasets/Rfes2014/src/fes2014/data/fes2014/ocean_tide.ini"
#' ))
#'
#'
tides <- function(lat, lon, first_day = Sys.Date(), last_day = Sys.Date() + 7, freq_mins = 10, tz = 0, mode = "all", FES_HANDLER) {
  # CHECKS BEGIN ----- #
  stopifnot(length(lat) == 1)
  stopifnot(length(lon) == 1)
  stopifnot(length(freq_mins) == 1)
  stopifnot(is.numeric(lat))
  stopifnot(is.numeric(lon))
  stopifnot(is.numeric(freq_mins))
  stopifnot(class(FES_HANDLER) == "character" | class(FES_HANDLER) == "externalptr")
  # CHECKS END ----- #

  old_TZ <- Sys.getenv("TZ")
  Sys.setenv(TZ = "UTC")
  on.exit(Sys.setenv(TZ = old_TZ), add = TRUE)

  first_day <- as.Date(first_day)
  last_day  <- as.Date(last_day)

  t0 <- lubridate::as_datetime(first_day - 2)
  t1 <- lubridate::as_datetime(last_day + 2)

  tides <- tibble::tibble(
    lat = lat,
    lon = lon,
    time = seq.POSIXt(from = t0, to = t1, by = 60 * freq_mins),
    epoch_sec = as.integer(time)
  )

  fes_handler <- if (class(FES_HANDLER) == "character") Rfes2014::fes_new("tide", "memory", FES_HANDLER) else FES_HANDLER

  tides$h <- Rfes2014::fes_calculate(fes_handler, tides)$h

  if (all(is.na(tides$h))) stop("the supplied latlon likely points to land")

  if (class(FES_HANDLER) == "character") {Rfes2014::fes_delete(fes_handler); rm(fes_handler)}

  # convert to local TZ using the "tz" values provided
  tides <- dplyr::mutate(tides,
                         time = time + lubridate::hours(tz),
                         h    = round(h / 100, 3),
                         lo   = zoo::rollapply(h, 5, which.min, align = "center", fill = 0) == 3,
                         hi   = zoo::rollapply(h, 5, which.max, align = "center", fill = 0) == 3
  )

  if (mode != "all") {
    if (mode == "lo") tides <- dplyr::filter(tides, lo)
    if (mode == "hi") tides <- dplyr::filter(tides, hi)
    if (mode == "hilo" | mode == "lohi") tides <- dplyr::filter(tides, lo | hi)
  }

  # return
  return(dplyr::select(tides, t = time, h, lo, hi))
}

#' Check if a combination of lat and lon points to a valid FES2014 location
#'
#' @inheritParams tides
#'
#' @return
#' A logical, `TRUE` if the location is valid and `FALSE` if the location is not
#'
#' @export
#'
#' @examples
#' FES_HANDLER = "/Users/ruiseabra/RS Dropbox/Rui Seabra/RS/bio/datasets/Rfes2014/src/fes2014/data/fes2014/ocean_tide.ini"
#'
#' # Saint Peter Saint Paul Archipelago, Brazil
#' # (valid location)
#' is.tides_location(
#'   lat = 0.917,
#'   lon = -29.346,
#'   FES_HANDLER = FES_HANDLER
#' )
#'
#' # Angeiras, Portugal
#' # (valid location)
#' is.tides_location(
#'   lat = 41.262,
#'   lon = -8.712,
#'   FES_HANDLER = FES_HANDLER
#' )
#'
#' # Angeiras (inland), Portugal
#' # (invalid location)
#' is.tides_location(
#'   lat = 41.262,
#'   lon = -8.000,
#'   FES_HANDLER = FES_HANDLER
#' )
is.tides_location <- function(lat, lon, FES_HANDLER) {
  # CHECKS BEGIN ----- #
  stopifnot(length(lat) == 1)
  stopifnot(length(lon) == 1)
  stopifnot(is.numeric(lat))
  stopifnot(is.numeric(lon))
  stopifnot(class(FES_HANDLER) == "character" | class(FES_HANDLER) == "externalptr")
  # CHECKS END ----- #

  old_TZ <- Sys.getenv("TZ")
  Sys.setenv(TZ = "UTC")
  on.exit(Sys.setenv(TZ = old_TZ), add = TRUE)

  t0 <- as.POSIXct("2023-10-14")
  t1 <- as.POSIXct("2023-10-14 01:00")

  tides <- tibble::tibble(
    lat = lat,
    lon = lon,
    time = seq.POSIXt(from = t0, to = t1, by = 3600),
    epoch_sec = as.integer(time)
  )

  fes_handler <- if (class(FES_HANDLER) == "character") Rfes2014::fes_new("tide", "memory", FES_HANDLER) else FES_HANDLER

  tides$h <- Rfes2014::fes_calculate(fes_handler, tides)$h

  if (class(FES_HANDLER) == "character") {Rfes2014::fes_delete(fes_handler); rm(fes_handler)}

  # return
  return(!is.na(tides$h[1]))
}

#' Get tide heights for several locations
#'
#' @inheritParams tides
#' @param x Either a tibble with 4 columns (loc = location name, lat = latitude, lon = longitude, tz = tz offset) or a path to a csv file that can be read with readr::read_csv to generate such a tibble

#' @return
#' A tibble with times and tide heights for each target location
#'
#' @export
#'
#' @examples
#' x <- tibble::tibble(
#'     loc = c(
#'       "Kap Farvel (Greenland)",
#'       "Scrabster (Scotland)",
#'       "Sachem Head (USA)",
#'       "Shinnecock Yacht Club (USA)",
#'       "Cape Hatteras (USA)",
#'       "Fernando de Noronha (Brazil)"
#'       ),
#'     lat = c(
#'       59.7500,
#'       58.6100,
#'       41.2450,
#'       40.8183,
#'       35.2333,
#'       -3.8333
#'     ),
#'     lon = c(
#'       -43.8833,
#'       -3.5450,
#'       -72.7083,
#'       -72.5533,
#'       -75.5167,
#'       -32.4167
#'     ),
#'     tz = c(
#'        -2,
#'        0,
#'        -5,
#'        -5,
#'        -5,
#'        -2
#'     )
#'  )
#'
#' tides_multiple(x, FES_HANDLER = "/Users/ruiseabra/RS Dropbox/Rui Seabra/RS/bio/datasets/Rfes2014/src/fes2014/data/fes2014/ocean_tide.ini")
tides_multiple <- function(x, first_day = Sys.Date(), last_day = Sys.Date() + 7, freq_mins = 10, mode = "all", FES_HANDLER) {

  stopifnot(class(FES_HANDLER) == "character")
  fes_handler <- Rfes2014::fes_new("tide", "memory", FES_HANDLER)

  if (all(class(x) == "character")) {
    x <- readr::read_csv(x, show_col_types = FALSE)
    colnames(x) <- c("loc", "lat", "lon", "tz")
  }

  # check that all locations are valid
  x <- dplyr::mutate(x, valid = purrr::map2_lgl(lat, lon, ~is.tides_location(lat = .x, lon = .y, FES_HANDLER = fes_handler)))
  if (any(!x$valid)) stop(paste0("some locations do not point to a valid FES2014 pixel: ", paste(dplyr::filter(x, !valid)$loc, collapse = ", ")))
  x$valid <- NULL

  # get tide heights for all target locations
  x <- dplyr::mutate(x,
      tide = purrr::pmap(
        list(lat, lon, tz),
        function(LAT, LON, TZ) tides(
          lat = LAT,
          lon = LON,
          first_day = first_day,
          last_day = last_day,
          freq_mins = freq_mins,
          tz = TZ,
          mode = mode,
          FES_HANDLER = fes_handler)
        )
      )

  Rfes2014::fes_delete(fes_handler); rm(fes_handler)

  return(x)
}

#' Get low tide details and stats for several locations (still being worked)
#'
#' @inheritParams tides_multiple

#' @return
#' A tibble with the times and heights for the low tides during the target period, headed by tide amplitude stats
#'
#' @export
tides_low_stats <- function(x, first_day = Sys.Date(), last_day = Sys.Date() + 7) {

  x <- tibble::tibble(
    loc = c(
      "Kap Farvel (Greenland)",
      "Scrabster (Scotland)",
      "Sachem Head (USA)",
      "Shinnecock Yacht Club (USA)",
      "Cape Hatteras (USA)",
      "Fernando de Noronha (Brazil)"
    ),
    lat = c(
      59.7500,
      58.6100,
      41.2450,
      40.8183,
      35.2333,
      -3.8333
    ),
    lon = c(
      -43.8833,
      -3.5450,
      -72.7083,
      -72.5533,
      -75.5167,
      -32.4167
    ),
    tz = c(
      -2,
      0,
      -5,
      -5,
      -5,
      -2
    )
  )

  first_day = Sys.Date()
  last_day  = Sys.Date() + 7

  first_day <- as.Date(first_day)
  last_day  <- as.Date(last_day)

  x      <- tides_multiple(x, first_day, last_day, freq_mins = 10, mode = "lo")
  x_wide <- tides_multiple(x, first_day - 100, last_day + 100, freq_mins = 10, mode = "all")

  x$wide <- x_wide$tide

  x <- x %>%
    dplyr::mutate(
      hi  = purrr::map_dbl(wide, ~max(.x$h)),
      lo  = purrr::map_dbl(wide, ~min(.x$h)),
      mid = lo + (hi - lo)/2,
      full_rng = hi - lo,
      work_rng = mid - lo,
      tide = purrr::map(tide, ~dplyr::filter(.x, lo) %>%
                          dplyr::select(time, h) %>%
                          dplyr::mutate(
                            d = as.Date(time),
                            t = substr(time, 12, 16),
                            noon = lubridate::hour(time) >= 12
                          ))
    ) %>%
    dplyr::select(-wide) %>%
    tidyr::unnest(cols = "tide")

  x_unique <- dplyr::select(x, d, lat, lon) %>% dplyr::distinct()
  sun <- purrr::pmap_dfr(
    list(x_unique$d, x_unique$lat, x_unique$lon),
    function(D, LAT, LON) suncalc::getSunlightTimes(
      date = D,
      lat  = LAT,
      lon  = LON,
      tz   = "UTC",
      keep = c("sunrise", "sunset")
      )
    ) %>%
    tibble::tibble()

  sun <- dplyr::left_join(
    dplyr::mutate(sun, latlon = stringr::str_c(lat, lon)),
    dplyr::select(x, lat, lon, tz) %>%
      dplyr::distinct() %>%
      dplyr::mutate(latlon = stringr::str_c(lat, lon)) %>%
      dplyr::select(latlon, tz),
    by = "latlon"
    ) %>%
    dplyr::mutate(
      sunrise = sunrise + lubridate::hours(tz),
      sunset  = sunset  + lubridate::hours(tz)
    ) %>%
    dplyr::select(-latlon, -tz)

  x <- dplyr::left_join(
    dplyr::mutate(x,   i = stringr::str_c(d, lat, lon)),
    dplyr::mutate(sun, i = stringr::str_c(date, lat, lon)) %>%
      dplyr::select(i, rise = sunrise, set = sunset),
    by = "i") %>%
    dplyr::select(-i)

  x <- x %>%
    dplyr::mutate(
      light = purrr::pmap_lgl(
        list(time, rise, set),
        function(TIME, RISE, SET) TIME >= RISE & TIME <= SET
      )
    )

  x <- x %>%
    dplyr::select(-rise, -set) %>%
    tidyr::nest(tides = c("time", "h", "d", "t", "noon", "light"))

# HERE (maybe don't nest and instead compute stats for each tide)



  TIME <- locs$tide %>% dplyr::bind_rows() %>% dplyr::pull(date) %>% unique() %>% tibble::tibble(date = .)

  locs$tide <- purrr::map(locs$tide, ~dplyr::left_join(TIME, .x, by = "date") %>%
                            dplyr::mutate(time = substr(time, 12, 16)))

  for (i in 1:nrow(locs)) colnames(locs$tide[[i]]) <- c("date", paste0(c("t", "h"), "_", i))

  tides <- locs$tide[[1]]
  if (nrow(locs) > 1) for (i in 2:nrow(locs)) tides <- dplyr::left_join(tides, locs$tide[[i]], by = "date")

  stats <- dplyr::select(locs, loc, hi, lo, mid, work_rng) %>% t()

  TIDES <- tibble::tibble(c0 = c(rownames(stats), as.character(as.Date(tides$date))))

  r <- nrow(stats)
  for (i in 1:nrow(locs)) {
    ii <- (i * 2) + -1:0
    top <- tibble::tibble(c1 = stats[,i], c2 = rep("", r))
    bot <- tibble::tibble(
      c1 = as.character(unlist(tides[, 1 + ii[1]])),
      c2 = as.character(unlist(tides[, 1 + ii[2]]))
    )
    sh <- dplyr::bind_rows(top, bot)
    colnames(sh) <- paste0("c", ii)

    TIDES <- dplyr::bind_cols(TIDES, sh)
  }

  # return
  readr::write_csv(TIDES, file.path(dirname(path), "tides.csv"), col_names = FALSE)
  return(locs)
}
