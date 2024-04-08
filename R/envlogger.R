#' Get paths to example files
#'
#' @description
#' `R_tools-package` comes bundled with several sample files in its inst/extdata directory. This function make them easy to access
#'
#' @param pattern Pattern to select one or more example files. Pattern is vectorized, so more than one value can be supplied. If NULL, all example files are listed.
#'
#' @return
#' The full path to one or more example files, or the filen ames of all example files available.
#'
#' @export
#'
#' @examples
#' # Get the file names of all example files
#' R_tools_example()
#'
#' # Get the full path to a target example file
#' R_tools_example("04FD 1700 8A6C 05-20230830 180125")
#'
#' # Get the full paths to any example files matching a search string
#' R_tools_example("2023-08-30 - uksca")
#'
#' # 'R_tools_example()' is vectorized, meaning that multiple search strings can be used
#' R_tools_example(c("04E9 6B00 943C 0C", "04CA A900 E31C 0F"))
R_tools_example <- function(pattern = NULL) {
  filenames <- dir(system.file("extdata", package = "R_tools"), recursive = TRUE)
  if (is.null(pattern)) {
    filenames
  } else {
    targets <- pattern %>%
      purrr::map(~stringr::str_subset(filenames, .x)) %>%
      unlist()
    system.file("extdata", targets, package = "R_tools", mustWork = TRUE)
  }
}

#' Check if a path points to an EnvLogger file, logfile or none of the two
#'
#' @param path A path to a file
#'
#' @return
#' A single numeric value, `1` if an EnvLogger file, `2` if an EnvLogger logfile, and `0` if none of the two.
#'
#' @export
#'
#' @examples
#' path <- R_tools_example("04FD 1700 8A6C 05-20230830 180125")
#' is.envlogger(path) # 1
#'
#' path <- R_tools_example("log_")[1]
#' is.envlogger(path)   # 2
is.envlogger <- function(path) {
  x <- path %>%
    readr::read_lines() %>%
    stringr::str_to_lower()

  has.eb <- x %>%
    stringr::str_detect("www.electricblue.eu") %>%
    any()

  has.envlogger <- x %>%
    stringr::str_detect("envlogger") %>%
    any()

  has.tap <- x %>%
    stringr::str_detect("tap logger") %>%
    any()

  has.download <- x %>%
    stringr::str_detect("data downloaded") %>%
    any()

  has.mission <- x %>%
    stringr::str_detect("mission running") %>%
    any()

  has.waiting <- x %>%
    stringr::str_detect("waiting to start") %>%
    any()

  # return
  val <- 0
  if (has.eb & has.envlogger) val <- 1
  if (has.tap | has.download | has.mission | has.waiting) val <- 2
  return(val)
}

#' Check if the paths provided point to EnvLogger files (quick)
#'
#' @description
#' Quickly determine if the paths provided point to EnvLogger report files.
#' This is done by testing if line 2 is equal to the string "www.electricblue.eu, Portugal".
#' As a result, reports generated using the the earliest versions of EnvLogger_Viewer will not be recognized.
#' Also, if the structure of the file has been changed (e.g., by opeining and saving in Excel), it will no longer be recognized as a report.
#'
#' @param paths A vector of paths to files.
#' @param negate 	If `TRUE`, inverts the resulting boolean vector.
#'
#' @return
#' A logical vector the same length as `paths`.
#'
#' @export
#'
#' @examples
#' paths <- dir(
#'  "/Users/ruiseabra/Dropbox/RS/bio/datasets/temp_loggers/reports/tidy/cctbon_norterocks/2021-01-18",
#'  full.names = TRUE,
#'  pattern = ".csv")
#' paths <- stringr::str_subset(paths, "log_", negate = TRUE)
#' is.envlogger(path) # 1
is.report_quick <- function(paths, negate = FALSE) {
  val <- purrr::map_lgl(paths, ~readLines(.x, n = 2)[2] == "www.electricblue.eu, Portugal")
  val[is.na(val)] <- FALSE
  if (negate) val <- !val
  return(val)
}

#' Get the value of a given field in an Envolgger file header
#'
#' @param header A tibble with columns `field` and `val` and all values in lower case
#' @param field_pattern A string that must match exactly one entry in the `field`column
#' @param force_numeric Logical (defaults to `FALSE`). If `TRUE`, the value is stripped of non-numeric characters and converted to numeric
#'
#' @return
#' The value for the target field, as string or numeric (if `force_numeric = TRUE`).
#'
#' @export
#'
#' @examples
#' header <- tibble::tibble(field = c("field1", "field2", "field3"), val = c("a", "1", "a1"))
#' env_header_val(header, "field1")
#' env_header_val(header, "field2", force_numeric = TRUE)
#' env_header_val(header, "field2", force_numeric = TRUE)
env_header_val <- function(header, field_pattern, force_numeric = FALSE) {
  val <- header$val[header$field == field_pattern]
  if (length(val) != 1) stop("'field_pattern' must match no more and no less than 1 time")
  if (force_numeric) val <- stringr::str_remove_all(val, "[^0-9.]") %>% as.numeric()
  val
}

#' Read the header of an EnvLogger file
#'
#' @description
#' Given a file path, check if it points to an EnvLogger file and, if `TRUE`, read just the header
#'
#' @param path Path to an EnvLogger file
#' @param check Logical, whether or not to check if path points to an EnvLogger file. Defaults to `TRUE`. Only meant to be `FALSE` when called from within `read_env()`, so as not to repeat the same check multiple times. **Do not set it to `FALSE` when running this function directly.**
#'#'
#' @return
#' A tibble with 1 row and 12 columns with the metadata values for most of the fields recorded in the header.
#'
#' @export
#'
#' @seealso [read_env_data()], [read_env()], [plot_env()]
#'
#' @examples
#' path <- R_tools_example("04FD 1700 8A6C 05-20230830 180125")
#' read_env_header(path)
#'
#' paths <- R_tools_example("iemin \\+ iespi")
#' paths <- paths[!grepl("log_", paths)]
#' headers <- purrr::map(paths, read_env_header)
#' purrr::list_rbind(headers)
read_env_header <- function(path, check = TRUE) {
  if (check) {
    env_status <- is.envlogger(path)
    if (env_status != 1) stop("not an EnvLogger file")
  }

  x <- readr::read_lines(path)

  skip <- max(stringr::str_which(x, "------------------------------"))

  data_rows_na <- stringr::str_which(x, ".,NA$")
  data_rows_na <- data_rows_na[data_rows_na > skip]
  data_rows_n  <- length(x) - skip - 1 - length(data_rows_na)

  header <- readr::read_csv(
    path,
    n_max = skip,
    col_names = c("field", "val"),
    show_col_types = FALSE
  ) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), stringr::str_to_lower))

  # fix fields that were named differently or were missing on earlier versions
  header$field[header$field == "time diff (sec)"] <- "time diff [logger-smartphone](sec)"
  header$field[header$field == "time diff [logger-smartphone](s)"] <- "time diff [logger-smartphone](sec)"
  if (!any(header$field == "device")) header <- tibble::add_row(header, field = "device", val = "missing")
  if (!any(header$field == "custom name")) header <- tibble::add_row(header, field = "custom name", val = "missing")

  header <- tibble::tibble(
    id       = env_header_val(header, "custom name"),
    serial   = env_header_val(header, "serial number") %>% stringr::str_replace_all(" ", ""),
    nrow     = data_rows_n,
    skip     = skip,
    lat      = env_header_val(header, "lat", TRUE),
    lon      = env_header_val(header, "long", TRUE),
    res      = env_header_val(header, "sampling resolution", TRUE),
    tdiff    = env_header_val(header, "time diff [logger-smartphone](sec)", TRUE),
    v_log    = env_header_val(header, "EnvLogger version", TRUE),
    v_app    = env_header_val(header, "EnvLogger viewer version", TRUE),
    dev_type = env_header_val(header, "downloading device type"),
    dev_make = env_header_val(header, "device"),
    dev_name = if (v_app > 5) env_header_val(header, "device name") else "",
    download = env_header_val(header, "downloading mode")
  )

  # return
  return(header)
}

#' Read the data from an EnvLogger file
#'
#' @description
#' Given a file path, check if it points to an EnvLogger file and, if `TRUE`, read just the data
#'
#' @inheritParams read_env_header
#' @param skip A numeric value indicating how many rows to skip when reading the target EnvLogger file to skip the header (the `$skip` column of the output from `read_env_header()`).
#' @param zero_secs Logical, whether to remove trailing seconds (only used if path points to an EnvLogger file; adjustment determined based on the first timestamp)
#'
#' @return
#' A tibble with 2 columns (`t` and `temp`) header$nrow` rows and 2 columns .
#'
#' @export
#'
#' @seealso [read_env_header()], [read_env()], [plot_env()]
#'
#' @examples
#' path <- R_tools_example("04FD 1700 8A6C 05-20230830 180125")
#' read_env_header(path)
read_env_data <- function(path, skip, zero_secs = TRUE, check = TRUE) {
  if (check) {
    env_status <- is.envlogger(path)
    if (env_status != 1) stop("not an EnvLogger file")
  }

  data <- readr::read_csv(
    path,
    skip = skip,
    show_col_types = FALSE
  ) %>%
    tidyr::drop_na() %>%
    dplyr::rename(t = time)

  if (zero_secs) data <- dplyr::mutate(data, t = t - lubridate::second(t[1]))

  # return
  return(data)
}

#' Read an EnvLogger logfile
#'
#' @description
#' Given a file path, check if it points to an EnvLogger logfile and, if `TRUE`, read it.
#'
#' @inheritParams read_env_header
#' @param path Path to an EnvLogger logfile
#'
#' @return
#' A tibble with one row for each interaction with an EnvLogger and 15 columns with relevant metadata.
#'
#' @export
#'
#' @seealso [read_env_header()], [read_env_dara()], [read_env()], [plot_env()]
#'
#' @examples
#' path <- R_tools_example("log_")[1]
#' read_env_log(path)
read_env_log <- function(path, check = TRUE) {
  if (check) {
    env_status <- is.envlogger(path)
    if (env_status != 2) stop("not an EnvLogger logfile")
  }

  cols <- path %>%
    readr::read_lines(n_max = 1) %>%
    stringr::str_split_1(",") %>%
    stringr::str_trim() %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all(" ", "_") %>%
    stringr::str_replace_all("[^[:alpha:]_]", "") %>%
    stringr::str_replace_all("sampling", "samp") %>%
    stringr::str_replace_all("interval", "int") %>%
    stringr::str_replace_all("resolution", "res")

  log <- path %>%
    readr::read_csv(skip = 1, col_names = cols, show_col_types = FALSE) %>%
    dplyr::select(-accuracy) %>%
    dplyr::mutate(
      code = as.character(code),
      time = stringr::str_sub(time, 1, 19) %>% as.POSIXct(),
      start_time = stringr::str_sub(start_time, 1, 19) %>% as.POSIXct()
    )

  if (!("device" %in% cols))  log <- tibble::add_column(log, device = "unknown")

  log <- log %>%
    dplyr::rename(
      serial = id,
      diff = time_diff_s,
      lon = long,
      v = version,
      start = start_time,
      dev = device,
      id = name
    ) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), stringr::str_to_lower)) %>%
    dplyr::relocate(time, id, serial, action, status)

  # return
  return(log)
}

#' Read an EnvLogger file or logfile
#'
#' @description
#' Given a file path, check if it points to an EnvLogger file or logfile and, if `TRUE`, read it.
#'
#' @param path Path to an EnvLogger file or logfile
#' @inheritParams read_env_header
#' @inheritParams read_env_data
#'
#' @seealso [read_env_header()], [read_env_data()], [read_env_log()], [plot_env()]
#'
#' @return
#' If path points to an EnvLogger file, a tibble with 1 row and 13 columns, among which `id` and `data`. If path points to an EnvLogger logfile, a tibble with one row for each interaction with an EnvLogger and 15 columns with relevant metadata.
#'
#' @export
#'
#' @examples
#' path <- R_tools_example("04FD 1700 8A6C 05-20230830 180125")
#' read_env(path, zero_secs = TRUE) # an EnvLogger file
#'
#' path <- R_tools_example("log_")[1]
#' read_env(path) # an EnvLogger logfile
read_env <- function(path, zero_secs = TRUE) {
  env_status <- is.envlogger(path)
  if (!env_status) stop("not an EnvLogger file or logfile")

  if (env_status == 1) {
    header <- read_env_header(path, check = FALSE)
    data <- read_env_data(path, skip = header$skip, zero_secs = zero_secs, check = FALSE)

    # tidy
    header$data <- list(data)
    out <- dplyr::relocate(header, id, serial, data)
  } else {
    out <- read_env_log(path, check = FALSE)
  }

  # return
  return(out)
}

#' Plot data from one EnvLogger
#'
#' @param env_data A tibble with EnvLogger data (columns `t` and `temp`)
#'
#' @return
#' A ggplot
#'
#' @seealso [read_env()]
#'
#' @export
#'
#' @examples
#' path <- R_tools_example("04FD 1700 8A6C 05-20230830 180125")
#' env_data <- read_env(path, zero_secs = TRUE)
#' plot_env(env_data$data[[1]])
plot_env <- function(env_data) {
  ggplot2::ggplot(env_data) +
    ggplot2::geom_line(ggplot2::aes(t, temp)) +
    ggplot2::xlab("") + ggplot2::ylab("")
}
