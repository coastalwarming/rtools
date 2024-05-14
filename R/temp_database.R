#' List all logfiles in a folder
#'
#' @description
#' Given a path to a folder containing Envlogger data, list all the logfiles.
#'
#' @param folder Path to a folder with Envlogger data
#'
#' @seealso [read_env_log()], [read_deployment_info()]
#'
#' @return
#' A vector with the paths to all logfiles found.
#'
#' @export
#'
#' @examples
#' folder <- "/Users/ruiseabra/Dropbox/RS/bio/datasets/temp_loggers/reports"
#' list_logs(folder)
list_logs <- function(folder) {
  paths <- dir(folder, full.names = TRUE, recursive = TRUE)
  paths[stringr::str_detect(basename(paths), "^log_")]
}

#' List all EnvLogger reports in a folder (quick)
#'
#' @description
#' Given a path to a folder containing Envlogger data, list all files that likely are EnvLogger reports.
#' Since `is.report_quick` only tests if line 2 is equal to the string "www.electricblue.eu, Portugal", reports generated using the the earliest versions of EnvLogger_Viewer or where the structure of the file has been changed (e.g., by opeining and saving in Excel) are not directly identifiable. Therefore, all files that are (more or less) likely to be reports are returned, and the collumn `report` can be used for filtering based on the output from `is.report_quick`. The size of all files is also included, as it often is an important criterium for downstream processing.
#'
#' @param folder Path to a folder with Envlogger data
#'
#' @seealso [is.report_quick()], [list_logs()]
#'
#' @return
#' A tibble with 3 columns containing the paths for all files inside `folder` that are (more or less) likely to be EnvLogger reports: `path` contains the full path for each file, `report` is a boolean vector containing the output from `is.report_quick` and `size` stores the size of each file (in bytes).
#'
#' @export
#'
#' @examples
#' folder <- "/Users/ruiseabra/Dropbox/RS/bio/datasets/temp_loggers/reports"
#' list_env_reports(folder)
list_env_reports <- function(folder) {
  # find all files in folder
  paths <- dir(folder, full.names = TRUE, recursive = TRUE)
  # discard logfiles
  paths <- paths[stringr::str_detect(basename(paths), "^log_", negate = TRUE)]
  # discard info files
  paths <- paths[stringr::str_detect(basename(paths), "^info", negate = TRUE)]
  # discard files without any spaces in their basename
  paths <- paths[stringr::str_detect(basename(paths), " ")]
  # discard non-csv files
  paths <- stringr::str_subset(stringr::str_to_lower(paths), ".csv$")

  # quickly check if they likely are EnvLogger report files
  paths <- tibble::tibble(
    path   = paths,
    report = purrr::map_lgl(path, is.report_quick),
    size   = file.size(path)
  )

  return(paths)
}

#' List all EnvLogger reports in a folder (quick) and grab their details
#'
#' @description
#' Given a path to a folder containing Envlogger data, list all files that likely are EnvLogger reports and grab their header details.
#' Read the description of [is.report_quick()] to better understand the limitations of the way files are quickly assessed as reports or not.
#'
#' @param folder Path to a folder with Envlogger data
#' @param min_size A numeric value (in bytes); only files larger than this will be retained; defaults to 1000 so that reports without any temperature data (or with only a few readings) are excluded.
#' @param max_size A numeric value (in bytes); only files smaller than this will be retained
#' @param parallel Logical, whether or not to use parallel computing (defaults to FALSE; based on the `future.apply` package)
#'
#' @seealso [is.report_quick()], [list_env_reports()]
#'
#' @return
#' A tibble with 3 columns containing the paths for all files inside `folder` that are (more or less) likely to be EnvLogger reports: `path` contains the full path for each file, `report` is a boolean vector containing the output from `is.report_quick` and `size` stores the size of each file (in bytes).
#'
#' @export
#'
#' @examples
#' folder <- "/Users/ruiseabra/Dropbox/RS/bio/datasets/temp_loggers/reports"
#' read_env_details(folder, parallel = TRUE)
read_env_details <- function(folder, min_size = 1000, max_size = 9999999999, parallel = FALSE) {
  # set up parallel computing
  if (parallel) {
    old_plan <- future::plan()
    old_handlers <- progressr::handlers("cli")

    future::plan("multisession")
    progressr::handlers(global = TRUE)

    message("  --> [i] parallel computing engaged")
    on.exit(future::plan(old_plan), add = TRUE)
    on.exit(if (is.null(old_handlers)) progressr::handlers("void") else progressr::handlers(old_handlers), add = TRUE)
  }

  paths <- folder %>%
    list_env_reports() %>%
    dplyr::filter(report, size >= min_size, size <= max_size)

  bar <- progressr::progressor(along = paths$path)
  heads <- future.apply::future_lapply(
    paths$path,
    function(x) {
      bar(message = "reading header details |")
      read_env_header(x)
    }
  )

  paths <- heads %>%
    dplyr::bind_rows() %>%
    dplyr::bind_cols(paths)

  return(paths)
}

#' Read Envlogger logfiles and list all serials
#'
#' @description
#' Given a vector of paths to Envlogger logfiles, read and list the serials included.
#'
#' @param paths A vector of paths to Envlogger logfiles
#' @param save Logical, whether to save the output to a `.csv` file in the same folder as the one where `path` is on.
#' @param colnames Logical, whether to include the names of columns in the `.csv` output file.
#'
#' @seealso [read_env_log()]
#'
#' @return
#' A tibble with one row for each unique serial name and 5 columns with relevant metadata.
#'
#' @export
#'
#' @examples
#' folder <- "/Users/ruiseabra/Dropbox/RS/bio/datasets/temp_loggers/reports"
#' paths  <- list_logs(folder)
#' read_deployment_info(path, save = FALSE)
read_deployment_info <- function(paths, save = TRUE, col_names = FALSE) {
  logs <- paths %>%
    purrr::map_dfr(read_env_log) %>%
    dplyr::filter(!is.na(id)) %>%
    dplyr::arrange(serial, time) %>%
    dplyr::group_by(serial, date = as.Date(time)) %>%
    dplyr::summarise(
      id   = dplyr::last(id),
      code = dplyr::last(code),
      int  = dplyr::last(samp_int_s),
      res  = dplyr::last(samp_res_c),
      .groups = "drop"
      ) %>%
    dplyr::arrange(id, date) %>%
    dplyr::mutate(
      action = "deploy",
      shore  = stringr::str_sub(id, 1, dplyr::if_else(nchar(id) == 9, 4, 5))
      ) %>%
    dplyr::select(date, action, serial, shore, id, code, int, res)

  # stopped here
  # read_env_log(paths[79]) and read_env_log(paths[82]) point to logfiles with just colnames
  # at first I deleted them, but then decided to undelete and stop checking, just in case the lack of data on those files could be related to them not having been fully downloaded
  # once I have internet I'll resume running the code above and resolving all issues that may arise permanently (i.e., editing or deleting files so that corrupted data is removed)

  downloads <- path %>%
    dirname() %>%
    dir(full.names = TRUE) %>%
    stringr::str_subset(pattern = "info", negate = TRUE) %>%
    stringr::str_subset(pattern = "log_", negate = TRUE)

  if (length(downloads)) {
    downloads <- downloads %>%
      basename() %>%
      stringr::str_to_lower() %>%
      stringr::str_split("-") %>%
      purrr::map_chr(1)

    log$action[log$serial %in% downloads] <- "download"
  }

  readr::write_csv(log, file.path(dirname(path), "info.csv"), col_names = col_names)

  log
}


#' Rearrange the content of an EnvloggerViewer folder into subfolders
#'
#' @description
#' Given a path to a folder from EnvloggerViewer, respective to only one day, reorganize the files into different subfolders by shore name.
#'
#' @param path The path to an EnvLoggerViewer daily folder
#' @param min_size Integer indicating the file size below which report files are rejected into a "check" folder (defaults to 700, which is just a few bytes greater than the size of a report with header but no data)
#'
#' @seealso [read_env_log()]
#'
#' @return
#' A tibble with one row for each unique serial name and 5 columns with relevant metadata.
#'
#' @export
#'
#' @examples
#' path <- "~/Library/CloudStorage/GoogleDrive-cctbonproject@gmail.com/My Drive/coastalwarming_29_rui/envlogger/2024-05-12"
#' subfolders(path)
subfolders <- function(path, min_size = 700) {
  # create a new folder to hold the reorganized files
  path_new <- stringr::str_c(path, "_sub")
  dir.create(path_new, showWarnings = FALSE)

  # list the available logfile(s) and determine the shores they correspond to
  logs <- tibble::tibble(
    path = dir(path, recursive = TRUE, full.names = TRUE, pattern = "log"),
    sh   = purrr::map(path, ~.x %>%
                        read_env_log() %>%
                        dplyr::pull(id) %>%
                        stringr::str_sub(1, 5) %>%
                        unique() %>%
                        sort())
  ) %>%
    tidyr::unnest(sh)

  # list all reports and determine their ids, shore code and file size
  # then, generate new file names (with id as prefix) and determine if there are files that need to be checked individually (because they lack id or file size is too small [likely without data])
  reps <- tibble::tibble(
    path   = dir(path, recursive = TRUE, full.names = TRUE, pattern = ".csv") %>%
      stringr::str_subset("log_", negate = TRUE),
    id     = purrr::map_chr(path, ~.x %>%
                   read_env() %>%
                   dplyr::pull(id)),
    fn_new = dplyr::if_else(is.na(id), basename(path), stringr::str_c(id, "-", basename(path))),
    sh     = stringr::str_sub(id, 1, 5),
    size   = file.size(path),
    check  = is.na(id) | size < min_size
  )

  # for each shore, create a new folder and copy the corresponding files into it (while also renaming the reports to the more complete file name convention)
  SH <- c(logs$sh, reps$sh) %>% unique() %>% sort()

  for (s in SH) {
    path_new2 <- file.path(path_new, s, basename(path))
    dir.create(path_new2, recursive = TRUE, showWarnings = FALSE)

    lo <- dplyr::filter(logs, sh == s)
    file.copy(lo$path, file.path(path_new2, basename(lo$path)))

    re <- dplyr::filter(reps, sh == s, !check)
    file.copy(re$path, file.path(path_new2, re$fn_new))
  }

  # if there are reports that were CHECK == TRUE, move them to a specific folder named "check"
  re <- dplyr::filter(reps, check)
  if (nrow(re)) {
    path_new3 <- file.path(path_new, "check", basename(path))
    dir.create(path_new3, recursive = TRUE, showWarnings = FALSE)

    lo <- logs$path %>% unique()
    file.copy(lo, file.path(path_new3, basename(lo)))

    file.copy(re$path, file.path(path_new3, re$fn_new))
  }
}
