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
#' @param col_names Logical, whether to include the names of columns in the `.csv` output file.
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
#' paths  <- rtools_example("/log")
#' read_deployment_info(paths)
read_deployment_info <- function(paths, col_names = FALSE) {
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

  downloads <- paths %>%
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

    logs$action[logs$serial %in% downloads] <- "download"
  }

  # readr::write_csv(logs, file.path(dirname(paths), "info.csv"), col_names = col_names)

  logs
}


#' Rearrange the content of an EnvloggerViewer folder into subfolders
#'
#' @description
#' Given a path to a folder from EnvloggerViewer, respective to only one day, reorganize the files into different subfolders by shore name.
#'
#' @param path The path to an EnvLoggerViewer daily folder
#' @param nchar Integer indicating the number of characters to be used to determine the shore name
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
#' path <- dirname(rtools_example("2023-08-31_ukhol")[1])
#' subfolders(path)
subfolders <- function(path, nchar = 5, min_size = 700) {
  # create a new folder to hold the reorganized files
  path_new <- stringr::str_c(path, "_sub")
  dir.create(path_new, showWarnings = FALSE)

  # list the available logfile(s) and determine the shores they correspond to
  logs <- tibble::tibble(
    path = dir(path, recursive = TRUE, full.names = TRUE, pattern = "log"),
    sh   = purrr::map(path, ~.x %>%
                        read_env_log() %>%
                        dplyr::pull(id) %>%
                        stringr::str_sub(1, nchar) %>%
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
    fn_new = dplyr::if_else(is.na(id) | stringr::str_detect(basename(path), id), basename(path), stringr::str_c(id, "-", basename(path))),
    sh     = stringr::str_sub(id, 1, nchar),
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
#
# path <- "~/RS Dropbox/Rui Seabra/RS/bio/datasets/temp_loggers/reports_tidy/_finalized_0.9/arber"
# check_folders <- function(path) {
#   sh <- basename(path)
#
#   paths <- dir(path, recursive = TRUE, full.names = TRUE)
#
#   # is this a shore without any logger data yet (just a logfile)
#   is.length.1 <- length(paths) == 1
#   is.all.log  <- all(stringr::str_detect(basename(paths), "^log_"))
#   if (is.length.1 & !is.all.log) stop(paste0(sh, ": only one file in the folder, but it isn't a logfile"))
#   if (is.length.1 & is.all.log) {
#     log <- read_env_log(paths)
#     message()
#
#
# }



#' Quickly perform simple checks on an EnvloggerViewer folder
#'
#' @description
#' Given a path to a folder from EnvloggerViewer, read data and present several pieces of information that help assessing if there are any issues with the files and data that must be addressed.
#'
#' @inheritParams subfolders
#'
#' @seealso [read_env_log()]
#'
#' @return
#' Prints diagnostic stats and returns invisibly a list with $dat and $pbox with $dat being a tibble with stats about each valid report found in the folder (i.e., reports with more than 2 lines of data and custom names that aren't empty) and $box being a boxplot of the temperatures recorded by each id. To plot the plot, simply run .$box.
#'
#' @export
#'
#' @examples
#' PATH <- dirname(rtools_example("2023-08-31_ukhol")[1])
#' out <- checkfolder(PATH)
#' out$dat # tibble with extra details
#' out$box # boxplot showing the spread of temperatures recorded by each id
checkfolder <- function(PATH, nchar = 5, min_size = 700) {
  # reports
  dat <- tibble::tibble(
    path = dir(PATH, recursive = TRUE, full.names = TRUE, pattern = ".csv") %>%
      stringr::str_subset("log_", negate = TRUE),
    data = purrr::map(path, ~ read_env(.x)),

    size = file.size(path),
    nrow = purrr::map_dbl(data, "nrow"),

    id   = purrr::map_chr(data, "id"),
    sh   = stringr::str_sub(id, 1, nchar),
    mic  = stringr::str_sub(id, start = nchar + 1),
    )

  dat <- split(dat, is.na(dat$id) | dat$nrow < 3)

  dat_na <- dat$`TRUE`

  dat <- dat$`FALSE` %>%
    dplyr::mutate(
      data = purrr::map(data, ~.x$data[[1]]),
      t0   = purrr::map(data, ~dplyr::first(.x$t)) %>% unlist() %>% as.POSIXct(tz = "UTC"),
      t1   = purrr::map(data, ~dplyr::last( .x$t)) %>% unlist() %>% as.POSIXct(tz = "UTC"),
      days = purrr::map2_dbl(t0, t1, ~difftime(.y, .x, units = "days")) %>% floor(),
      min  = purrr::map_dbl(data, ~min( .x$temp)),
      avg  = purrr::map_dbl(data, ~mean(.x$temp)),
      max  = purrr::map_dbl(data, ~max( .x$temp))
    ) %>%
    dplyr::arrange(sh, mic)

  # logfiles
  logs <- dir(PATH, recursive = TRUE, full.names = TRUE, pattern = "log")
  if (!length(logs)) {
    log_msg <- "file missing"
  } else {
    SH <- logs %>%
      purrr::map(~.x %>%
                   read_env_log() %>%
                   dplyr::select(id) %>%
                   tidyr::drop_na() %>%
                   dplyr::pull(id) %>%
                   stringr::str_sub(1, nchar)) %>%
      unlist() %>%
      unique()

    if (unique(dat$sh) %in% SH %>% all()) {
      # logfile has entries for the all shores for which there are reports with data (only assessed using shore names)
      log_msg <- "ok"
    } else {
      log_msg <- "shore missing"
    }
  }

  # plot
  box <- NULL
  if (plot) {
    # dat %>%
    #   dplyr::select(id, temp1 = min, temp2 = avg, temp3 = max) %>%
    #   tidyr::pivot_longer(
    #     cols = dplyr::starts_with("temp"),
    #     names_to = "x",
    #     values_to = "y"
    #     ) %>%
    #   dplyr::mutate(x = x %>%
    #                   stringr::str_remove("temp") %>%
    #                   as.numeric()) %>%
    # ggplot2::ggplot() +
    #   ggplot2::geom_line(ggplot2::aes(x, y, group = id), alpha = 0.2) +
    #   ggplot2::geom_point(ggplot2::aes(x, y, group = id), alpha = 0.2, size = 3) +
    #   ggplot2::xlab("") + ggplot2::ylab("") +
    #   ggplot2::scale_x_continuous(
    #     breaks = 1:3,
    #     labels = c("min", "avg", "max")
    # )

    box <- dat %>%
      tidyr::unnest(cols = data) %>%
      dplyr::select(id, temp) %>%
      ggplot2::ggplot() +
      ggplot2::geom_boxplot(ggplot2::aes(id, temp)) +
      ggplot2::xlab("") + ggplot2::ylab("") +
      ggplot2::coord_flip()
  }

  # eval
  msg <- paste0(
    " >>> checking folder: ", basename(PATH), "\n",
    "\n",
    " --- logfile ........... ", log_msg, "\n",
    " --- total reports ..... ", nrow(dat_na) + nrow(dat), "\n",
    " --- empty reports ..... ", nrow(dat_na), "\n",
    "\n",
    " -------------------------------", "\n",
    " >>> stats for valid reports <<<", "\n",
    "\n",
    " --> n ................. ", nrow(dat), "\n",
    " --> min t0 ............ ", min(dat$t0), "\n",
    " --> max t1 ............ ", max(dat$t1), "\n",
    "\n",
    " --> min span .......... ", min(dat$days), "\n",
    " --> max span .......... ", max(dat$days), "\n",
    " --> median span ....... ", median(dat$days), "\n",
    "\n",
    " --> min temp .......... ", min(dat$min), "\n",
    " --> median min temp ... ", median(dat$min), "\n",
    "\n",
    " --> max temp .......... ", max(dat$max), "\n",
    " --> median max temp ... ", median(dat$max), "\n",
    "\n",
    " --------------", "\n",
    " >>> browse <<<", "\n")
    cat(msg)
    print(dplyr::select(dat, sh, mic, days, min, avg, max), n = nrow(dat))

  # return
  invisible(list(dat = dat, box = box))
}

