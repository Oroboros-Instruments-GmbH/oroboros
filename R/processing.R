# R/processing.R
# Core folder scan + DLD8 processing logic ported from the app.

source("R/constants.R")

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

normalize_metadata_value_R <- function(value) {
  if (is.null(value)) return("")
  if (is.numeric(value)) return(as.character(as.integer(value)))
  trimws(as.character(value))
}

extract_protocol_id_R <- function(protocol_str) {
  if (is.null(protocol_str)) return(NA_character_)
  s <- as.character(protocol_str)
  if (length(s) == 0 || is.na(s)) return(NA_character_)
  m <- regexpr("D\\d+", s, perl = TRUE)
  if (m[1] == -1) return(NA_character_)
  regmatches(s, m)[1]
}

normalize_mark_label_R <- function(s) {
  if (is.null(s)) return("")
  s <- as.character(s)
  if (length(s) == 0 || any(is.na(s))) return("")
  if (length(s) > 1) s <- s[1]
  s <- trimws(s)
  if (!nzchar(s)) return("")
  s <- sub("^\\d+", "", s)
  s <- trimws(s)
  while (nchar(s) > 0 && !grepl("[[:alnum:]]$", s)) {
    s <- substr(s, 1, nchar(s) - 1)
  }
  trimws(s)
}

resolve_marker_text_only <- function(mark_name, marks) {
  if (is.null(mark_name)) return(NA_character_)
  target <- trimws(as.character(mark_name))
  if (!nzchar(target)) return(NA_character_)

  for (mk in marks) {
    mk_name <- mk$markName
    if (is.null(mk_name) || is.na(mk_name)) next
    if (identical(as.character(mk_name), target)) return(mk_name)
  }

  target_norm <- tolower(target)
  for (mk in marks) {
    mk_name <- mk$markName
    if (is.null(mk_name) || is.na(mk_name)) next
    mk_norm <- tolower(trimws(as.character(mk_name)))
    if (length(mk_norm) == 0 || any(is.na(mk_norm))) next
    if (isTRUE(mk_norm == target_norm)) return(mk_name)
  }

  target_norm2 <- tolower(normalize_mark_label_R(target))
  if (nzchar(target_norm2)) {
    for (mk in marks) {
      mk_name <- mk$markName
      if (is.null(mk_name) || is.na(mk_name)) next
      mk_norm2 <- tolower(normalize_mark_label_R(mk_name))
      if (nzchar(mk_norm2) && mk_norm2 == target_norm2) return(mk_name)
    }
  }

  target
}

marker_exists <- function(mark_name, marks) {
  if (is.null(mark_name)) return(FALSE)
  target <- trimws(as.character(mark_name))
  if (!nzchar(target)) return(FALSE)

  for (mk in marks) {
    mk_name <- mk$markName
    if (is.null(mk_name) || is.na(mk_name)) next
    if (identical(as.character(mk_name), target)) return(TRUE)
  }

  target_norm <- tolower(target)
  for (mk in marks) {
    mk_name <- mk$markName
    if (is.null(mk_name) || is.na(mk_name)) next
    mk_norm <- tolower(trimws(as.character(mk_name)))
    if (length(mk_norm) == 0 || any(is.na(mk_norm))) next
    if (isTRUE(mk_norm == target_norm)) return(TRUE)
  }

  target_norm2 <- tolower(normalize_mark_label_R(target))
  if (nzchar(target_norm2)) {
    for (mk in marks) {
      mk_name <- mk$markName
      if (is.null(mk_name) || is.na(mk_name)) next
      mk_norm2 <- tolower(normalize_mark_label_R(mk_name))
      if (nzchar(mk_norm2) && mk_norm2 == target_norm2) return(TRUE)
    }
  }

  FALSE
}

empty_excluded_chambers_df <- function() {
  data.frame(
    rel_path = character(),
    chamber = character(),
    reason = character(),
    stringsAsFactors = FALSE
  )
}

normalize_excluded_chambers_df <- function(x) {
  empty <- empty_excluded_chambers_df()
  if (is.null(x) || length(x) == 0) return(empty)
  if (!is.data.frame(x)) {
    x <- as.data.frame(x, stringsAsFactors = FALSE)
  }
  if (nrow(x) == 0) return(empty)
  for (nm in names(empty)) {
    if (!nm %in% names(x)) x[[nm]] <- empty[[nm]]
  }
  x <- x[, names(empty), drop = FALSE]
  x$rel_path <- as.character(x$rel_path)
  x$chamber <- as.character(x$chamber)
  x$reason <- as.character(x$reason)
  unique(x)
}

is_non_experimental_protocol <- function(protocol_name) {
  protocol_name <- trimws(as.character(protocol_name %||% ""))
  nzchar(protocol_name) && (
    protocol_name %in% EXCLUDED_PROTOCOLS ||
      grepl("calibration", protocol_name, ignore.case = TRUE)
  )
}

classify_project_file <- function(rel, dld8_json, exclude_non_experimental = TRUE,
                                  verbose = FALSE) {
  empty_excluded <- empty_excluded_chambers_df()
  if (is.null(dld8_json) || !is.list(dld8_json)) {
    return(list(status = "parse_failure", excluded_chambers = empty_excluded))
  }

  has_metadata <- FALSE
  has_allowed_chamber <- FALSE
  has_allowed_marks <- FALSE
  excluded_chambers <- empty_excluded

  for (ch in c("chamber_a", "chamber_b")) {
    md <- get_sample_metadata_from_json(ch, dld8_json)
    if (length(md) == 0) next
    has_metadata <- TRUE

    protocol <- md$protocol %||% ""
    if (isTRUE(exclude_non_experimental) && is_non_experimental_protocol(protocol)) {
      excluded_chambers <- rbind(
        excluded_chambers,
        data.frame(
          rel_path = rel,
          chamber = ch,
          reason = "protocol",
          stringsAsFactors = FALSE
        )
      )
      next
    }

    has_allowed_chamber <- TRUE
    marks <- get_marks_from_chamber(ch, dld8_json, verbose = verbose)
    if (length(marks) > 0) {
      has_allowed_marks <- TRUE
    }
  }

  excluded_chambers <- normalize_excluded_chambers_df(excluded_chambers)

  if (!has_metadata) {
    return(list(status = "excluded_by_marks", excluded_chambers = empty_excluded))
  }
  if (isTRUE(exclude_non_experimental) && !has_allowed_chamber) {
    return(list(status = "excluded_by_protocol", excluded_chambers = empty_excluded))
  }
  if (!has_allowed_marks) {
    return(list(status = "excluded_by_marks", excluded_chambers = excluded_chambers))
  }

  list(status = "include", excluded_chambers = excluded_chambers)
}

get_project_chambers <- function(project, rel, include_non_experimental = FALSE) {
  chambers <- c("chamber_a", "chamber_b")
  if (isTRUE(include_non_experimental)) return(chambers)

  excluded_chambers <- normalize_excluded_chambers_df(project$excluded_chambers)
  if (nrow(excluded_chambers) == 0) return(chambers)

  setdiff(chambers, excluded_chambers$chamber[excluded_chambers$rel_path == rel])
}

#' Debug: Print Present Marks Per File/Chamber
#'
#' Prints the mark names found in each file and chamber.
#'
#' @param project A `dld8_project` object.
#' @param include_non_experimental Logical; include non-experimental files.
#' @export
debug_print_present_marks <- function(project, include_non_experimental = FALSE) {
  rel_files <- project$dld8_files
  if (isTRUE(include_non_experimental)) {
    rel_files <- c(rel_files, project$excluded_dld8_files)
  }
  for (rel in rel_files) {
    dld8_json <- get_dld8(project, rel)
    if (is.null(dld8_json)) {
      message("[skip] parse failed: ", rel)
      next
    }
    for (ch in get_project_chambers(project, rel, include_non_experimental = include_non_experimental)) {
      marks <- get_marks_from_chamber(ch, dld8_json, verbose = FALSE)
      names_vec <- character()
      if (length(marks) > 0) {
        names_vec <- vapply(marks, function(mk) mk$markName %||% NA_character_, character(1))
        names_vec <- names_vec[!is.na(names_vec) & nzchar(names_vec)]
      }
      message(rel, " | ", ch, " | marks: ", paste(names_vec, collapse = ", "))
    }
  }
}

#' Prompt for a Folder Path (Interactive)
#'
#' Convenience helper for interactive sessions to capture a folder path.
#'
#' @param prompt Character prompt displayed in the console.
#' @return Character string entered by the user.
#' @export
prompt_folder_path <- function(prompt = "Enter folder path: ") {
  readline(prompt = prompt)
}

filter_keys_recursive <- function(x, key_to_ignore = "&") {
  if (is.list(x)) {
    if (!is.null(names(x))) {
      keep <- names(x) != key_to_ignore
      x <- x[keep]
    }
    return(lapply(x, filter_keys_recursive, key_to_ignore = key_to_ignore))
  }
  x
}

norm_rel <- function(p) {
  p <- gsub("\\\\", "/", p)
  p <- sub("^\\./", "", p)
  p
}

rel_path <- function(abs_path, base) {
  abs_norm <- normalizePath(abs_path, winslash = "/", mustWork = FALSE)
  base_norm <- normalizePath(base, winslash = "/", mustWork = FALSE)
  prefix <- paste0(base_norm, "/")
  if (startsWith(abs_norm, prefix)) {
    norm_rel(sub(prefix, "", abs_norm, fixed = TRUE))
  } else {
    norm_rel(abs_norm)
  }
}

safe_get <- function(x, path, default = NULL) {
  cur <- x
  for (p in path) {
    if (!is.list(cur) || is.null(cur[[p]])) {
      return(default)
    }
    cur <- cur[[p]]
  }
  cur
}

build_file_cache <- function(source_folder, rel_files) {
  if (length(rel_files) == 0) {
    return(data.frame(rel_path = character(), mtime = numeric(), size = numeric(), stringsAsFactors = FALSE))
  }
  rows <- list()
  for (rel in rel_files) {
    abs_path <- file.path(source_folder, rel)
    info <- tryCatch(file.info(abs_path), error = function(e) NULL)
    if (is.null(info) || is.na(info$size)) next
    rows <- c(rows, list(data.frame(
      rel_path = rel,
      mtime = as.numeric(info$mtime),
      size = as.numeric(info$size),
      stringsAsFactors = FALSE
    )))
  }
  if (length(rows) == 0) {
    return(data.frame(rel_path = character(), mtime = numeric(), size = numeric(), stringsAsFactors = FALSE))
  }
  do.call(rbind, rows)
}

#' Retrieve parsed JSON for a project file, caching in the project
#'
#' The project stores an environment in `project$parsed_dld8` so that
#' subsequent calls reuse the same object rather than reparsing from disk.
#' The environment is created by `new_dld8_project()` and populated by
#' `create_dld8_project()`; any function that needs the contents of a
#' `.dld8` file should call `get_dld8(project, rel)` instead of
#' `read_dld8_json()` directly.
#'
get_dld8 <- function(project, rel) {
  env <- project$parsed_dld8
  abs_path <- file.path(project$source_folder, rel)

  # If we already parsed this file, check whether it is stale compared to file_cache.
  if (is.environment(env) && exists(rel, envir = env, inherits = FALSE)) {
    cached_json <- env[[rel]]
    stale <- FALSE

    if (!is.null(project$file_cache) && nrow(project$file_cache) > 0) {
      idx <- which(project$file_cache$rel_path == rel)
      if (length(idx) == 1) {
        info <- tryCatch(file.info(abs_path), error = function(e) NULL)
        if (!is.null(info) && !is.na(info$size)) {
          stale <- !(isTRUE(all.equal(as.numeric(info$mtime), project$file_cache$mtime[idx])) &&
                     isTRUE(all.equal(as.numeric(info$size), project$file_cache$size[idx])))
        } else {
          stale <- TRUE
        }
      } else {
        stale <- TRUE
      }
    }

    if (!stale) {
      return(cached_json)
    }
  }

  dld8_json <- tryCatch(read_dld8_json(abs_path), error = function(e) NULL)
  if (is.environment(env)) {
    env[[rel]] <- dld8_json
  } else {
    # fall back to list if older project structure
    project$parsed_dld8 <- list()
    project$parsed_dld8[[rel]] <- dld8_json
  }
  dld8_json
}

get_latest_run_data <- function(dld8_json) {
  if (!is.list(dld8_json)) return(list())
  if (!is.null(dld8_json$run)) return(dld8_json$run)

  keys <- names(dld8_json)
  if (is.null(keys)) return(list())
  matches <- regmatches(keys, regexec("^runsToSave_(\\d+)$", keys))
  idx <- vapply(matches, function(m) {
    if (length(m) == 2) as.integer(m[2]) else NA_integer_
  }, integer(1))
  if (all(is.na(idx))) return(list())
  latest_key <- keys[which.max(idx)]
  run_data <- dld8_json[[latest_key]]
  if (is.null(run_data)) list() else run_data
}

#' Read a DLD8 File (BSON)
#'
#' Reads a `.dld8` file into a list and removes problematic keys.
#' Requires `mongolite` (preferred) or `bson`.
#'
#' @param path Path to a `.dld8` file.
#' @return List containing the first BSON document.
#' @keywords internal
read_dld8_json <- function(path) {
  if (!file.exists(path)) {
    stop(paste("File not found:", path))
  }
  raw <- readBin(path, "raw", n = file.info(path)$size)
  docs <- NULL
  if (requireNamespace("mongolite", quietly = TRUE) &&
      "read_bson" %in% getNamespaceExports("mongolite")) {
    sink_con <- file("NUL", open = "w")
    on.exit(try(close(sink_con), silent = TRUE), add = TRUE)
    sink(sink_con)
    sink(sink_con, type = "message")
    docs <- tryCatch(mongolite::read_bson(path, simplify = FALSE), error = function(e) e)
    sink(type = "message")
    sink()
    if (inherits(docs, "error")) {
      stop(docs$message)
    }
  } else if (requireNamespace("bson", quietly = TRUE)) {
    docs <- bson::bson_to_list(raw)
  } else if (requireNamespace("mongolite", quietly = TRUE)) {
    stop("mongolite is installed but does not export read_bson; please install/update mongolite.")
  } else {
    stop("Install the 'mongolite' (preferred) or 'bson' package to read .dld8 files.")
  }
  filtered <- lapply(docs, filter_keys_recursive, key_to_ignore = "&")
  if (length(filtered) < 1) list() else filtered[[1]]
}

get_run_start_recording_time <- function(dld8_json) {
  run_data <- get_latest_run_data(dld8_json)
  ts <- safe_get(run_data, c("timeStampRecordingStart", "value"), default = NA_real_)
  if (is.na(ts)) return(NA)
  as.POSIXct(ts / 1e6, origin = "1970-01-01", tz = "")
}

get_chamber_key_name <- function(chamber_id) {
  if (identical(chamber_id, "chamber_a") || identical(chamber_id, "A") || identical(chamber_id, 1)) {
    "chamber1"
  } else {
    "chamber2"
  }
}

get_protocol_name <- function(dld8_json, chamber_id) {
  run_data <- get_latest_run_data(dld8_json)
  chamber_key <- get_chamber_key_name(chamber_id)
  safe_get(run_data, c(chamber_key, "protocol", "displayName", "value"), default = "")
}

get_air_calibration_datetime <- function(dld8_json, chamber_id) {
  run_data <- get_latest_run_data(dld8_json)
  chamber_key <- get_chamber_key_name(chamber_id)
  ts <- safe_get(
    run_data,
    c(chamber_key, "oxygenCalibration", "airCalibrationTimeStamp", "value"),
    default = NA_real_
  )
  if (is.na(ts)) return(NA)
  as.POSIXct(ts / 1e6, origin = "1970-01-01", tz = "")
}

air_calibration_is_same_day <- function(dld8_json) {
  recording_time <- get_run_start_recording_time(dld8_json)
  if (is.na(recording_time)) return(FALSE)
  for (chamber_id in c("chamber_a", "chamber_b")) {
    protocol <- get_protocol_name(dld8_json, chamber_id)
    air_time <- get_air_calibration_datetime(dld8_json, chamber_id)
    if (is.na(air_time)) return(FALSE)
    is_cleaning <- grepl("cleaning", protocol, ignore.case = TRUE)
    no_protocol <- trimws(protocol) == ""
    is_ok <- no_protocol || is_cleaning || as.Date(air_time) == as.Date(recording_time)
    if (!is_ok) return(FALSE)
  }
  TRUE
}

oroboros_temp_is_ok <- function(dld8_json, temp) {
  run_data <- get_latest_run_data(dld8_json)
  run_temp <- safe_get(run_data, c("measurementConfig", "temperature", "value"), default = NA_real_)
  if (is.na(run_temp)) return(FALSE)
  isTRUE(all.equal(as.numeric(run_temp), as.numeric(temp)))
}

#' Extract Sample Metadata From DLD8 JSON
#'
#' @param chamber_id Character: `"chamber_a"` or `"chamber_b"`.
#' @param dld8_json Parsed DLD8 list (from `read_dld8_json()`).
#' @return List of metadata for the chamber.
#' @keywords internal
get_sample_metadata_from_json <- function(chamber_id, dld8_json) {
  chamber_key <- if (identical(chamber_id, "chamber_a")) "chamber1" else "chamber2"
  chamber_key_settings <- if (identical(chamber_id, "chamber_a")) "settingsChamber1" else "settingsChamber2"
  chamber_key_protocol <- if (identical(chamber_id, "chamber_a")) "protocolChamber1" else "protocolChamber2"

  run_data <- get_latest_run_data(dld8_json)
  if (length(run_data) == 0) return(list())

  info <- safe_get(run_data, c(chamber_key, "sampleInfo"), default = list())
  background <- safe_get(run_data, c(chamber_key, "oxygenBackgroundCorrection"), default = list())

  measurement_config <- safe_get(run_data, c("measurementConfig"), default = list())
  settings <- safe_get(measurement_config, c(chamber_key_settings, "chamberVolumeInMilliliter"), default = list())
  protocol <- safe_get(measurement_config, c(chamber_key_protocol, "displayName"), default = list())

  list(
    metaData = list(
      sampleType = safe_get(info, c("sampleType", "value")),
      cohort = safe_get(info, c("cohort", "value")),
      sampleCode = safe_get(info, c("sampleCode", "value")),
      sampleNumber = safe_get(info, c("sampleNumber", "value")),
      subsampleNumber = safe_get(info, c("subsampleNumber", "value")),
      samplePreparation = safe_get(info, c("samplePreparation", "value")),
      experimentCode = safe_get(info, c("experimentCode", "value"))
    ),
    protocol = safe_get(protocol, c("value")),
    operator = safe_get(run_data, c("userInfo", "userName", "value")),
    oxygraph = safe_get(run_data, c("oxygraphInfo", "serialNumber", "value")),
    sensor = safe_get(run_data, c(chamber_key, "oxygenCalibration", "airSensorNumber", "value")),
    a0 = safe_get(background, c("a0", "value")),
    b0 = safe_get(background, c("b0", "value")),
    R1 = safe_get(run_data, c(chamber_key, "oxygenCalibration", "r1", "value")),
    backgroundCorrection = list(
      a0 = safe_get(background, c("a0", "value")),
      b0 = safe_get(background, c("b0", "value"))
    ),
    sampleQuantity = list(
      amount = safe_get(info, c("normalizations", "normalizations_0", "amount", "value")),
      volume = safe_get(settings, c("value"))
    ),
    chamberVolume = safe_get(settings, c("value")),
    calibrationStatus = air_calibration_is_same_day(dld8_json),
    temperatureStatus = oroboros_temp_is_ok(dld8_json, EXPECTED_OROBOROS_TEMPERATURE)
  )
}

#' Extract Marks From a Chamber
#'
#' @param chamber_id Character: `"chamber_a"` or `"chamber_b"`.
#' @param dld8_json Parsed DLD8 list (from `read_dld8_json()`).
#' @param verbose Logical; if `TRUE` print diagnostic messages while parsing.
#' @return List of marks.
#' @keywords internal
get_marks_from_chamber <- function(chamber_id, dld8_json, verbose = FALSE) {
  run_data <- get_latest_run_data(dld8_json)
  if (length(run_data) == 0) return(list())

  chamber_str <- if (identical(chamber_id, "chamber_a")) "chamber1" else "chamber2"
  marks_root <- safe_get(run_data, c(chamber_str, "analyses", "analyses_0", "marks"), default = list())
  if (!is.list(marks_root) || length(marks_root) == 0) return(list())

  marks <- list()
  for (value in marks_root) {
    if (isTRUE(verbose)) {
      message("[debug] processing raw mark entry", paste(capture.output(str(value)), collapse = " "))
    }
    if (!is.list(value) || is.null(value$markName)) next
    mark <- list(
      markName = safe_get(value, c("markName", "value")),
      timeStart = safe_get(value, c("timeStart", "value")),
      timeEnd = safe_get(value, c("timeEnd", "value")),
      protocolStepIdx = safe_get(value, c("protocolStepIdx", "value")),
      respiratoryState = safe_get(value, c("respiratoryState", "value")),
      markId = safe_get(value, c("markId", "value")),
      nPoints = safe_get(value, c("nPoints", "value")),
      calibrationVolume = safe_get(value, c("calibrationVolume", "value")),
      calibrationConcentration = safe_get(value, c("calibrationConcentration", "value")),
      cumulativeAddedVolume = safe_get(value, c("cumulativeAddedVolume", "value")),
      isProtocolMark = safe_get(value, c("isProtocolMark", "value")),
      isReferenceMark = safe_get(value, c("isReferenceMark", "value")),
      isBaselineMark = safe_get(value, c("isBaselineMark", "value")),
      concentrationCorrectionFactor = safe_get(value, c("concentrationCorrectionFactor", "value")),
      values = list()
    )
    mark_values <- safe_get(value, c("markValue"), default = list())
    if (isTRUE(verbose) && (!is.list(mark_values) || length(mark_values) == 0)) {
      message("[debug]   no markValue list for mark", mk$markName, "; value structure:",
              paste(capture.output(str(value)), collapse = " "))
    }
    if (is.list(mark_values) && length(mark_values) > 0) {
      for (mv in mark_values) {
        mark$values <- c(mark$values, list(list(
          datastructureInfo = safe_get(mv, c("datastructureInfo")),
          average = safe_get(mv, c("average", "value")),
          median = safe_get(mv, c("median", "value")),
          standardDeviation = safe_get(mv, c("standardDeviation", "value")),
          outlierIndex = safe_get(mv, c("outlierIndex", "value")),
          range = safe_get(mv, c("range", "value")),
          maximum = safe_get(mv, c("maximum", "value")),
          minimum = safe_get(mv, c("minimum", "value"))
        )))
      }
    }
    marks <- c(marks, list(mark))
  }
  marks
}

#' Scan a Folder for `.dld8` Files
#'
#' @param source_folder Folder to scan.
#' @return Character vector of relative paths under `source_folder`.
#' @export
scan_dld8_files <- function(source_folder) {
  files <- list.files(source_folder, pattern = "\\.dld8$", recursive = TRUE,
                      full.names = TRUE, ignore.case = TRUE)
  if (length(files) == 0) return(character())
  files <- sort(files)
  vapply(files, function(p) rel_path(p, source_folder), character(1))
}

#' Construct a DLD8 Project Object
#'
#' @param source_folder Root folder containing `.dld8` files.
#' @param rel_files Relative paths included in the project.
#' @param excluded_files Relative paths excluded in the project.
#' @param exclude_non_experimental Logical; whether non-experimental chambers/files were filtered.
#' @param parse_errors Optional list of parse errors.
#' @param excluded_chambers Optional data frame of chamber-level exclusions.
#' @return An object of class `dld8_project`.
#' @keywords internal
new_dld8_project <- function(source_folder, rel_files, excluded_files, exclude_non_experimental,
                             parse_errors = list(), parsed_dld8 = new.env(parent = emptyenv()),
                             excluded_chambers = empty_excluded_chambers_df()) {
  # `parsed_dld8` is a named list (or environment) containing the result of
  # `read_dld8_json()` for each file included in the project.  it is used
  # by the various `extract_*`/`validate_*` helpers so they don't re‑read
  # every file on every call.
  file_cache <- build_file_cache(source_folder, c(rel_files, excluded_files))
  structure(
    list(
      source_folder = source_folder,
      dld8_files = rel_files,
      excluded_dld8_files = excluded_files,
      excluded_chambers = normalize_excluded_chambers_df(excluded_chambers),
      parse_errors = parse_errors,
      file_cache = file_cache,
      parsed_dld8 = parsed_dld8,
      marker_overrides = data.frame(
        rel_path = character(),
        chamber = character(),
        baseline = character(),
        reference = character(),
        stringsAsFactors = FALSE
      ),
      protocol_overrides = data.frame(
        protocol = character(),
        baseline = character(),
        reference = character(),
        stringsAsFactors = FALSE
      ),
      settings = list(
        copy_mode = "reference",
        exclude_non_experimental = exclude_non_experimental
      )
    ),
    class = "dld8_project"
  )
}

#' Create a DLD8 Project (In Memory)
#'
#' Scans a folder, parses files, and returns a `dld8_project` object.
#'
#' @param source_folder Folder containing `.dld8` files.
#' @param exclude_non_experimental Logical; filter out non-experimental chambers/files.
#' @param verbose Logical; if `TRUE` show parsing diagnostics from `get_marks_from_chamber`.
#' @return A `dld8_project` object.
#' @export
create_dld8_project <- function(source_folder, exclude_non_experimental = TRUE, verbose = FALSE) {
  source_folder <- normalizePath(source_folder, winslash = "/", mustWork = FALSE)

  rel_files <- scan_dld8_files(source_folder)
  if (length(rel_files) == 0) {
    return(new_dld8_project(source_folder, character(), character(), exclude_non_experimental))
  }

  dld8_cache <- list()
  parse_failures <- character()
  parse_errors <- list()
  for (rel in rel_files) {
    abs_path <- file.path(source_folder, rel)
    dld8_json <- tryCatch(read_dld8_json(abs_path), error = function(e) e)
    if (is.null(dld8_json)) {
      parse_failures <- c(parse_failures, rel)
    } else if (inherits(dld8_json, "error")) {
      parse_failures <- c(parse_failures, rel)
      parse_errors[[rel]] <- conditionMessage(dld8_json)
    } else {
      dld8_cache[[rel]] <- dld8_json
    }
  }

  # convert parsed list to environment for mutable caching
  parsed_env <- new.env(parent = emptyenv())
  for (rel in names(dld8_cache)) parsed_env[[rel]] <- dld8_cache[[rel]]

  included <- character()
  excluded_by_protocol <- character()
  excluded_by_marks <- character()
  excluded_chambers <- empty_excluded_chambers_df()
  for (rel in names(dld8_cache)) {
    classification <- classify_project_file(
      rel,
      dld8_cache[[rel]],
      exclude_non_experimental = exclude_non_experimental,
      verbose = verbose
    )

    if (identical(classification$status, "include")) {
      included <- c(included, rel)
      if (nrow(classification$excluded_chambers) > 0) {
        excluded_chambers <- rbind(excluded_chambers, classification$excluded_chambers)
      }
    } else if (identical(classification$status, "excluded_by_protocol")) {
      excluded_by_protocol <- c(excluded_by_protocol, rel)
    } else if (identical(classification$status, "excluded_by_marks")) {
      excluded_by_marks <- c(excluded_by_marks, rel)
    }
  }

  excluded <- c(excluded_by_protocol, excluded_by_marks, parse_failures)

  new_dld8_project(source_folder, included, excluded, exclude_non_experimental,
                   parse_errors = parse_errors,
                   parsed_dld8 = parsed_env,
                   excluded_chambers = excluded_chambers)
}

#' Extract Metadata Data Frame
#'
#' @param project A `dld8_project` object.
#' @param include_non_experimental Logical; include non-experimental files.
#' @return Data frame of metadata (one row per file/chamber).
#' @export
extract_metadata_df <- function(project, include_non_experimental = FALSE) {
  source_folder <- project$source_folder
  rel_files <- project$dld8_files
  if (isTRUE(include_non_experimental)) {
    rel_files <- c(rel_files, project$excluded_dld8_files)
  }
  if (length(rel_files) == 0) return(data.frame())

  rows <- list()
  for (rel in rel_files) {
    dld8_json <- get_dld8(project, rel)
    if (is.null(dld8_json)) next
    for (ch in get_project_chambers(project, rel, include_non_experimental = include_non_experimental)) {
      md <- get_sample_metadata_from_json(ch, dld8_json)
      if (length(md) == 0) next
      row <- data.frame(
        rel_path = rel,
        chamber = ch,
        protocol = md$protocol %||% NA_character_,
        operator = md$operator %||% NA_character_,
        oxygraph = md$oxygraph %||% NA_character_,
        sensor = md$sensor %||% NA_character_,
        sampleType = md$metaData$sampleType %||% NA_character_,
        cohort = md$metaData$cohort %||% NA_character_,
        sampleCode = md$metaData$sampleCode %||% NA_character_,
        sampleNumber = md$metaData$sampleNumber %||% NA_character_,
        subsampleNumber = md$metaData$subsampleNumber %||% NA_character_,
        samplePreparation = md$metaData$samplePreparation %||% NA_character_,
        experimentCode = md$metaData$experimentCode %||% NA_character_,
        chamberVolume = md$chamberVolume %||% NA_real_,
        a0 = md$a0 %||% NA_real_,
        b0 = md$b0 %||% NA_real_,
        R1 = md$R1 %||% NA_real_,
        calibrationStatus = md$calibrationStatus %||% NA,
        temperatureStatus = md$temperatureStatus %||% NA,
        stringsAsFactors = FALSE
      )
      rows <- c(rows, list(row))
    }
  }
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

#' Extract Marks Data Frame
#'
#' @param project A `dld8_project` object.
#' @param include_non_experimental Logical; include non-experimental files.
#' @return Data frame of marks (one row per mark).
#' @export
extract_marks_df <- function(project, include_non_experimental = FALSE) {
  source_folder <- project$source_folder
  rel_files <- project$dld8_files
  if (isTRUE(include_non_experimental)) {
    rel_files <- c(rel_files, project$excluded_dld8_files)
  }
  if (length(rel_files) == 0) return(data.frame())

  rows <- list()
  for (rel in rel_files) {
    dld8_json <- get_dld8(project, rel)
    if (is.null(dld8_json)) next
    for (ch in get_project_chambers(project, rel, include_non_experimental = include_non_experimental)) {
      marks <- get_marks_from_chamber(ch, dld8_json, verbose = FALSE)
      if (length(marks) == 0) next
      for (mk in marks) {
        row <- data.frame(
          rel_path = rel,
          chamber = ch,
          markName = mk$markName %||% NA_character_,
          timeStart = mk$timeStart %||% NA_real_,
          timeEnd = mk$timeEnd %||% NA_real_,
          protocolStepIdx = mk$protocolStepIdx %||% NA_real_,
          respiratoryState = mk$respiratoryState %||% NA_character_,
          markId = mk$markId %||% NA_real_,
          nPoints = mk$nPoints %||% NA_real_,
          calibrationVolume = mk$calibrationVolume %||% NA_real_,
          calibrationConcentration = mk$calibrationConcentration %||% NA_real_,
          cumulativeAddedVolume = mk$cumulativeAddedVolume %||% NA_real_,
          isProtocolMark = mk$isProtocolMark %||% NA,
          isReferenceMark = mk$isReferenceMark %||% NA,
          isBaselineMark = mk$isBaselineMark %||% NA,
          concentrationCorrectionFactor = mk$concentrationCorrectionFactor %||% NA_real_,
          stringsAsFactors = FALSE
        )
        rows <- c(rows, list(row))
      }
    }
  }
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

#' Check for Changes in Project Files
#'
#' Compares current file size/mtime to the cached snapshot stored in the project.
#'
#' @param project A `dld8_project` object.
#' @param include_non_experimental Logical; include non-experimental files.
#' @param detect_new Logical; also detect new files dropped into the folder.
#' @param update Logical; update the project's cache in the returned object.
#' @return List with `changed`, `added`, `removed`, `current`, `cached`.
#'   If `update=TRUE`, includes `project` with refreshed cache.
#' @export
check_project_changes <- function(project, include_non_experimental = TRUE,
                                  detect_new = TRUE, update = FALSE) {
  tracked <- project$dld8_files
  if (isTRUE(include_non_experimental)) {
    tracked <- c(tracked, project$excluded_dld8_files)
  }

  if (isTRUE(detect_new)) {
    all_now <- scan_dld8_files(project$source_folder)
  } else {
    all_now <- tracked
  }

  current <- build_file_cache(project$source_folder, all_now)
  cached <- project$file_cache
  if (is.null(cached)) {
    cached <- data.frame(rel_path = character(), mtime = numeric(), size = numeric(), stringsAsFactors = FALSE)
  }

  cur_map <- setNames(seq_len(nrow(current)), current$rel_path)
  old_map <- setNames(seq_len(nrow(cached)), cached$rel_path)

  added <- setdiff(current$rel_path, cached$rel_path)
  removed <- setdiff(cached$rel_path, current$rel_path)

  common <- intersect(current$rel_path, cached$rel_path)
  changed <- character()
  for (rel in common) {
    c_row <- current[cur_map[[rel]], ]
    o_row <- cached[old_map[[rel]], ]
    if (!isTRUE(all.equal(c_row$mtime, o_row$mtime)) ||
        !isTRUE(all.equal(c_row$size, o_row$size))) {
      changed <- c(changed, rel)
    }
  }

  out <- list(
    changed = changed,
    added = added,
    removed = removed,
    current = current,
    cached = cached
  )

  if (isTRUE(update)) {
    project$file_cache <- current

    # Ensure parsed_dld8 is an environment for inplace updates.
    if (!is.environment(project$parsed_dld8)) {
      env_new <- new.env(parent = emptyenv())
      if (!is.null(project$parsed_dld8) && length(project$parsed_dld8) > 0) {
        for (nm in names(project$parsed_dld8)) {
          env_new[[nm]] <- project$parsed_dld8[[nm]]
        }
      }
      project$parsed_dld8 <- env_new
    }
    env <- project$parsed_dld8

    exclude_non_experimental <- project$settings$exclude_non_experimental %||% TRUE
    project$excluded_chambers <- normalize_excluded_chambers_df(project$excluded_chambers)

    # Remove deleted files from project and cache.
    for (rel in removed) {
      if (exists(rel, envir = env, inherits = FALSE)) {
        rm(list = rel, envir = env)
      }
      project$dld8_files <- setdiff(project$dld8_files, rel)
      project$excluded_dld8_files <- setdiff(project$excluded_dld8_files, rel)
      project$excluded_chambers <- project$excluded_chambers[
        project$excluded_chambers$rel_path != rel, , drop = FALSE
      ]
    }

    # Update changed and added files.
    for (rel in c(changed, added)) {
      abs_path <- file.path(project$source_folder, rel)
      dld8_json <- tryCatch(read_dld8_json(abs_path), error = function(e) NULL)
      env[[rel]] <- dld8_json

      classification <- classify_project_file(
        rel,
        dld8_json,
        exclude_non_experimental = exclude_non_experimental,
        verbose = FALSE
      )
      project$excluded_chambers <- project$excluded_chambers[
        project$excluded_chambers$rel_path != rel, , drop = FALSE
      ]

      if (identical(classification$status, "include")) {
        project$dld8_files <- unique(c(setdiff(project$dld8_files, rel), rel))
        project$excluded_dld8_files <- setdiff(project$excluded_dld8_files, rel)
        if (nrow(classification$excluded_chambers) > 0) {
          project$excluded_chambers <- rbind(project$excluded_chambers, classification$excluded_chambers)
        }
      } else {
        project$excluded_dld8_files <- unique(c(setdiff(project$excluded_dld8_files, rel), rel))
        project$dld8_files <- setdiff(project$dld8_files, rel)
      }
    }

    project$excluded_chambers <- normalize_excluded_chambers_df(project$excluded_chambers)

    out$project <- project
  }

  out
}

#' Add Baseline/Reference Overrides
#'
#' Supports two modes:
#' - Path-based: `rel_path`, `chamber`, and optional `baseline`/`reference`.
#' - Protocol-based: `protocol`, and optional `baseline`/`reference`.
#'
#' @param project A `dld8_project` object.
#' @param overrides Data frame defining overrides.
#' @param replace Logical; replace existing overrides for the same key.
#' @return Updated `dld8_project`.
#' @export
add_marker_overrides <- function(project, overrides, replace = TRUE) {
  # supports either:
  # 1) rel_path, chamber, baseline?, reference?
  # 2) protocol, baseline?, reference?
  if (is.null(overrides) || nrow(overrides) == 0) return(project)

  has_path <- all(c("rel_path", "chamber") %in% names(overrides))
  has_protocol <- "protocol" %in% names(overrides)
  has_baseline <- "baseline" %in% names(overrides)
  has_reference <- "reference" %in% names(overrides)

  if (!has_baseline && !has_reference) {
    stop("Overrides must include at least one of: baseline, reference.")
  }

  if (has_path && !has_protocol) {
    ov <- overrides
    ov$rel_path <- vapply(ov$rel_path, normalize_metadata_value_R, character(1))
    ov$chamber <- vapply(ov$chamber, normalize_metadata_value_R, character(1))
    if (has_baseline) ov$baseline <- vapply(ov$baseline, normalize_metadata_value_R, character(1))
    if (!has_baseline) ov$baseline <- ""
    if (has_reference) ov$reference <- vapply(ov$reference, normalize_metadata_value_R, character(1))
    if (!has_reference) ov$reference <- ""
    ov <- ov[, c("rel_path", "chamber", "baseline", "reference")]
    if (isTRUE(replace) && nrow(project$marker_overrides) > 0) {
      key <- paste(project$marker_overrides$rel_path, project$marker_overrides$chamber, sep = "||")
      new_key <- paste(ov$rel_path, ov$chamber, sep = "||")
      project$marker_overrides <- project$marker_overrides[!key %in% new_key, , drop = FALSE]
    }
    project$marker_overrides <- rbind(project$marker_overrides, ov)
    return(project)
  }

  if (has_protocol && !has_path) {
    ov <- overrides
    ov$protocol <- vapply(ov$protocol, normalize_metadata_value_R, character(1))
    if (has_baseline) ov$baseline <- vapply(ov$baseline, normalize_metadata_value_R, character(1))
    if (!has_baseline) ov$baseline <- ""
    if (has_reference) ov$reference <- vapply(ov$reference, normalize_metadata_value_R, character(1))
    if (!has_reference) ov$reference <- ""
    ov <- ov[, c("protocol", "baseline", "reference")]
    if (isTRUE(replace) && nrow(project$protocol_overrides) > 0) {
      project$protocol_overrides <- project$protocol_overrides[
        !project$protocol_overrides$protocol %in% ov$protocol, , drop = FALSE
      ]
    }
    project$protocol_overrides <- rbind(project$protocol_overrides, ov)
    return(project)
  }

  stop("Overrides must be either path-based (rel_path, chamber) or protocol-based (protocol).")
}

#' Validate Effective Baseline/Reference Markers
#'
#' Checks baseline/reference for every file/chamber. Uses protocol defaults,
#' overridden by protocol-level overrides and then by path-level overrides.
#' Returns a table with the resolved markers, their sources, and existence.
#'
#' @param project A `dld8_project` object.
#' @param include_non_experimental Logical; include non-experimental files.
#' @param require Character vector of required markers (`"baseline"`, `"reference"`).
#' @return Data frame with one row per file/chamber and validation results.
#' @export
validate_effective_markers <- function(project, include_non_experimental = FALSE,
                                       require = c("baseline", "reference")) {
  require <- intersect(c("baseline", "reference"), require)
  rel_files <- project$dld8_files
  if (isTRUE(include_non_experimental)) {
    rel_files <- c(rel_files, project$excluded_dld8_files)
  }

  lib <- load_protocol_library()
  out_rows <- list()

  # we now keep a shared cache on the project object itself
  get_file <- function(rel) {
    get_dld8(project, rel)
  }

  for (rel in rel_files) {
    dld8_json <- get_file(rel)
    if (is.null(dld8_json)) next
    for (ch in get_project_chambers(project, rel, include_non_experimental = include_non_experimental)) {
      md <- get_sample_metadata_from_json(ch, dld8_json)
      if (length(md) == 0) next
      marks <- get_marks_from_chamber(ch, dld8_json, verbose = FALSE)
      if (length(marks) == 0) next

      protocol_raw <- md$protocol %||% ""
      protocol_id <- extract_protocol_id_R(protocol_raw)
      protocol_for_lookup <- if (!is.na(protocol_id)) protocol_id else protocol_raw

      baseline_mark <- NA_character_
      reference_mark <- NA_character_
      baseline_source <- "none"
      reference_source <- "none"

      if (!is.null(lib) && nzchar(protocol_for_lookup)) {
        b <- lib$get_baseline_mark(protocol_for_lookup)
        r <- lib$get_reference_mark(protocol_for_lookup)
        if (!is.na(b) && nzchar(b)) {
          baseline_mark <- b
          baseline_source <- "protocol_default"
        }
        if (!is.na(r) && nzchar(r)) {
          reference_mark <- r
          reference_source <- "protocol_default"
        }
      }

      if (!is.null(project$protocol_overrides) && nrow(project$protocol_overrides) > 0) {
        po <- project$protocol_overrides
        match_rows <- po[po$protocol == protocol_raw, , drop = FALSE]
        if (nrow(match_rows) == 0 && !is.na(protocol_id)) {
          match_rows <- po[po$protocol == protocol_id, , drop = FALSE]
        }
        if (nrow(match_rows) > 0) {
          if (nzchar(match_rows$baseline[1])) {
            baseline_mark <- match_rows$baseline[1]
            baseline_source <- "protocol_override"
          }
          if (nzchar(match_rows$reference[1])) {
            reference_mark <- match_rows$reference[1]
            reference_source <- "protocol_override"
          }
        }
      }

      if (!is.null(project$marker_overrides) && nrow(project$marker_overrides) > 0) {
        mo <- project$marker_overrides
        match_rows <- mo[mo$rel_path == rel & mo$chamber == ch, , drop = FALSE]
        if (nrow(match_rows) > 0) {
          if (nzchar(match_rows$baseline[1])) {
            baseline_mark <- match_rows$baseline[1]
            baseline_source <- "path_override"
          }
          if (nzchar(match_rows$reference[1])) {
            reference_mark <- match_rows$reference[1]
            reference_source <- "path_override"
          }
        }
      }

      baseline_ok <- if ("baseline" %in% require) nzchar(baseline_mark) && marker_exists(baseline_mark, marks) else NA
      reference_ok <- if ("reference" %in% require) nzchar(reference_mark) && marker_exists(reference_mark, marks) else NA

      out_rows <- c(out_rows, list(data.frame(
        rel_path = rel,
        chamber = ch,
        protocol = protocol_raw,
        baseline = if (nzchar(baseline_mark)) baseline_mark else NA_character_,
        baseline_source = baseline_source,
        baseline_ok = baseline_ok,
        reference = if (nzchar(reference_mark)) reference_mark else NA_character_,
        reference_source = reference_source,
        reference_ok = reference_ok,
        stringsAsFactors = FALSE
      )))
    }
  }

  if (length(out_rows) == 0) return(data.frame())
  do.call(rbind, out_rows)
}



#' Load Protocol Library
#'
#' Uses the packaged `protocols` dataset from the `oroboros` package.
#'
#' @param pkg Package name containing the `protocols` dataset.
#' @return List with helpers for mark order and baseline/reference marks.
#' @keywords internal
load_protocol_library <- function(pkg = "oroboros") {
  df <- NULL
  if (exists("protocols", envir = .GlobalEnv, inherits = FALSE)) {
    df <- get("protocols", envir = .GlobalEnv)
  } else if (requireNamespace(pkg, quietly = TRUE)) {
    df <- get("protocols", envir = asNamespace(pkg))
  } else if (file.exists(file.path("assets", "protocols.csv"))) {
    df <- read.csv(file.path("assets", "protocols.csv"),
                   stringsAsFactors = FALSE, check.names = FALSE)
  }
  if (is.null(df)) return(NULL)
  list(
    df = df,
    get_mark_order = function(protocol_name) {
      row <- df[df[["DLP#"]] == protocol_name, , drop = FALSE]
      if (nrow(row) == 0) return(character())
      n_steps <- suppressWarnings(as.integer(row[["# of steps"]][1]))
      if (is.na(n_steps) || n_steps <= 0) return(character())
      marks <- character()
      for (i in seq_len(n_steps)) {
        step_col <- paste0("Step ", i)
        if (!is.null(row[[step_col]]) && !is.na(row[[step_col]][1])) {
          marks <- c(marks, trimws(as.character(row[[step_col]][1])))
        }
      }
      marks
    },
    get_baseline_mark = function(protocol_name) {
      row <- df[df[["DLP#"]] == protocol_name, , drop = FALSE]
      if (nrow(row) == 0) return(NA_character_)
      name <- trimws(as.character(row[["Baseline Mark"]][1]))
      if (!is.na(name) && nzchar(name)) return(name)
      step_no <- suppressWarnings(as.integer(row[["Baseline step"]][1]))
      if (is.na(step_no)) return(NA_character_)
      step_col <- paste0("Step ", step_no)
      if (!is.null(row[[step_col]]) && !is.na(row[[step_col]][1])) {
        return(trimws(as.character(row[[step_col]][1])))
      }
      NA_character_
    },
    get_reference_mark = function(protocol_name) {
      row <- df[df[["DLP#"]] == protocol_name, , drop = FALSE]
      if (nrow(row) == 0) return(NA_character_)
      name <- trimws(as.character(row[["Reference Mark"]][1]))
      if (!is.na(name) && nzchar(name)) return(name)
      step_no <- suppressWarnings(as.integer(row[["Reference step"]][1]))
      if (is.na(step_no)) return(NA_character_)
      step_col <- paste0("Step ", step_no)
      if (!is.null(row[[step_col]]) && !is.na(row[[step_col]][1])) {
        return(trimws(as.character(row[[step_col]][1])))
      }
      NA_character_
    }
  )
}

compute_precalc <- function(meta, marks, verbose = FALSE) {
  # meta: sample metadata list; marks: list of mark objects for one chamber.
  # Returns a list containing per-mark flux_per_volume, specific_flux, and
  # dilution_factor entries.  `specific_flux` is computed as
  #   fluxPV / (sample_conc * prev_df)
  # If sample concentration is missing or zero the specific_flux value is left
  # as NA so that data-quality issues remain visible rather than masked.
  # where sample_conc is derived from metadata.  If concentration is missing or
  # zero we drop it from the denominator; this ensures baseline-corrected and
  # FCR values remain computable even when the metadata is incomplete.
  bg <- meta$backgroundCorrection %||% list()
  a0 <- bg$a0 %||% 0.0
  b0 <- bg$b0 %||% 0.0

  q <- meta$sampleQuantity %||% list()
  amount <- q$amount %||% 0.0
  volume <- q$volume %||% 0.0
  sample_conc <- if (!is.null(volume) && volume != 0) amount / volume else NA_real_
  if (isTRUE(verbose)) {
    message(sprintf("[debug] sample quantity amount=%s volume=%s conc=%s", amount, volume, sample_conc))
  }

  chamber_vol <- meta$chamberVolume %||% 2

  proto <- list()
  for (mk in marks) {
    slope <- NA_real_
    o2c <- NA_real_
    if (isTRUE(verbose)) {
      message(sprintf("[debug] examining mark '%s' with %d values", mk$markName, length(mk$values)))
    }
    if (is.list(mk$values)) {
      for (v in mk$values) {
        di <- v$datastructureInfo %||% list()
        ct <- safe_get(di, c("channelType", "value"), default = NA)
        df <- safe_get(di, c("dataFlags", "value"), default = NA)
        if (!is.na(ct) && !is.na(df)) {
          ct_ok <- (ct == 0) || (toupper(as.character(ct)) == "OXYGEN")
          is_slope <- (df == 1) || (toupper(as.character(df)) == "SLOPE")
          is_signal <- (df == 0) || (toupper(as.character(df)) == "SIGNAL")
          if (ct_ok && is_slope) {
            slope <- v$median %||% v$average %||% v$value %||% NA_real_
          }
          if (ct_ok && is_signal) {
            o2c <- v$median %||% v$average %||% v$value %||% NA_real_
          }
        }
      }
    }
    if (isTRUE(verbose)) {
      if (is.na(slope) || is.na(o2c)) {
        message(sprintf("[debug] mark '%s' missing slope or o2c (slope=%s, o2c=%s)",
                        mk$markName, slope, o2c))
      }
    }
    proto <- c(proto, list(list(
      name = mk$markName,
      slope = slope,
      o2c = o2c,
      calib = mk$calibrationVolume %||% 0.0
    )))
  }

  precalc <- list(
    flux_per_volume = list(),
    dilution_factor = list(),
    sample_conc = sample_conc,
    specific_flux = list()
  )

  prev_df <- 1.0
  for (i in seq_along(proto)) {
    step <- proto[[i]]
    name <- step$name
    slope <- step$slope
    o2c <- step$o2c
    if (isTRUE(verbose)) {
      message(sprintf("[debug] compute_precalc step=%s slope=%s o2c=%s", name, slope, o2c))
    }

    if (!is.na(slope) && !is.na(o2c)) {
      flux_pv <- slope - (a0 + b0 * o2c)
      precalc$flux_per_volume[[name]] <- flux_pv
      if (!is.na(prev_df) && prev_df != 0) {
        if (is.na(sample_conc) || sample_conc == 0) {
          if (isTRUE(verbose)) {
            message(sprintf("[debug] skipping specific_flux for '%s': sample_conc=%s", name, sample_conc))
          }
        } else {
          precalc$specific_flux[[name]] <- flux_pv / (sample_conc * prev_df)
        }
      }
    }

    if (i < length(proto)) {
      next_calib <- proto[[i + 1]]$calib %||% 0.0
      df_i <- prev_df - (prev_df * next_calib / 1000.0) / chamber_vol
    } else {
      df_i <- prev_df
    }
    precalc$dilution_factor[[name]] <- df_i
    prev_df <- df_i
  }

  precalc
}

#' Extract Flux/FCR Tables
#'
#' Builds flux and FCR tables using the same formulas as the Python app.
#'
#' @param project A `dld8_project` object.
#' @param include_non_experimental Logical; include non-experimental files.
#' @param format `"long"` or `"wide"`.
#' @param verbose Logical; print progress.
#' @param mark_order_source `"present"` (use marks present in files),
#'   `"protocol"` (use protocol steps), or `"protocol_then_present"` (fallback).
#' @param group_by_protocol Logical; when TRUE, return a list of tables per protocol.
#' @param pkg Package name containing the `protocols` dataset.
#' @return A list of data frames: `flux_per_volume`, `specific_flux`,
#'   `specific_flux_bc`, `fcr`, `fcr_bc`.  The baseline-corrected and FCR
#'   tables always include the same rows as `specific_flux`; when the baseline
#'   or reference mark is not available (or its value is zero) the corresponding
#'   entries are filled with `NA` rather than dropping the row entirely.
#' @export
extract_flux_tables <- function(project, include_non_experimental = FALSE,
                                format = c("long", "wide"),
                                verbose = FALSE,
                                pkg = "oroboros",
                                mark_order_source = c("present", "protocol", "protocol_then_present"),
                                group_by_protocol = FALSE) {
  format <- match.arg(format)
  mark_order_source <- match.arg(mark_order_source)
  lib <- load_protocol_library(pkg = pkg)

  source_folder <- project$source_folder
  rel_files <- project$dld8_files
  if (isTRUE(include_non_experimental)) {
    rel_files <- c(rel_files, project$excluded_dld8_files)
  }
  if (length(rel_files) == 0) {
    return(list(
      flux_per_volume = data.frame(),
      specific_flux = data.frame(),
      specific_flux_bc = data.frame(),
      fcr = data.frame(),
      fcr_bc = data.frame()
    ))
  }

  rows_flux <- list()
  rows_spec <- list()
  rows_spec_bc <- list()
  rows_fcr <- list()
  rows_fcr_bc <- list()

  processed_pairs <- 0
  for (rel in rel_files) {
    dld8_json <- get_dld8(project, rel)
    if (is.null(dld8_json)) {
      if (isTRUE(verbose)) message("[skip] parse failed: ", rel)
      next
    }

    for (ch in get_project_chambers(project, rel, include_non_experimental = include_non_experimental)) {
      md <- get_sample_metadata_from_json(ch, dld8_json)
      if (length(md) == 0) {
        if (isTRUE(verbose)) message("[skip] no metadata: ", rel, " ", ch)
        next
      }

      marks <- get_marks_from_chamber(ch, dld8_json)
      if (length(marks) == 0) {
        if (isTRUE(verbose)) message("[skip] no marks: ", rel, " ", ch)
        next
      }

      precalc <- compute_precalc(md, marks, verbose = verbose)
      protocol_raw <- md$protocol %||% NA_character_
      protocol_id <- extract_protocol_id_R(protocol_raw)
      protocol_for_lookup <- if (!is.na(protocol_id)) protocol_id else protocol_raw
      protocol_name <- protocol_raw

      baseline_mark <- if (!is.null(lib)) lib$get_baseline_mark(protocol_for_lookup) else NA_character_
      reference_mark <- if (!is.null(lib)) lib$get_reference_mark(protocol_for_lookup) else NA_character_

      if (!is.null(project$protocol_overrides) && nrow(project$protocol_overrides) > 0) {
        po <- project$protocol_overrides
        match_rows <- po[po$protocol == protocol_raw, , drop = FALSE]
        if (nrow(match_rows) == 0 && !is.na(protocol_id)) {
          match_rows <- po[po$protocol == protocol_id, , drop = FALSE]
        }
        if (nrow(match_rows) > 0) {
          if (nzchar(match_rows$baseline[1])) baseline_mark <- match_rows$baseline[1]
          if (nzchar(match_rows$reference[1])) reference_mark <- match_rows$reference[1]
        }
      }

      if (!is.null(project$marker_overrides) && nrow(project$marker_overrides) > 0) {
        match_rows <- project$marker_overrides[
          project$marker_overrides$rel_path == rel & project$marker_overrides$chamber == ch, , drop = FALSE
        ]
        if (nrow(match_rows) > 0) {
          if (nzchar(match_rows$baseline[1])) baseline_mark <- match_rows$baseline[1]
          if (nzchar(match_rows$reference[1])) reference_mark <- match_rows$reference[1]
        }
      }

      mark_order <- character()
      if (mark_order_source == "protocol" || mark_order_source == "protocol_then_present") {
        if (!is.null(lib)) {
          mark_order <- lib$get_mark_order(protocol_for_lookup)
        }
      }
      if (mark_order_source == "present" || length(mark_order) == 0) {
        mark_order <- vapply(marks, function(mk) mk$markName %||% NA_character_, character(1))
      }
      mark_order <- mark_order[!is.na(mark_order) & nzchar(mark_order)]
      if (length(mark_order) == 0) {
        if (isTRUE(verbose)) message("[skip] empty mark order: ", rel, " ", ch)
        next
      }

      baseline_mark <- resolve_marker_text_only(baseline_mark, marks)
      reference_mark <- resolve_marker_text_only(reference_mark, marks)

      if (isTRUE(verbose)) {
        message(sprintf("[debug] %s %s baseline='%s' reference='%s' # marks=%d",
                        rel, ch, baseline_mark, reference_mark, length(marks)))
      }

      meta_row <- list(
        rel_path = rel,
        filename = basename(rel),
        chamber = ch,
        chamberVolume = md$chamberVolume %||% NA_real_,
        sampleAmount = (md$sampleQuantity %||% list())$amount %||% NA_real_,
        sampleConcentration = if (!is.null(md$chamberVolume) && md$chamberVolume != 0 &&
                                  !is.null((md$sampleQuantity %||% list())$amount)) {
          (md$sampleQuantity$amount) / md$chamberVolume
        } else {
          NA_real_
        },
        baselineMarker = baseline_mark,
        referenceMarker = reference_mark,
        baselineStep = if (!is.null(lib) && !is.na(baseline_mark)) {
          match(baseline_mark, mark_order)
        } else {
          NA_integer_
        },
        referenceStep = if (!is.null(lib) && !is.na(reference_mark)) {
          match(reference_mark, mark_order)
        } else {
          NA_integer_
        },
        oxygraph = md$oxygraph %||% NA_character_,
        sensor = md$sensor %||% NA_character_,
        sampleType = md$metaData$sampleType %||% NA_character_,
        cohort = md$metaData$cohort %||% NA_character_,
        sampleCode = md$metaData$sampleCode %||% NA_character_,
        sampleNumber = md$metaData$sampleNumber %||% NA_character_,
        subsampleNumber = md$metaData$subsampleNumber %||% NA_character_,
        protocol = protocol_name
      )

      processed_pairs <- processed_pairs + 1
      for (mark in mark_order) {
        flux_val <- precalc$flux_per_volume[[mark]] %||% NA_real_
        spec_val <- precalc$specific_flux[[mark]] %||% NA_real_

        rows_flux <- c(rows_flux, list(as.data.frame(c(meta_row, list(mark = mark, value = flux_val)))))
        rows_spec <- c(rows_spec, list(as.data.frame(c(meta_row, list(mark = mark, value = spec_val)))))

        # determine baseline/reference values once per pair
        base_val <- NA_real_
        if (!is.na(baseline_mark) && !is.null(precalc$specific_flux[[baseline_mark]])) {
          base_val <- precalc$specific_flux[[baseline_mark]]
        }
        ref_val <- NA_real_
        if (!is.na(reference_mark) && !is.null(precalc$specific_flux[[reference_mark]])) {
          ref_val <- precalc$specific_flux[[reference_mark]]
        }
        if (isTRUE(verbose)) {
          message(sprintf("[debug]   mark=%s spec=%s base=%s ref=%s",
                          mark, spec_val, base_val, ref_val))
        }

        # always append rows (value may be NA when baseline/ref missing)
        spec_bc_val <- NA_real_
        if (!is.na(spec_val) && !is.na(base_val)) {
          # note: if base_val is zero, preserve spec_val-base_val = spec_val (no correction);
          # this keeps behaviour roughly consistent while avoiding empty tables
          spec_bc_val <- spec_val - base_val
        }
        rows_spec_bc <- c(rows_spec_bc, list(as.data.frame(c(
          meta_row, list(mark = mark, value = spec_bc_val)
        ))))

        fcr_val <- NA_real_
        if (!is.na(spec_val) && !is.na(ref_val) && ref_val != 0) {
          fcr_val <- spec_val / ref_val
        }
        rows_fcr <- c(rows_fcr, list(as.data.frame(c(
          meta_row, list(mark = mark, value = fcr_val)
        ))))

        fcr_bc_val <- NA_real_
        if (!is.na(spec_val) && !is.na(base_val) && !is.na(ref_val)) {
          ref_bc <- ref_val - base_val
          if (!is.na(ref_bc) && ref_bc != 0) {
            fcr_bc_val <- (spec_val - base_val) / ref_bc
          }
        }
        rows_fcr_bc <- c(rows_fcr_bc, list(as.data.frame(c(
          meta_row, list(mark = mark, value = fcr_bc_val)
        ))))
      }
    }
  }

  if (isTRUE(verbose)) message("[info] processed pairs: ", processed_pairs)
  to_df <- function(rows) if (length(rows) == 0) data.frame() else do.call(rbind, rows)
  tables_long <- list(
    flux_per_volume = to_df(rows_flux),
    specific_flux = to_df(rows_spec),
    specific_flux_bc = to_df(rows_spec_bc),
    fcr = to_df(rows_fcr),
    fcr_bc = to_df(rows_fcr_bc)
  )

  if (format == "long") {
    if (!isTRUE(group_by_protocol)) return(tables_long)
    split_by_protocol <- function(df) {
      if (nrow(df) == 0) return(list())
      split(df, df$protocol, drop = TRUE)
    }
    return(list(
      flux_per_volume = split_by_protocol(tables_long$flux_per_volume),
      specific_flux = split_by_protocol(tables_long$specific_flux),
      specific_flux_bc = split_by_protocol(tables_long$specific_flux_bc),
      fcr = split_by_protocol(tables_long$fcr),
      fcr_bc = split_by_protocol(tables_long$fcr_bc)
    ))
  }

  meta_cols <- c(
    "rel_path","filename","chamber","chamberVolume","sampleAmount","sampleConcentration",
    "baselineMarker","referenceMarker","baselineStep","referenceStep",
    "oxygraph","sensor",
    "sampleType","cohort","sampleCode","sampleNumber","subsampleNumber","protocol"
  )

  wide_from_long <- function(df) {
    if (nrow(df) == 0) return(df)
    df$mark <- as.character(df$mark)
    if (requireNamespace("tidyr", quietly = TRUE)) {
      wide <- tidyr::pivot_wider(
        df,
        id_cols = all_of(meta_cols),
        names_from = mark,
        values_from = value,
        values_fn = list(value = ~ if (length(.x)) .x[[1]] else NA_real_)
      )
      return(as.data.frame(wide))
    }
    wide <- reshape(df, idvar = meta_cols, timevar = "mark", direction = "wide")
    colnames(wide) <- sub("^value\\.", "", colnames(wide))
    wide
  }

  tables_wide <- list(
    flux_per_volume = wide_from_long(tables_long$flux_per_volume),
    specific_flux = wide_from_long(tables_long$specific_flux),
    specific_flux_bc = wide_from_long(tables_long$specific_flux_bc),
    fcr = wide_from_long(tables_long$fcr),
    fcr_bc = wide_from_long(tables_long$fcr_bc)
  )

  if (!isTRUE(group_by_protocol)) return(tables_wide)

  split_by_protocol <- function(df) {
    if (nrow(df) == 0) return(list())
    split(df, df$protocol, drop = TRUE)
  }

  list(
    flux_per_volume = split_by_protocol(tables_wide$flux_per_volume),
    specific_flux = split_by_protocol(tables_wide$specific_flux),
    specific_flux_bc = split_by_protocol(tables_wide$specific_flux_bc),
    fcr = split_by_protocol(tables_wide$fcr),
    fcr_bc = split_by_protocol(tables_wide$fcr_bc)
  )
}

matches_mark_datastructure_field <- function(actual, expected) {
  if (is.null(actual) || length(actual) == 0 || any(is.na(actual))) return(FALSE)
  actual <- actual[[1]]

  if (is.numeric(expected)) {
    actual_num <- suppressWarnings(as.numeric(actual))
    return(!is.na(actual_num) && isTRUE(all.equal(actual_num, expected)))
  }

  identical(
    toupper(trimws(as.character(actual))),
    toupper(trimws(as.character(expected)))
  )
}

extract_mark_channel_value <- function(mark,
                                       datastructure_label,
                                       channel_type,
                                       data_location,
                                       data_flags,
                                       value_field = "median") {
  if (!is.list(mark$values) || length(mark$values) == 0) return(NA_real_)

  for (v in mark$values) {
    di <- v$datastructureInfo %||% list()
    label_ok <- matches_mark_datastructure_field(
      safe_get(di, c("datastructureLabel", "value"), default = NA),
      datastructure_label
    )
    channel_ok <- matches_mark_datastructure_field(
      safe_get(di, c("channelType", "value"), default = NA),
      channel_type
    )
    location_ok <- matches_mark_datastructure_field(
      safe_get(di, c("dataLocation", "value"), default = NA),
      data_location
    )
    flags_ok <- matches_mark_datastructure_field(
      safe_get(di, c("dataFlags", "value"), default = NA),
      data_flags
    )

    if (label_ok && channel_ok && location_ok && flags_ok) {
      value <- v[[value_field]] %||% NA_real_
      return(as.numeric(value))
    }
  }

  NA_real_
}

#' Extract Other Channel Tables
#'
#' Builds mark-based tables for non-flux channels using the same metadata and
#' mark ordering logic as `extract_flux_tables()`.
#'
#' Currently this returns the oxygen signal per mark, using mark values where
#' `datastructureLabel == "O2"`, `channelType == 0`, `dataLocation == 3`,
#' and `dataFlags == 0`; the numeric value is taken from `median$value`.
#'
#' @param project A `dld8_project` object.
#' @param include_non_experimental Logical; include non-experimental files.
#' @param format `"long"` or `"wide"`.
#' @param verbose Logical; print progress.
#' @param mark_order_source `"present"` (use marks present in files),
#'   `"protocol"` (use protocol steps), or `"protocol_then_present"` (fallback).
#' @param group_by_protocol Logical; when TRUE, return a list of tables per protocol.
#' @param pkg Package name containing the `protocols` dataset.
#' @return A list of data frames with one entry, `oxygen`.
#' @export
extract_other_channels <- function(project, include_non_experimental = FALSE,
                                   format = c("long", "wide"),
                                   verbose = FALSE,
                                   pkg = "oroboros",
                                   mark_order_source = c("present", "protocol", "protocol_then_present"),
                                   group_by_protocol = FALSE) {
  format <- match.arg(format)
  mark_order_source <- match.arg(mark_order_source)
  lib <- load_protocol_library(pkg = pkg)

  source_folder <- project$source_folder
  rel_files <- project$dld8_files
  if (isTRUE(include_non_experimental)) {
    rel_files <- c(rel_files, project$excluded_dld8_files)
  }
  if (length(rel_files) == 0) {
    return(list(
      oxygen = data.frame()
    ))
  }

  rows_oxygen <- list()

  processed_pairs <- 0
  for (rel in rel_files) {
    dld8_json <- get_dld8(project, rel)
    if (is.null(dld8_json)) {
      if (isTRUE(verbose)) message("[skip] parse failed: ", rel)
      next
    }

    for (ch in get_project_chambers(project, rel, include_non_experimental = include_non_experimental)) {
      md <- get_sample_metadata_from_json(ch, dld8_json)
      if (length(md) == 0) {
        if (isTRUE(verbose)) message("[skip] no metadata: ", rel, " ", ch)
        next
      }

      marks <- get_marks_from_chamber(ch, dld8_json)
      if (length(marks) == 0) {
        if (isTRUE(verbose)) message("[skip] no marks: ", rel, " ", ch)
        next
      }

      protocol_raw <- md$protocol %||% NA_character_
      protocol_id <- extract_protocol_id_R(protocol_raw)
      protocol_for_lookup <- if (!is.na(protocol_id)) protocol_id else protocol_raw
      protocol_name <- protocol_raw

      baseline_mark <- if (!is.null(lib)) lib$get_baseline_mark(protocol_for_lookup) else NA_character_
      reference_mark <- if (!is.null(lib)) lib$get_reference_mark(protocol_for_lookup) else NA_character_

      if (!is.null(project$protocol_overrides) && nrow(project$protocol_overrides) > 0) {
        po <- project$protocol_overrides
        match_rows <- po[po$protocol == protocol_raw, , drop = FALSE]
        if (nrow(match_rows) == 0 && !is.na(protocol_id)) {
          match_rows <- po[po$protocol == protocol_id, , drop = FALSE]
        }
        if (nrow(match_rows) > 0) {
          if (nzchar(match_rows$baseline[1])) baseline_mark <- match_rows$baseline[1]
          if (nzchar(match_rows$reference[1])) reference_mark <- match_rows$reference[1]
        }
      }

      if (!is.null(project$marker_overrides) && nrow(project$marker_overrides) > 0) {
        match_rows <- project$marker_overrides[
          project$marker_overrides$rel_path == rel & project$marker_overrides$chamber == ch, , drop = FALSE
        ]
        if (nrow(match_rows) > 0) {
          if (nzchar(match_rows$baseline[1])) baseline_mark <- match_rows$baseline[1]
          if (nzchar(match_rows$reference[1])) reference_mark <- match_rows$reference[1]
        }
      }

      mark_order <- character()
      if (mark_order_source == "protocol" || mark_order_source == "protocol_then_present") {
        if (!is.null(lib)) {
          mark_order <- lib$get_mark_order(protocol_for_lookup)
        }
      }
      if (mark_order_source == "present" || length(mark_order) == 0) {
        mark_order <- vapply(marks, function(mk) mk$markName %||% NA_character_, character(1))
      }
      mark_order <- mark_order[!is.na(mark_order) & nzchar(mark_order)]
      if (length(mark_order) == 0) {
        if (isTRUE(verbose)) message("[skip] empty mark order: ", rel, " ", ch)
        next
      }

      baseline_mark <- resolve_marker_text_only(baseline_mark, marks)
      reference_mark <- resolve_marker_text_only(reference_mark, marks)

      meta_row <- list(
        rel_path = rel,
        filename = basename(rel),
        chamber = ch,
        chamberVolume = md$chamberVolume %||% NA_real_,
        sampleAmount = (md$sampleQuantity %||% list())$amount %||% NA_real_,
        sampleConcentration = if (!is.null(md$chamberVolume) && md$chamberVolume != 0 &&
                                  !is.null((md$sampleQuantity %||% list())$amount)) {
          (md$sampleQuantity$amount) / md$chamberVolume
        } else {
          NA_real_
        },
        baselineMarker = baseline_mark,
        referenceMarker = reference_mark,
        baselineStep = if (!is.null(lib) && !is.na(baseline_mark)) {
          match(baseline_mark, mark_order)
        } else {
          NA_integer_
        },
        referenceStep = if (!is.null(lib) && !is.na(reference_mark)) {
          match(reference_mark, mark_order)
        } else {
          NA_integer_
        },
        oxygraph = md$oxygraph %||% NA_character_,
        sensor = md$sensor %||% NA_character_,
        sampleType = md$metaData$sampleType %||% NA_character_,
        cohort = md$metaData$cohort %||% NA_character_,
        sampleCode = md$metaData$sampleCode %||% NA_character_,
        sampleNumber = md$metaData$sampleNumber %||% NA_character_,
        subsampleNumber = md$metaData$subsampleNumber %||% NA_character_,
        protocol = protocol_name
      )

      processed_pairs <- processed_pairs + 1
      for (mark in mark_order) {
        mark_idx <- which(vapply(marks, function(mk) identical(mk$markName, mark), logical(1)))[1]
        oxygen_val <- if (!is.na(mark_idx)) {
          extract_mark_channel_value(
            marks[[mark_idx]],
            datastructure_label = "O2",
            channel_type = 0,
            data_location = 3,
            data_flags = 0,
            value_field = "median"
          )
        } else {
          NA_real_
        }

        if (isTRUE(verbose)) {
          message(sprintf("[debug] %s %s mark=%s oxygen=%s", rel, ch, mark, oxygen_val))
        }

        rows_oxygen <- c(rows_oxygen, list(as.data.frame(c(
          meta_row, list(mark = mark, value = oxygen_val)
        ))))
      }
    }
  }

  if (isTRUE(verbose)) message("[info] processed pairs: ", processed_pairs)
  to_df <- function(rows) if (length(rows) == 0) data.frame() else do.call(rbind, rows)
  tables_long <- list(
    oxygen = to_df(rows_oxygen)
  )

  if (format == "long") {
    if (!isTRUE(group_by_protocol)) return(tables_long)
    split_by_protocol <- function(df) {
      if (nrow(df) == 0) return(list())
      split(df, df$protocol, drop = TRUE)
    }
    return(list(
      oxygen = split_by_protocol(tables_long$oxygen)
    ))
  }

  meta_cols <- c(
    "rel_path","filename","chamber","chamberVolume","sampleAmount","sampleConcentration",
    "baselineMarker","referenceMarker","baselineStep","referenceStep",
    "oxygraph","sensor",
    "sampleType","cohort","sampleCode","sampleNumber","subsampleNumber","protocol"
  )

  wide_from_long <- function(df) {
    if (nrow(df) == 0) return(df)
    df$mark <- as.character(df$mark)
    if (requireNamespace("tidyr", quietly = TRUE)) {
      wide <- tidyr::pivot_wider(
        df,
        id_cols = all_of(meta_cols),
        names_from = mark,
        values_from = value,
        values_fn = list(value = ~ if (length(.x)) .x[[1]] else NA_real_)
      )
      return(as.data.frame(wide))
    }
    wide <- reshape(df, idvar = meta_cols, timevar = "mark", direction = "wide")
    colnames(wide) <- sub("^value\\.", "", colnames(wide))
    wide
  }

  tables_wide <- list(
    oxygen = wide_from_long(tables_long$oxygen)
  )

  if (!isTRUE(group_by_protocol)) return(tables_wide)

  split_by_protocol <- function(df) {
    if (nrow(df) == 0) return(list())
    split(df, df$protocol, drop = TRUE)
  }

  list(
    oxygen = split_by_protocol(tables_wide$oxygen)
  )
}
