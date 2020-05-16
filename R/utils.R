
`%||%` <- function(l, r) if (is.null(l)) r else l

get_zip_data <- function(files, keys, recurse, include_directories) {
  files <- normalizePath(files)
  is_dir <- file.info(files)$isdir

  if (!recurse && any(is_dir)) {
    warning("directories ignored in zip file, specify recurse = TRUE")
    files <- files[!is_dir]
    keys <- keys[!is_dir]
    is_dir <- is_dir[!is_dir]
  }

  if (!length(files)) {
    return(data.frame(
      stringsAsFactors = FALSE,
      key = character(),
      files = character(),
      dir = logical()
    ))
  }

  zip_data <- do.call(rbind, mapply(
    function(key, file, is_dir) {
      if (is_dir) {
        files <- c(
          "", # Entry for the parent dir
          list.files( # Entries for all children (dirs and files)
            path = file,
            recursive = TRUE,
            include.dirs = TRUE,
            all.files = TRUE,
            no.. = TRUE
          )
        )

        keys <- file.path(key, files)
        files <- file.path(file, files)
        dirs <- file.info(files)$isdir
        keys[dirs & !grepl("/$", keys)] <- paste0(
          keys[dirs & !grepl("/$", keys)], "/"
        )

        data.frame(
          stringsAsFactors = FALSE,
          key = keys,
          files = files,
          dir = dirs
        )
      } else {
        data.frame(
          stringsAsFactors = FALSE,
          key = key,
          files = file,
          dir = FALSE
        )
      }
    },
    key = keys,
    file = files,
    is_dir = is_dir,
    SIMPLIFY = FALSE
  ))

  zip_data <- zip_data[!duplicated(zip_data$key) & zip_data$key != "/", ]

  row.names(zip_data) <- NULL
  zip_data$files <- normalizePath(zip_data$files)

  if (!include_directories) {
    zip_data[!zip_data$dir, ]
  } else {
    zip_data
  }
}

warn_for_dotdot <- function(files) {
  if (any(grepl("^[.][/\\\\]", files))) {
    warning("Some paths start with `./`, creating non-portable zip file")
  }
  if (any(grepl("^[.][.][/\\\\]", files))) {
    warning("Some paths reference parent directory, ",
            "creating non-portable zip file")
  }
  files
}

mkdirp <- function(x, ...) {
  dir.create(x, showWarnings = FALSE, recursive = TRUE, ...)
}

need_packages <- function(pkgs, what = "this function") {
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop(sprintf("The `%s` package is needed for %s", p, what))
    }
  }
}

deprecated <- function(id, msg) {
  if (isTRUE(zip_data$deprecated[[id]])) return(invisible())
  zip_data$deprecated <- as.list(zip_data$deprecated)
  zip_data$deprecated[[id]] <-  TRUE
  m <- simpleMessage(paste0("Note: ", msg, "\n"))
  class(m) <- c("deprecated", class(m))
  message(m)
}
