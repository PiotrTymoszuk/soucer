# Tools for moving between directories

#' Navigate between directories and list directory content
#'
#' @description
#' `enter_directory()` sets working directory.
#' `go_proj_directory()` sets the working directory back to the RStudio
#' project directory.
#' `go_up_directory()` moves up in the directory tree relative to the current
#' working directory.
#' `list_file_type()` lists all files in the given directory with the
#' given extension.
#'
#' @details
#' Wrappers around the base functions `dir.exists()` and `dir.create()`.
#' If `create = FALSE` in `enter_directory()`, the function raises an error,
#' if the path does not exist.
#'
#' @param path a character vector containing a single path name.
#' @param create logical; should the directory be created if it does
#' not exist yet?
#' @param showWarnings logical; should the warnings on failure be shown?
#' @param extension file extension.
#' @param ... other arguments passed to dir.create().
#'
#' @return Nothing, called for its side effects.
#'
#' @export

  enter_directory <- function(path, create = FALSE, showWarnings = TRUE, ...) {

    chk_dir <- dir.exists(paths = path)

    if(!chk_dir) {

      if(!create) {

        stop(paste("The directory does not exist. Change the 'create'",
                   "argument to create it"),
             call. = FALSE)

      } else {

        message(paste('Creating', path))

        dir.create(path, showWarnings = showWarnings, ...)

      }

    }

    setwd(path)

  }

#' @rdname enter_directory
#' @export

  go_proj_directory <- function() {

    setwd(rprojroot::find_rstudio_root_file())

  }

#' @rdname enter_directory
#' @export

  go_up_directory <- function(){

    file_sep <- .Platform$file.sep

    splitted_dir <- stringi::stri_split_fixed(getwd(), pattern = file_sep)[[1]]

    up_dir <- paste(splitted_dir[-length(splitted_dir)], collapse = file_sep)

    setwd(up_dir)

  }

#' @rdname enter_directory
#' @export

  list_file_type <- function(path = '.', extension = '.txt') {

    list.files(path = path,
               pattern = paste0('\\', extension, '$'))

  }

# END ----
