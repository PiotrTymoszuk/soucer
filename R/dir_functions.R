# This script contains tools for moving between directories

# directory moving ------

#' Go to or create a directory
#'
#' @param path a character vector containing a single path name.
#' @param create logical; should the directory be created if it does not exist yet?
#' @param showWarnings logical; should the warnings on failure be shown?
#' @param ... other arguments passed to dir.create().
#' @return Nothing, called for its side effects.
#' @description A handy wrapper around the base functions dir.exists and dir.create.
#' @details If 'create' is set to FALSE, the function raises an error, if the path does not exist.
#' @export

  enter_directory <- function(path, create = FALSE, showWarnings = TRUE, ...) {

    chk_dir <- dir.exists(paths = path)

    if(!chk_dir) {

      if(!create) {

        stop("The directory does not exist. Change the 'create' argument to create it")

      } else {

        message(paste('Creating', path))

        dir.create(path, showWarnings = showWarnings, ...)

      }

    }

    setwd(path)

  }

#' Go back to the project directory
#'
#' @description Sets the working directory back to the RStudio project directory.
#' @return Nothing, called for its side effects.
#' @export

  go_proj_directory <- function() {

    setwd(rprojroot::find_rstudio_root_file())

  }

#' Go up in the directory tree
#'
#' @description Moves up in the directory tree relative to the current working directory.
#' @return Nothing, called for its side effects.
#' @export

  go_up_directory <- function(){

    file_sep <- .Platform$file.sep

    splitted_dir <- stringi::stri_split_fixed(getwd(), pattern = file_sep)[[1]]

    up_dir <- paste(splitted_dir[-length(splitted_dir)], collapse = file_sep)

    setwd(up_dir)

  }

#' List files with the given extension
#'
#' @description Lists all files in the given directory with the given extension.
#' @param path a character vector containing a single path name.
#' @param extension file extension.
#' @return Nothing, called for its side effects.
#' @export

  list_file_type <- function(path = '.', extension = '.txt') {

    list.files(path = path,
               pattern = paste0('\\', extension, '$'))

  }

