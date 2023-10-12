# Functions for accessing cached R objects

#' Load cached script execution results.
#'
#' @description
#' A short description...
#'
#' @param cache_path a character string specifying the path and name of the
#' cached file to be loaded (usually `.RData` or `.RDA`).
#' @param script_path a character string specifying the path to the R script to
#' be executed if the cached file does not exist.
#' @param encoding encoding of the script, UTF-8 per default.
#' @param message user defined text to be displayed prior to loading of
#' the cached file.
#' @param envir the environment where the cached file data should be loaded.
#' @param crash logical, should the execution of the R script be halted on error
#' and the error returned?
#'
#' @export

  access_cache <- function(cache_path,
                           script_path,
                           message = 'Loading cached results',
                           envir = globalenv(),
                           crash = TRUE,
                           encoding = 'UTF-8') {

    if(file.exists(cache_path)) {

      insert_msg(message)

      load(cache_path, envir = envir)

    } else {

      source_safe(script_path,
                  message = TRUE,
                  crash = crash,
                  encoding = encoding)

    }

  }

# END -------
