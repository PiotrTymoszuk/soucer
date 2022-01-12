# contains code for sourcing R files

#' Source safely the R file
#'
#' @param path path to the R file.
#' @param encoding encoding of the script, UTF-8 per default.
#' @param message logical, should a message with the file name be displayed prior to execution?
#' @param crash logical, should an error be returned? Useful during debugging.
#' @param ... additional arguments passed to source().
#' @return a data frame with the script name, execution status and possible errors.
#' @details sources the script, the first error is captured. Useful in multi-file analysis pipelines.
#' @export

  source_safe <- function(path, encoding = 'UTF-8', message = FALSE, crash = FALSE, ...) {

    if(length(path) > 1) stop('Provide a single path string')

    if(message) insert_msg(paste('Executing:', path))

    start_time <- Sys.time()

    diagn_output <- try(source(path,
                               encoding = 'UTF-8', ...),
                        silent = TRUE)

    end_time <- Sys.time()

    if(class(diagn_output) == 'try-error') {

      if(crash) stop(diagn_output, call. = FALSE)

      status <- tibble::tibble(path = path,
                               status = 'failed',
                               errors = attr(diagn_output, 'condition')$message,
                               calls = paste(as.character(attr(diagn_output, 'condition')$call), collapse = ' '),
                               elapsed = end_time - start_time)

    } else {

      status <- tibble::tibble(path = path,
                               status = 'success',
                               errors = 'none',
                               calls = 'none',
                               elapsed = end_time - start_time)

    }

    if(message) {

      print(status)

    }

    status

  }

#' Source safely a pipeline of R files
#'
#' @param paths a vector with the file paths.
#' @param encoding encoding of the script, UTF-8 per default.
#' @param message logical, should a message with the file name be displayed prior to execution?
#' @param crash logical, should an error be returned? Useful during debugging.
#' @param ... additional arguments passed to source().
#' @return a data frame with the script names, execution status and possible errors.
#' @details sources the scrips, the first error per script is captured. Useful in multi-file analysis pipelines:
#' if 'crash' is FALSE, single execution errors are captured but do not necessarily crash the pipeline.
#' @export

  source_all <- function(paths, encoding = 'UTF-8', message = FALSE, crash = FALSE, ...) {

    purrr::map_dfr(paths,
                   source_safe,
                   encoding = encoding,
                   message = message,
                   crash = crash, ...)

  }
