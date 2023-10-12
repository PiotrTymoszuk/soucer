# Contains code for sourcing R files

#' Source safely R files
#'
#' @description
#' `source_safe()` and `source_all()` source respectively one or more R scripts
#' and return their execution status. The first error is captured and, if
#' `crash = TRUE`, halts the execution of the script pipeline.
#'#'
#' @param path path to an R file.
#' @param paths paths to R files.
#' @param encoding encoding of the script, UTF-8 per default.
#' @param message logical, should a message with the file name be displayed
#' prior to and after execution?
#' @param crash logical, should an error be returned? Useful during debugging.
#' @param ... additional arguments passed to `source()`.
#'
#' @return a data frame with the script name, execution status and
#' possible errors.
#'
#' @export

  source_safe <- function(path,
                          encoding = 'UTF-8',
                          message = FALSE,
                          crash = FALSE, ...) {

    ## input control -------

    if(length(path) > 1) stop('Provide a single path string')

    stopifnot(is.logical(message))
    stopifnot(is.logical(crash))

    ## sourcing ------

    if(message) insert_msg(paste('Executing:', path))

    start_time <- Sys.time()

    diagn_output <- try(source(path, encoding = 'UTF-8', ...),
                        silent = TRUE)

    end_time <- Sys.time()

    if(inherits(diagn_output, 'try-error')) {

      if(crash) stop(diagn_output, call. = FALSE)

      status <-
        tibble::tibble(path = path,
                       status = 'failed',
                       errors = attr(diagn_output, 'condition')$message,
                       calls = paste(as.character(attr(diagn_output, 'condition')$call),
                                     collapse = ' '),
                       elapsed = end_time - start_time)

    } else {

      status <- tibble::tibble(path = path,
                               status = 'success',
                               errors = 'none',
                               calls = 'none',
                               elapsed = end_time - start_time)

    }

    if(message) print(status)

    status

  }

#' @rdname source_safe
#' @export

  source_all <- function(paths,
                         encoding = 'UTF-8',
                         message = FALSE,
                         crash = FALSE, ...) {

    purrr::map_dfr(paths,
                   source_safe,
                   encoding = encoding,
                   message = message,
                   crash = crash, ...)

  }

# END -----
