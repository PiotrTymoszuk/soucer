# This script contains the code for functions used for signaling within a script


#' Insert a message displayed during file sourcing
#'
#' @description Displays a user defined message during sourcing of the script.
#' @param text user-specified message text.
#' @param separator separator sign.
#' @param sep_len separator length.
#' @return Nothing, called for its side effects.
#' @export

  insert_msg <- function(text = NULL, separator = '>', sep_len = 60) {

    message(rep(separator, sep_len))

    if(!is.null(text)) message(text)

  }

#' Insert a header with the name of the sourced script
#'
#' @description Displays a message with the name of the sourced script.
#' @param prefix a user-specified prefix displayed before the script name.
#' @return Nothing, called for its side effects.
#' @export

  insert_head <- function(prefix = 'Executing:') {

    insert_msg(paste(prefix, scriptName::current_filename()), separator = '<>')

  }

#' Insert a trailer with the name of the sourced script
#'
#' @description Displays a message with the name of the sourced script.
#' and user-specified confirmation of the successful execution.
#' @param prefix a user-specified prefix displayed before the script name.
#' @return Nothing, called for its side effects.
#' @export

  insert_tail <- function(def_suffix = 'succesfully sourced') {

    insert_msg(paste(scriptName::current_filename(), def_suffix), separator = '<>')

  }
