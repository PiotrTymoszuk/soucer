# This script contains the code for functions used for signaling within a script


#' Messages displayed during R file sourcing
#'
#' @description
#' `insert_msg()` displays a user defined message during sourcing of the script.
#' `insert_head()` prints a message with the name of the sourced script.
#' `insert_tail()` displays a message with the name of the sourced script and
#' user-specified confirmation of the successful execution.
#'
#' @param text user-specified message text.
#' @param separator separator sign.
#' @param sep_len separator length.
#' @param prefix a user-specified text displayed before the script name.
#' @param suffix a user-specified text displayed after the script name.
#'
#' @return Nothing, called for its side effects.
#'
#' @export

  insert_msg <- function(text = NULL, separator = '>', sep_len = 60) {

    message(rep(separator, sep_len))

    if(!is.null(text)) message(text)

  }

#' @rdname insert_msg
#' @export

  insert_head <- function(prefix = 'Executing:') {

    insert_msg(paste(prefix, scriptName::current_filename()), separator = '<>')

  }

#' @rdname insert_msg
#' @export

  insert_tail <- function(suffix = 'succesfully sourced') {

    insert_msg(paste(scriptName::current_filename(), suffix), separator = '<>')

  }

# END ------
