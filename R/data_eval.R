
#' Create tibbles from user objects and/or user data
#'
#' @param .data A data.frame/tibble, or NULL
#' @param .allow_null Flag to allow NULL values as arguments
#'   (these will not appear as columns in the output)
#' @param ... Arguments are passed to \link[dplyr]{transmute} if \code{data} is
#'   present, and \link[tibble]{tibble} if it is not.
#'
#' @return A \link[tibble]{tibble} with the results of ...
#' @export
#' @importFrom rlang !!!
#'
data_eval <- function(.data = NULL, ..., .allow_null = FALSE) {
  args <- rlang::quos(...)

  # check NULLs, which tibble doesn't accept and transmute complains about
  is_null <- vapply(args, identical, FUN.VALUE = logical(1), rlang::quo(NULL))
  if(!.allow_null && any(is_null)) {
    missing_names <- names(args)[is_null]
    stop("NULL values in data_eval: ", paste0("'", missing_names, "'", collapse = ", "))
  }
  args <- args[!is_null]

  if(is.null(.data)) {
    tibble::tibble(!!!args)
  } else {
    tibble::as_tibble(dplyr::transmute(.data, !!!args))
  }
}
