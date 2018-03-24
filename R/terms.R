
tidy_terms <- function(...) {
  args <- rlang::quos(...)

  lst <- purrr::map(args, function(x) {
    if(rlang::quo_is_call(x)) {
      result <- try(rlang::eval_tidy(x), silent = TRUE)
      if(inherits(result, "term_list")) {
        return(unclass(result))
      } else if(inherits(result, "tidy_terms")) {
        return(result)
      }
    }

    x
  })

  lst <- do.call(purrr::splice, lst)
  class(lst) <- c("term_list", "tidy_terms")
  lst
}

formula.term_list <- function(x, env = parent.frame(), ...) {
  txt <- paste(as.character(x), collapse = " + ")
  exp <- parse(text = txt)
  form <- ~exp
  form[[2]] <- exp[[1]]
  environment(form) <- env
  form
}

as.character.term_list <- function(x, ...) {
  lst <- purrr::map(x, function(item) {
    if(rlang::is_quosure(item)) {
      term_text(item)
    } else {
      as.character(item)
    }
  })
  unlist(lst)
}

print.term_list <- function(x, ...) {
  cat("<tidy_terms>\n")
  cat(paste(as.character(x), collapse = " + "))
}

crossed <- function(...) {
  args <- rlang::quos(...)
  class(args) <- c("term_crossed", "tidy_terms")
  args
}

as.character.term_crossed <- function(x, ...) {
  chrs <- purrr::map_chr(x, term_text)
  paste(chrs, collapse = "*")
}

nested <- function(x, y) {
  args <- list(rlang::enquo(x), rlang::enquo(y))
  class(args) <- c("term_nested", "term")
  args
}

as.character.term_nested <- function(x, ...) {
  chrs <- purrr::map_chr(x, term_text)
  paste(chrs, collapse = " %in% ")
}

interacted <- function(...) {
  args <- rlang::quos(...)
  class(args) <- c("term_interacted", "term")
  args
}

as.character.term_interacted <- function(x, ...) {
  chrs <- purrr::map_chr(x, term_text)
  paste(chrs, collapse = ":")
}

expand_terms <- function(x) {
  UseMethod("expand_terms")
}

expand_terms.default <- function(x) {
  x
}

expand_terms.term_list <- function(x) {
  args <- purrr::map(x, expand_terms)
  lst <- do.call(purrr::splice, args)
  structure(lst, class = c("term_list", "tidy_terms"))
}

expand_terms.term_crossed <- function(args) {
  combinations <- purrr::map(seq_along(args), function(m) {
    combn <- utils::combn(seq_along(args), m)
    purrr::map(seq_len(ncol(combn)), function(x) {
      quos_list <- args[combn[, x, drop = TRUE]]
      interacted(!!!quos_list)
    })
  })
  do.call(purrr::splice, combinations)
}

term_text <- function(quo_obj) {
  # this should really get fixed to be way faster
  exp <- rlang::get_expr(quo_obj)

  tf <- tempfile()
  withr::with_tempfile(tf, {
    dput(exp, file = tf)
    paste(readLines(tf), collapse = " ")
  })
}
