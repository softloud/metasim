#' Get simulation-level parameters
#'
#' Get default arguments for simulation
#'
#' @examples
#' # check defaults
#' get_defaults()
#' # does it still return defaults?
#' get_defaults(a = 3)
#' @export

get_defaults <- function(...){
  formals() %>% {
    tibble(
      argument = names(.),
      value = as.list(.)
    )
  }

}

#' Get specified argments
#'
#' @examples
#' get_specified()
#' get_specified(a = 3)
#' get_specified(a = 3, b = "hello, world")
#' @export

get_specified <- function(...) {
  output <- match.call()

  if (!is.null(output %>% names())) {
    as.list(output)[-1] %>%  {
      tibble(argument = names(.),
             value = as.list(.))

    }
  } else {
    "no specified args"
  }}

#' Simulation-level parameters
#'
#' @examples
#' get_arguments()
#' get_arguments(a = 3, x = "1")
#'
#' @export

get_arguments <- function(...) {
  match_call <- match.call()
  names_match_call <- names(match_call)

  specified_args <- if (!is.null(names_match_call)) {
    as.list(match_call)[-1] %>%  {
      tibble(argument = names(.),
             specified = as.list(.))

    }
  } else {
    "no specified args"
  }

  defaults <- formals() %>% {
    tibble(argument = names(.),
           default = as.list(.))
  }


  output <- if (!is.character(specified_args)) {
    defaults %>%
      full_join(specified_args, by = "argument") %>%
      mutate(default_flag = map_lgl(specified, is.null),
             value_raw = if_else(
               default_flag,
               default,
               specified
             )) %>%
      arrange(argument) %>%
      mutate(value = case_when(
        class(value_raw) == "language" ~ eval(value_raw),
        TRUE ~ value_raw
      ))

  } else {defaults}

  return(output %>% filter(as.character(argument) != "..."))
}

