#' Recombine Length 2
#'
#' @param x `<list>` of character vectors
#'
#' @returns `<character>` string
#'
#' @examples
#' random_hcpcs() |>
#'    split_lengths() |>
#'    remove_redundant() |>
#'    split_first() |>
#'    process_groups() |>
#'    reduce_groups() |>
#'    recombine2()
#'
#' @importFrom purrr list_c
#'
#' @autoglobal
#'
#' @export
recombine2 <- function(x) {

  x <- getelem(x, "g2")

  if (empty(x))
    return(character(0))

  paste0(
    "^(",
    sf_smush(
      list_c(x),
      sep = "|"),
    ")[A-Z0-9]{3}$"
    )
}

#' Recombine Length 3
#'
#' @param x `<list>` of character vectors
#'
#' @returns `<character>` string
#'
#' @examples
#' random_hcpcs() |>
#'    split_lengths() |>
#'    remove_redundant() |>
#'    split_first() |>
#'    process_groups() |>
#'    reduce_groups() |>
#'    recombine3()
#'
#' @importFrom stringr str_glue
#' @importFrom purrr map
#'
#' @autoglobal
#'
#' @export
recombine3 <- function(x) {

  x <- getelem(x, "g3")

  if (empty(x)) return(character(0))

  c(
    if (empty(x[vlen(x) == 1])) character(0) else str_glue("{list_c(x[vlen(x) == 1])}"),

    if (empty(x[vlen(x) > 1])) character(0) else map(x[vlen(x) > 1], function(x) {

          front <- uniq(sf_sub(delist(x), start = 1, stop = 1))

          back  <- sf_smush(sf_remove(delist(x), str_glue("^[{front}]{{1}}")), sep = "|")

          str_glue("{front}({back})")
        })
    )
}

#' Recombine Length 4
#'
#' @param x `<list>` of character vectors
#'
#' @returns `<character>` string
#'
#' @examples
#' random_hcpcs() |>
#'    split_lengths() |>
#'    remove_redundant() |>
#'    split_first() |>
#'    process_groups() |>
#'    reduce_groups() |>
#'    recombine4()
#'
#' @importFrom stringr str_glue
#' @importFrom purrr map
#'
#' @autoglobal
#'
#' @export
recombine4 <- function(x) {

  x <- getelem(x, "g4")

  if (empty(x)) return(character(0))

  c(
    if (empty(x[vlen(x) == 1])) character(0) else str_glue("{list_c(x[vlen(x) == 1])}"),

    if (empty(x[vlen(x) > 1])) character(0) else map(x[vlen(x) > 1], function(x) {

      front <- uniq(sf_sub(delist(x), start = 1, stop = 1))

      back  <- sf_smush(sf_remove(delist(x), str_glue("^[{front}]{{1}}")), sep = "|")

      str_glue("{front}({back})")
    })
  )
}

#' Recombine Length 5
#'
#' @param x `<list>` of character vectors
#'
#' @returns `<character>` string
#'
#' @examples
#' random_hcpcs() |>
#'    split_lengths() |>
#'    remove_redundant() |>
#'    split_first() |>
#'    process_groups() |>
#'    reduce_groups() |>
#'    recombine5()
#'
#' @importFrom stringr str_glue
#' @importFrom purrr map
#'
#' @autoglobal
#'
#' @export
recombine5 <- function(x) {

  x <- getelem(x, "g5")

  if (empty(x)) return(character(0))

  c(
    if (empty(x[vlen(x) == 1])) character(0) else str_glue("{list_c(x[vlen(x) == 1])}"),

    if (empty(x[vlen(x) > 1])) character(0) else map(x[vlen(x) > 1], function(x) {

      front <- uniq(sf_sub(delist(x), start = 1, stop = 1))

      back  <- sf_smush(sf_remove(delist(x), str_glue("^[{front}]{{1}}")), sep = "|")

      str_glue("{front}({back})")
    })
  )
}

#' Recombine Groups
#'
#' @param x `<list>` of character vectors
#'
#' @returns `<list>` of character vectors
#'
#' @examples
#' random_hcpcs() |>
#'    split_lengths() |>
#'    remove_redundant() |>
#'    split_first() |>
#'    process_groups() |>
#'    reduce_groups() |>
#'    recombine_groups()
#' @autoglobal
#'
#' @export
recombine_groups <- function(x) {

  list(
    v1 = x$g1,
    v2 = recombine2(x),
    v3 = recombine3(x),
    v4 = recombine4(x),
    v5 = recombine5(x)
  )
}
