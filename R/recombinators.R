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
#' @importFrom stringr str_glue
#'
#' @autoglobal
#'
#' @export
recombine2 <- function(x) {
  x <- getelem(x, "g2")

  if (empty(x))
    return(character(0))

  x <- sf_smush(list_c(x), sep = "|")

  str_glue("({x})")
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
#' @importFrom purrr map list_flatten
#' @importFrom glue as_glue
#'
#' @autoglobal
#'
#' @export
recombine3 <- function(x) {
  x <- getelem(x, "g3")

  if (empty(x))
    return(character(0))

  list(if (empty(x[vlen(x) == 1]))
    NULL
    else
      parentheses(sf_smush(delist(x[vlen(x) == 1]), sep = "|")) |> as_glue(),
    if (empty(x[vlen(x) > 1]))
      NULL
    else
      map(x[vlen(x) > 1], function(x) {
        front <- uniq(sf_sub(delist(x), start = 1, stop = 1))

        back  <- sf_smush(sf_remove(delist(x), str_glue("^[{front}]{{1}}")), sep = "|")

        str_glue("{front}({back})")

      })) |> list_flatten()
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
#' @importFrom purrr map list_flatten
#' @importFrom glue as_glue
#'
#' @autoglobal
#'
#' @export
recombine4 <- function(x) {
  x <- getelem(x, "g4")

  if (empty(x))
    return(character(0))

  list(if (empty(x[vlen(x) == 1]))
    NULL
    else
      parentheses(sf_smush(delist(x[vlen(x) == 1]), sep = "|")) |> as_glue(),
    if (empty(x[vlen(x) > 1]))
      NULL
    else
      map(x[vlen(x) > 1], function(x) {
        front <- uniq(sf_sub(delist(x), start = 1, stop = 1))

        back  <- sf_smush(sf_remove(delist(x), str_glue("^[{front}]{{1}}")), sep = "|")

        str_glue("{front}({back})")

      })) |> list_flatten()
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
#' @importFrom purrr map list_flatten
#' @importFrom glue as_glue
#'
#' @autoglobal
#'
#' @export
recombine5 <- function(x) {
  x <- getelem(x, "g5")

  if (empty(x))
    return(character(0))

  list(if (empty(x[vlen(x) == 1]))
    NULL
    else
      parentheses(sf_smush(delist(x[vlen(x) == 1]), sep = "|")) |> as_glue(),
    if (empty(x[vlen(x) > 1]))
      NULL
    else
      map(x[vlen(x) > 1], function(x) {
        front <- uniq(sf_sub(delist(x), start = 1, stop = 1))

        back  <- sf_smush(sf_remove(delist(x), str_glue("^[{front}]{{1}}")), sep = "|")

        str_glue("{front}({back})")

      })) |> list_flatten()
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
