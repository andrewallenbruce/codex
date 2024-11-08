#' Split Character Vector by Lengths
#'
#' @param x `<character>` vector
#'
#' @param verbose `<logical>` print output
#'
#' @returns `<list>` of character vectors
#'
#' @examples
#' random_hcpcs(5) |>
#'    split_lengths()
#'
#' @autoglobal
#'
#' @export
split_lengths <- function(x, verbose = FALSE) {

  stopifnot(is.character(x))

  x <- sf_remove(x, "\\*|\\s") |>
    uniq_narm() |>
    stringr::str_sort()

  l <- vlen(x)

  out <- list(
    x1 = x[l == 1],
    x2 = x[l == 2],
    x3 = x[l == 3],
    x4 = x[l == 4],
    x5 = x[l == 5])

  if (verbose) {
    return(invisible(out))
  } else {
    return(out)
  }
}

#' Split Character Vector at End
#'
#' @param x `<character>` vector
#'
#' @returns `<list>` of character vectors
#'
#' @examples
#' random_hcpcs(10) |>
#'    split_end()
#'
#' @autoglobal
#'
#' @export
split_end <- \(x) {

  if (any(sf_detect(x, "[A-Z]$"))) {

    end <- sf_extract(x, "[A-Z]$")
    beg <- glue::glue("{take_at(end, 5)}{sf_remove(end, '[A-Z]$')}")

    c(split_at(x = x[!sf_detect(x, "[A-Z]$")], i = 1, z = 1),
      split_at(x = end, y = beg, i = 1, z = 2))

  } else {

    split_at(x, i = 1, z = 1)

  }
}

#' Split Character Vector by Lengths
#'
#' @param x `<character>` vector
#'
#' @param verbose `<logical>` print output
#'
#' @returns `<list>` of character vectors
#'
#' @examples
#' random_hcpcs(5) |>
#'    split_lengths() |>
#'    remove_redundant() |>
#'    split_first()
#'
#' @importFrom collapse %!in% .c
#'
#' @autoglobal
#'
#' @export
split_first <- function(x, verbose = FALSE) {

  .c(x1, x2, x3, x4, x5) %=% x

  out <- list(
    x1 = list(x1),
    x2 = split_1(x2),
    x3 = split_1(x3),
    x4 = split_1(x4),
    x5 = split_end(x5))

  if (verbose) {
    return(invisible(out))
  } else {
    return(out)
  }
}
