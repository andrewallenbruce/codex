#' Split Character Vector by Lengths
#'
#' @param x <character> vector
#'
#' @param verbose <logical> print output
#'
#' @returns <list> of character vectors
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
    unique_narm() |>
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
