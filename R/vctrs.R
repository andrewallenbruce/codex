#' Is Vector Empty?
#'
#' @param x vector
#'
#' @returns `<logical>`
#'
#' @examples
#' empty(character(0))
#'
#' empty(c())
#'
#' empty(NULL)
#'
#' empty(NA)
#'
#' empty(list())
#'
#' empty(list(character(0)))
#'
#' empty(list(x = character(0)))
#'
#' @autoglobal
#'
#' @export
empty <- \(x) vctrs::vec_is_empty(x)

#' Chop Vector by Group
#'
#' @param v `<character>` vector
#'
#' @param g `<integer>` group
#'
#' @returns `<list>`
#'
#' @examples
#' (v <- random_hcpcs(2))
#'
#' (g <- sample(1:2, size = length(v), replace = TRUE))
#'
#' gchop(v, g)
#' @autoglobal
#'
#' @export
gchop <- \(v, g) vctrs::vec_chop(x = v, sizes = vctrs::vec_run_sizes(g))
