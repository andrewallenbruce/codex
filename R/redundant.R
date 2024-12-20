#' Contrast Redundant Characters
#'
#' @param long `<character>` vector
#'
#' @param short `<character>` vector
#'
#' @returns `<character>` vector
#'
#' @examples
#' contrast(
#'    long = c("39550", "92083", "C5274"),
#'    short = c("3", "5", "M", "U"))
#'
#' @importFrom collapse %!in%
#'
#' @export
#'
#' @autoglobal
contrast <- function(long, short) {
  if (empty(long))
    return(character(0))

  if (empty(short))
    return(long)

  long[sf_sub(x = long,
              start = 1,
              stop = uniq_vlen(short)) %!in% short]
}

#' Alias for `contrast()`
#'
#' @inherit contrast
#'
#' @inheritDotParams contrast
#'
#' @export
#'
#' @autoglobal
rr <- \(...) contrast(...)

#' Remove Redundant Characters
#'
#' @param x `<list>` of vectors
#'
#' @returns `<list>` of vectors
#'
#' @examples
#' random_hcpcs(15) |>
#'    split_lengths() |>
#'    remove_redundant()
#'
#' @importFrom collapse .c %=%
#'
#' @export
#'
#' @autoglobal
remove_redundant <- function(x) {
  .c(x1, x2, x3, x4, x5) %=% x

  list(
    x1 = x1,
    x2 = rr(x2, x1),
    x3 = rr(x3, x1) |> rr(x2),
    x4 = rr(x4, x1) |> rr(x2) |> rr(x3),
    x5 = rr(x5, x1) |> rr(x2) |> rr(x3) |> rr(x4)
  )
}
