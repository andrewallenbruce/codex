#' Lump Like Vectors Together
#'
#' @param x <numeric> vector
#'
#' @param threshold <numeric> threshold; default is `3`
#'
#' @examples
#' lump(c(1:3, 5, 8:10, 15))
#'
#' @returns <numeric> vector
#'
#' @autoglobal
#'
#' @export
lump <- \(x, threshold = 3) {

  stopifnot(exprs = {
    is.numeric(x)
    is.numeric(threshold)
  })

  xo <- order(x)

  xs <- x[xo]

  dlag <- abs(c(0, xs[-1] - xs[seq_along(xs) - 1]))

  bi <- ifelse(dlag >= threshold, 1, 0)

  id <- cumsum(bi) + 1

  id[xo]
}

#' Jaccard Similarity
#'
#' Calculate the Jaccard index between two sets.
#'
#' The Jaccard index, also known as the Jaccard similarity coefficient, measures
#' the similarity between two sets by comparing the size of their intersection
#' to the size of their union.
#'
#' @param x,y vectors representing the two sets.
#'
#' @returns <numeric> value
#'
#' @examples
#' jaccard(c(1, 2, 3), c(2, 3, 4))
#'
#' jaccard(factor(c(1, 2, 3)), factor(c(2, 3, 4)))
#'
#' jaccard(c(1), c())
#'
#' @autoglobal
#'
#' @export
jaccard <- function(x, y) {

  intersection <- length(intersect(x, y))

  union <- length(x) + length(y) - intersection

  if (union == 0) return(0)

  intersection / union
}
