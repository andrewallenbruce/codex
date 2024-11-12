#' Generate Random HCPCS Codes Vector
#'
#' @param n `<integer>` number of codes; default is `10`
#'
#' @returns `<character>` vector
#'
#' @examples
#' random_hcpcs()
#' @export
#'
#' @autoglobal
random_hcpcs <- function(n = 10) {

  h <- sf_convert(get_pin("hcpcs_vec"))

  c(sample(c(sf_extract(LETTERS, "[^DINOW-Z]"), 0:9), size = sample.int(5, 1)),
    sf_sub(sample(h, n), stop = 2),
    sf_sub(sample(h, n), stop = 3),
    sf_sub(sample(h, n), stop = 4),
    sf_sub(sample(h, n), stop = 5)
  )
}

#' Generate Random HCPCS Codes Vector (Version 2)
#'
#' @param n `<integer>` number of codes; default is `10`
#'
#' @returns `<character>` vector
#'
#' @examples
#' random_hcpcs2()
#' @export
#'
#' @autoglobal
random_hcpcs2 <- function(n = 10) {

  p <- sf_c(sf_extract(LETTERS, "[^DINOW-Z]"), 0:9)

  h <- sf_convert(get_pin("hcpcs_vec"))

  sf_convert(
    c(
      cheapr::sample_(x = p, size = sample.int(5, 1)),
      sf_sub(cheapr::sample_(x = h, size = n), stop = 2),
      sf_sub(cheapr::sample_(x = h, size = n), stop = 3),
      sf_sub(cheapr::sample_(x = h, size = n), stop = 4),
      sf_sub(cheapr::sample_(x = h, size = n), stop = 5)
      )
    )

}
