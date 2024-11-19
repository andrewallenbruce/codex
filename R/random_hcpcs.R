#' Generate Random HCPCS Codes Vector
#'
#' @param n `<integer>` number of codes; default is `10`
#'
#' @param lng `<integer>` vector of lengths; default is `c(1:5)`
#'
#' @returns `<character>` vector
#'
#' @examples
#' random_hcpcs()
#'
#' random_hcpcs(50, 3)
#'
#' @importFrom cheapr sample_
#'
#' @export
#'
#' @autoglobal
random_hcpcs <- function(n = 10, lng = c(1:5)) {
  p <- sf_c(sf_extract(LETTERS, "[^DINOW-Z]"), 0:9)

  h <- sf_convert(get_pin("hcpcs_vec"))

  sf_convert(
    sf_c(
      if (any(lng == 1))
        sample_(x = p, size = sample.int(5, 1))
      else
        NULL,
      if (any(lng == 2))
        sf_sub(sample_(x = h, size = n), stop = 2)
      else
        NULL,
      if (any(lng == 3))
        sf_sub(sample_(x = h, size = n), stop = 3)
      else
        NULL,
      if (any(lng == 4))
        sf_sub(sample_(x = h, size = n), stop = 4)
      else
        NULL,
      if (any(lng == 5))
        sf_sub(sample_(x = h, size = n), stop = 5)
      else
        NULL
    )
  )

}
