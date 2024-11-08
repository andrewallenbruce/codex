#' Generate Random HCPCS Codes Vector
#'
#' @param n `<integer>` number of codes
#'
#' @returns `<character>` vector
#'
#' @examples
#' random_hcpcs(n = 5)
#' @export
#'
#' @autoglobal
random_hcpcs <- function(n = 10) {

  h <- sf_convert(get_pin("hcpcs_vec"))

  c(sample(c(sf_extract(LETTERS, "[^DINOW-Z]"), 0:9), size = sample.int(5, 1)),
    sf_sub(sample(h, n), z = 2),
    sf_sub(sample(h, n), z = 3),
    sf_sub(sample(h, n), z = 4),
    sf_sub(sample(h, n), z = 5)
  )
}
