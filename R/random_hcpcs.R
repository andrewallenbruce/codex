#' Generate Random HCPCS Codes Vector
#'
#' @param n <integer> number of codes
#'
#' @returns <character> vector
#'
#' @examples
#' random_hcpcs(n = 5)
#' @export
#'
#' @autoglobal
random_hcpcs <- function(n = 10) {

  h <- stringfish::convert_to_sf(get_pin("hcpcs_vec"))

  ltrs <- LETTERS[stringfish::sf_grepl(LETTERS, "[^DINOW-Z]", nthreads = 4L)]

  c(
    sample(c(ltrs, 0:9), size = sample.int(5, 1)),
    sfsub(sample(h, n), z = 2),
    sfsub(sample(h, n), z = 3),
    sfsub(sample(h, n), z = 4),
    sfsub(sample(h, n), z = 5)
  )
}
