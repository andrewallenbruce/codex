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
    stringfish::sf_substr(sample(h, n), 1, 2, nthreads = 4L),
    stringfish::sf_substr(sample(h, n), 1, 3, nthreads = 4L),
    stringfish::sf_substr(sample(h, n), 1, 4, nthreads = 4L),
    stringfish::sf_substr(sample(h, n), 1, 5, nthreads = 4L)
  )
}

#' Generate Random HCPCS Codes
#'
#' @param n <integer> number of codes
#'
#' @param l <integer> length of codes
#'
#' @returns <character> vector
#'
#' @examples
#' random_hcpcs2(n = 5, l = 5)
#'
#' @autoglobal
#'
#' @noRd
random_hcpcs2 <- function(n, l) {

  h <- stringfish::convert_to_sf(
    get_pin("hcpcs_vec")
  )

  stringfish::sf_substr(
    sample(h, size = n),
    start = 1,
    stop = l,
    nthreads = 4L)
}
