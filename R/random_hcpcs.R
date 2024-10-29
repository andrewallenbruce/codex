#' Generate Random HCPCS Codes
#'
#' @param n <integer> number of codes
#'
#' @param l <integer> length of codes
#'
#' @returns <character> vector
#'
#' @examples
#' random_hcpcs(n = 5, l = 5)
#'
#' @autoglobal
#'
#' @export
random_hcpcs <- \(n, l) {

  h <- stringfish::convert_to_sf(
    collapse::funique(
      northstar::search_descriptions()$hcpcs_code))

  stringfish::sf_substr(
    sample(h, size = n),
    start = 1,
    stop = l,
    nthreads = 4L)
}

#' Generate Random HCPCS Codes Vector
#'
#' @param n <integer> number of codes
#'
#' @returns <character> vector
#'
#' @examples
#' random_hcpcs_vec(n = 5)
#' @export
#'
#' @autoglobal
random_hcpcs_vec <- \(n = 10) {

  nn <- if (n >= 10) 10 else n

  c(
    sample(
      c(LETTERS[
        stringfish::sf_grepl(
          LETTERS, "[^DINOW-Z]", nthreads = 4L)], 0:9),
      size = nn),
    random_hcpcs(n = n, l = 2),
    random_hcpcs(n = n, l = 3),
    random_hcpcs(n = n, l = 4),
    random_hcpcs(n = n, l = 5)
  )

}
