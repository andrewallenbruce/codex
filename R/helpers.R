#' @noRd
unique_vlength <- \(x) collapse::funique(collapse::vlengths(x, use.names = FALSE))

#' @noRd
unique_narm <- \(x) collapse::funique(collapse::na_rm(x))

#' @noRd
max_vlength <- \(x) collapse::fmax(collapse::vlengths(x, use.names = FALSE))

#' @noRd
empty <- \(x) vctrs::vec_is_empty(x)

#' @noRd
chop <- \(v, g) vctrs::vec_chop(v, sizes = vctrs::vec_run_sizes(g))

#' @noRd
take_at <- \(x, i = 1) stringfish::sf_substr(x, start = i, stop = i, nthreads = 4L)

#' @noRd
detect_az <- \(x) x[stringfish::sf_grepl(x, "[A-Z]{1}", nthreads = 4L)]
