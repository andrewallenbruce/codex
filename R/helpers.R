#' Unique Lengths
#' @param x vector
#' @returns vector
#' @autoglobal
#' @keywords internal
#' @export
unique_vlength <- \(x) collapse::funique(collapse::vlengths(x, use.names = FALSE))

#' Unique Values with NAs Removed
#' @param x vector
#' @returns vector
#' @autoglobal
#' @keywords internal
#' @export
unique_narm <- \(x) collapse::funique(collapse::na_rm(x))

#' Max of Lengths
#' @param x vector
#' @returns integer
#' @autoglobal
#' @keywords internal
#' @export
max_vlength <- \(x) collapse::fmax(collapse::vlengths(x, use.names = FALSE))

#' Is Empty
#' @param x vector
#' @returns logical
#' @autoglobal
#' @keywords internal
#' @export
empty <- \(x) vctrs::vec_is_empty(x)

#' Chop Vector by Group
#' @param v vector
#' @param g group
#' @returns vector
#' @autoglobal
#' @keywords internal
#' @export
chop <- \(v, g) vctrs::vec_chop(v, sizes = vctrs::vec_run_sizes(g))

#' Subset Vector at Index
#' @param x vector
#' @param i index
#' @returns vector
#' @autoglobal
#' @keywords internal
#' @export
take_at <- \(x, i = 1) stringfish::sf_substr(x, start = i, stop = i, nthreads = 4L)

#' Subset Vector by Range
#' @param x vector
#' @param i index start
#' @param z index end
#' @returns vector
#' @autoglobal
#' @keywords internal
#' @export
sf_sub <- \(x, i = 1, z) stringfish::sf_substr(x, start = i, stop = z, nthreads = 4L)

#' Extract by Regex
#' @param s subject
#' @param p pattern
#' @returns vector
#' @autoglobal
#' @keywords internal
#' @export
sf_extract <- \(s, p) s[stringfish::sf_grepl(s, p, nthreads = 4L)]

#' Detect by Regex
#' @param s subject
#' @param p pattern
#' @returns vector
#' @autoglobal
#' @keywords internal
#' @export
sf_detect <- \(s, p) stringfish::sf_grepl(s, p, nthreads = 4L)

#' Remove by Regex
#' @param s subject
#' @param p pattern
#' @returns vector
#' @autoglobal
#' @keywords internal
#' @export
sf_remove <- \(s, p) stringfish::sf_gsub(s, p, "", nthreads = 4L)

#' Sort Order
#' @param x vector
#' @returns vector
#' @autoglobal
#' @keywords internal
#' @export
sort_order <- \(x) {

  sort <- stringr::str_sort(x, numeric = TRUE)
  alph <- stringr::str_extract_all(sort, stringr::regex("[A-Z]"))
  numb <- stringr::str_extract_all(sort, stringr::regex("[0-9]"))
  alph <- purrr::list_c(alph)
  numb <- purrr::list_c(numb)

  paste0(
    paste0(alph, collapse = ""),
    paste0(numb, collapse = "")
  )
}

#' Lump Like Vectors Together
#' @param x vector
#' @returns vector
#' @autoglobal
#' @keywords internal
#' @export
lump <- function(x, threshold = 3) {

  stopifnot(is.numeric(x))

  xo <- order(x)

  xs <- x[xo]

  dlag <- abs(c(0, xs[-1] - xs[seq_along(xs) - 1]))

  bi <- ifelse(dlag >= threshold, 1, 0)

  id <- cumsum(bi) + 1

  id[xo]
}

#' Convert Letters to Integers
#' @param x vector of letters
#' @examples
#' letters_to_numbers(LETTERS)
#' @returns vector of integers
#' @autoglobal
#' @keywords internal
#' @export
letters_to_numbers <- \(x) {

  unname(
    setNames(
      seq_along(LETTERS), LETTERS)[
      sf_extract(x, "[A-Z]{1}")]
    )
}
