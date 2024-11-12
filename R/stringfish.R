#' Subset Vector by Range
#'
#' @param x `<character>` vector
#'
#' @param start `<integer>` index start; default is `1`
#'
#' @param stop `<integer>` index end
#'
#' @returns `<character>` vector
#'
#' @examples
#' sf_sub(random_hcpcs(2), 1, 2)
#'
#' @autoglobal
#'
#' @export
sf_sub <- \(x, start = 1, stop) stringfish::sf_substr(x, start = start, stop = stop, nthreads = 4L)

#' Convert string to stringfish vector
#'
#' @param x `<character>` vector
#'
#' @returns `<character>` stringfish vector
#'
#' @examples
#' sf_convert(random_hcpcs(50))
#'
#' @autoglobal
#'
#' @export
sf_convert <- \(x) stringfish::convert_to_sf(x)

#' Count number of characters in character vector
#'
#' @param x `<character>` vector
#'
#' @returns `<character>` stringfish vector
#'
#' @examples
#' sf_nchar(random_hcpcs())
#'
#' @autoglobal
#'
#' @export
sf_nchar <- \(x) stringfish::sf_nchar(x, nthreads = 4L)

#' Subset Vector at Index
#'
#' @param x `<character>` vector
#'
#' @param start `<integer>` index
#'
#' @returns `<character>` vector
#'
#' @examples
#' take_at(random_hcpcs(2), 2)
#'
#' @autoglobal
#'
#' @export
take_at <- \(x, start = 1) sf_sub(x, start = start, stop = start)

#' Detect by Regex
#'
#' @param s `<character>` vector
#'
#' @param p `<character>` regex pattern
#'
#' @returns `<logical>` vector
#'
#' @examples
#' sf_detect(random_hcpcs(), "[A-Z]{1}")
#'
#' @autoglobal
#'
#' @export
sf_detect <- \(s, p) stringfish::sf_grepl(s, p, nthreads = 4L)

#' Extract by Regex
#'
#' @param s `<character>` vector
#'
#' @param p `<character>` regex pattern
#'
#' @returns `<character>` vector
#'
#' @examples
#' sf_extract(random_hcpcs(), "[A-Z]{1}")
#'
#' @autoglobal
#'
#' @export
sf_extract <- \(s, p) s[sf_detect(s, p)]

#' Remove by Regex
#'
#' @param s `<character>` vector
#'
#' @param p `<character>` regex pattern
#'
#' @returns `<character>` vector
#'
#' @examples
#' sf_remove(LETTERS, "A")
#'
#' sf_remove(paste0(LETTERS, collapse = ""), "A")
#'
#' @autoglobal
#'
#' @export
sf_remove <- \(s, p) stringfish::sf_gsub(s, p, "", nthreads = 4L)

#' Concatenate Vectors
#'
#' @param ... Any number of vectors, coerced to `<character>` vector, if necessary
#'
#' @returns concatenated `<character>` vector
#'
#' @examples
#' sf_c(LETTERS, "A")
#'
#' @autoglobal
#'
#' @export
sf_c <- \(...) stringfish::sfc(...)
