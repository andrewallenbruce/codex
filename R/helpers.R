#' Lengths of Vector
#'
#' @param x vector
#'
#' @returns vector
#'
#' @examples
#' random_hcpcs(5) |> vlen()
#'
#' @autoglobal
#'
#' @export
vlen <- \(x) collapse::vlengths(x, use.names = FALSE)

#' Unique Lengths of Vector
#'
#' @param x vector
#'
#' @returns vector
#'
#' @examples
#' random_hcpcs(5) |> unique_vlen()
#'
#' @autoglobal
#'
#' @export
unique_vlen <- \(x) collapse::funique(vlen(x))

#' Unique Values with NAs Removed
#'
#' @param x vector
#'
#' @returns vector
#'
#' @examples
#' unique_narm(c("A", NA, "A", 1))
#'
#' @autoglobal
#'
#' @export
unique_narm <- \(x) collapse::funique(collapse::na_rm(x))

#' Maximum Vector Length
#'
#' @param x vector
#'
#' @returns integer
#'
#' @examples
#' random_hcpcs(5) |> max_vlen()
#'
#' @autoglobal
#'
#' @export
max_vlen <- \(x) collapse::fmax(vlen(x))

#' Is Vector Empty?
#'
#' @param x vector
#'
#' @returns logical
#'
#' @examples
#' empty(character(0))
#'
#' empty(c())
#'
#' empty(NULL)
#'
#' empty(NA)
#'
#' @autoglobal
#'
#' @export
empty <- \(x) vctrs::vec_is_empty(x)

#' Chop Vector by Group
#'
#' @param v vector
#'
#' @param g group
#'
#' @returns vector
#'
#' @examples
#' (v <- random_hcpcs(2))
#'
#' (g <- sample(1:2, size = length(v), replace = TRUE))
#'
#' chop(v, g)
#' @autoglobal
#'
#' @export
chop <- \(v, g) vctrs::vec_chop(v, sizes = vctrs::vec_run_sizes(g))

#' Subset Vector by Range
#'
#' @param x vector
#'
#' @param i index start
#'
#' @param z index end
#'
#' @returns vector
#'
#' @examples
#' sf_sub(random_hcpcs(2), 1, 2)
#'
#' @autoglobal
#'
#' @export
sf_sub <- \(x, i = 1, z) stringfish::sf_substr(x, start = i, stop = z, nthreads = 4L)

#' Subset Vector at Index
#'
#' @param x vector
#'
#' @param i index
#'
#' @returns vector
#'
#' @examples
#' take_at(random_hcpcs(2), 2)
#'
#' @autoglobal
#'
#' @export
take_at <- \(x, i = 1) sf_sub(x, i = i, z = i)

#' Detect by Regex
#'
#' @param s subject
#'
#' @param p regex pattern
#'
#' @returns [logical] vector
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
#' @param s subject
#'
#' @param p regex pattern
#'
#' @returns vector
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
#' @param s subject
#'
#' @param p regex pattern
#'
#' @returns vector
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

#' Sort and Order Vector
#'
#' @param x [character] vector
#'
#' @returns vector
#'
#' @examples
#' (x <- sample(c(LETTERS, 0:9), size = 10))
#'
#' sort_order(x)
#'
#' @autoglobal
#'
#' @export
sort_order <- \(x) {

  sort <- stringr::str_sort(x, numeric = TRUE)

  alph <- purrr::list_c(
    stringr::str_extract_all(
      sort,
      stringr::regex("[A-Z]")
      )
    )

  numb <- purrr::list_c(
    stringr::str_extract_all(
      sort, stringr::regex("[0-9]")
      )
    )

  paste0(
    paste0(alph, collapse = ""),
    paste0(numb, collapse = "")
  )
}

#' Convert Letters to Integers
#'
#' @param x [vector] of letters
#'
#' @examples
#' letters_to_numbers(LETTERS)
#'
#' @returns vector of integers
#'
#' @importFrom stats setNames
#'
#' @autoglobal
#'
#' @export
letters_to_numbers <- \(x) {

  stopifnot(is.character(x))

  unname(
    setNames(
      seq_along(LETTERS), LETTERS)[
      sf_extract(x, "[A-Z]{1}")]
    )
}
