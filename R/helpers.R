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

#' Collapse a vector to length 1
#'
#' @param ... A split `<chr>` vector
#'
#' @returns A collapsed `<chr>` string
#'
#' @examples
#' smush(c("X", "Y", "Z"))
#'
#' smush(random_hcpcs(2))
#'
#' @autoglobal
#'
#' @export
smush <- \(...) paste0(..., collapse = "")

#' Unlist with no names
#'
#' @param x A named or unnamed `<list>`
#'
#' @returns An unnamed `<chr>` vector
#'
#' @examples
#' delist(list(x = "XYZ"))
#'
#' delist(list("XYZ"))
#'
#' @autoglobal
#'
#' @export
delist <- \(x) unlist(x, use.names = FALSE)

#' Delist, Unname and Split a String
#'
#' @param x `<chr>` string or named `<list>` of `<chr>` strings
#'
#' @returns An unnamed `<list>` of split `<chr>` vectors
#'
#' @examples
#' # unnamed vector
#' splits("XYZ")
#'
#' # named vector
#' splits(c(x = "XYZ"))
#'
#' # unnamed list with one element
#' splits(list("XYZ"))
#'
#' # unnamed list with multiple elements
#' splits(list("YYY", "ZZZ"))
#'
#' # named list with one element
#' splits(list(x = "XYZ"))
#'
#' # named list with multiple elements
#' splits(list(x = "YYY", xx = "ZZZ"))
#'
#' @autoglobal
#'
#' @export
splits <- \(x) {

  res <- strsplit(delist(x), "")

  if (length(res) == 1) return(res[[1]])

  res
}

#' Wrap A String in Brackets
#'
#' @param x `<chr>` string
#'
#' @returns `<chr>` string
#'
#' @examples
#' bracket("XYZ")
#'
#' @autoglobal
#'
#' @export
bracket <- \(x) paste0(r"--{[}--", x, r"--{]}--")

#' Wrap A String in Parentheses
#'
#' @param x `<chr>` string
#'
#' @returns `<chr>` string
#'
#' @examples
#' parent("XYZ")
#'
#' @autoglobal
#'
#' @export
parent <- \(x) paste0(r"--{(}--", x, r"--{)}--")

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

  smush(
    smush(alph),
    smush(numb)
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
