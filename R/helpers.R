#' If Else wrapper using [kit::iif()]
#'
#' @param x `<logical>` vector
#'
#' @param yes,no Values to return depending on TRUE/FALSE element of `x`. Must
#'   be same type and be either length 1 or same length of `x`.
#'
#' @returns vector of same length as `x` and attributes as `yes`. Data values
#'          are taken from values of `yes` and `no`.
#'
#' @examples
#' x <- c(1:4, 3:2, 1:4)
#'
#' iif_else(x > 2L, x, x - 1L)
#'
#' @autoglobal
#'
#' @export
iif_else <- \(x, yes, no) kit::iif(test = x, yes = yes, no = no, nThread = 4L)

#' Predicate to filter out NAs
#'
#' @param x vector
#'
#' @returns `<logical>` vector
#'
#' @examples
#' c(NA, "AA") |> not_na()
#'
#' @autoglobal
#'
#' @export
not_na <- \(x) !cheapr::is_na(x)

#' Get named element from list
#'
#' @param ll named `<list>`
#'
#' @param nm `<character>` element name
#'
#' @returns named `<list>` element
#'
#' @examples
#' list(x1 = NA, x2 = "AA") |> getem("x2")
#'
#' @autoglobal
#'
#' @export
getem <- \(ll, nm) collapse::get_elem(l = ll, elem = nm, regex = TRUE)

#' Lengths of Vector
#'
#' @param x vector
#'
#' @returns `<integer>` vector
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
#' @returns `<integer>` vector
#'
#' @examples
#' random_hcpcs(5) |> uniq_vlen()
#'
#' @autoglobal
#'
#' @export
uniq_vlen <- \(x) collapse::funique(vlen(x))

#' Unique Values with NAs Removed
#'
#' @param x vector
#'
#' @returns vector
#'
#' @examples
#' uniq_narm(c("A", NA, "A", 1))
#'
#' @autoglobal
#'
#' @export
uniq_narm <- \(x) collapse::funique(collapse::na_rm(x))

#' Maximum Vector Length
#'
#' @param x vector
#'
#' @returns `<integer>` vector
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
#' @returns `<logical>`
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
#' empty(list())
#'
#' @autoglobal
#'
#' @export
empty <- \(x) vctrs::vec_is_empty(x)

#' Chop Vector by Group
#'
#' @param v `<character>` vector
#'
#' @param g `<integer>` group
#'
#' @returns `<list>`
#'
#' @examples
#' (v <- random_hcpcs(2))
#'
#' (g <- sample(1:2, size = length(v), replace = TRUE))
#'
#' gchop(v, g)
#' @autoglobal
#'
#' @export
gchop <- \(v, g) vctrs::vec_chop(v, sizes = vctrs::vec_run_sizes(g))

#' Subset Vector by Range
#'
#' @param x `<character>` vector
#'
#' @param i `<integer>` index start; default is `1`
#'
#' @param z `<integer>` index end
#'
#' @returns `<character>` vector
#'
#' @examples
#' sf_sub(random_hcpcs(2), 1, 2)
#'
#' @autoglobal
#'
#' @export
sf_sub <- \(x, i = 1, z) stringfish::sf_substr(x, start = i, stop = z, nthreads = 4L)

#' Convert string to stringfish vector
#'
#' @param x `<character>` vector
#'
#' @returns `<character>` stringfish vector
#'
#' @examples
#' sf_convert(random_hcpcs(500))
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
#' @param i `<integer>` index
#'
#' @returns `<character>` vector
#'
#' @examples
#' take_at(random_hcpcs(2), 2)
#'
#' @autoglobal
#'
#' @export
take_at <- \(x, i = 1) sf_sub(x, i = i, z = i)

#' Split Vector by Index Subset
#'
#' @param x `<character>` vector
#'
#' @param y `<character>` vector; default is `x`
#'
#' @param i `<integer>` index start
#'
#' @param z `<integer>` index end
#'
#' @returns `<character>` vector
#'
#' @examples
#' x <- c("0741T", "E0628", "L4387", "0360T", "1127F", "0002M")
#'
#' (end <- sf_extract(x, "[A-Z]$"))
#'
#' (beg <- paste0(take_at(end, 5), sf_remove(end, "[A-Z]$")))
#'
#' split_at(x = end, y = beg, i = 1, z = 2)
#'
#' @autoglobal
#'
#' @export
split_at <- \(x, y = x, i, z) collapse::rsplit(x, sf_sub(y, i, z), use.names = FALSE)

#' Split Vector
#'
#' @param x `<character>` vector
#'
#' @returns `<character>` vector
#'
#' @examples
#' split_1(c("AA", strrep(LETTERS[1:5], 2), "Z"))
#'
#' @autoglobal
#'
#' @export
split_1 <- \(x) split_at(x, y = x, i = 1, z = 1)

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

#' Collapse a vector to length 1
#'
#' @param ... `<character>` string
#'
#' @returns collapsed `<character>` string
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
#' @param x Named or unnamed `<list>`
#'
#' @returns Unnamed `<character>` vector
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
#' @param x `<character>` string or named `<list>`
#'
#' @returns Unnamed `<list>` of split `<character>` vectors
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
#' @param x `<character>` string
#'
#' @returns `<character>` string
#'
#' @examples
#' bracket("XYZ")
#'
#' @autoglobal
#'
#' @export
bracket <- \(x) smush(r"--{[}--", x, r"--{]}--")

#' Wrap A String in Parentheses
#'
#' @param x `<character>` string
#'
#' @returns `<character>` string
#'
#' @examples
#' parentheses("XYZ")
#'
#' @autoglobal
#'
#' @export
parentheses <- \(x) smush(r"--{(}--", x, r"--{)}--")

#' Sort and Order Vector
#'
#' @param x `<character>` vector
#'
#' @returns `<character>` vector
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

  smush(smush(alph), smush(numb))
}

#' Convert Letters to Integers
#'
#' @param x `<character>` vector of letters
#'
#' @examples
#' letters_to_numbers(LETTERS)
#'
#' @returns `<integer>` vector
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
