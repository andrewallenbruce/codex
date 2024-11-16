#' Split Character Vector by Lengths
#'
#' @param x `<character>` vector
#'
#' @returns `<list>` of character vectors
#'
#' @examples
#' random_hcpcs(5) |>
#'    split_lengths()
#'
#' @autoglobal
#'
#' @export
split_lengths <- function(x) {

  stopifnot(is.character(x))

  x <- sf_remove(x, "\\*|\\s") |>
    uniq_narm() |>
    strsort()

  l <- vlen(x)

  list(
    x1 = x[l == 1],
    x2 = x[l == 2],
    x3 = x[l == 3],
    x4 = x[l == 4],
    x5 = x[l == 5])
}

#' Split Character Vector by Lengths
#'
#' @param x `<character>` vector
#'
#' @returns `<list>` of character vectors
#'
#' @examples
#' random_hcpcs(5) |>
#'    split_lengths() |>
#'    remove_redundant() |>
#'    split_first()
#'
#' @importFrom collapse %=% .c
#'
#' @autoglobal
#'
#' @export
split_first <- function(x) {

  .c(x1, x2, x3, x4, x5) %=% x

  list(
    x1 = list(x1),
    x2 = split_1(x2),
    x3 = split_1(x3),
    x4 = split_1(x4),
    x5 = split_end(x5))
}

#' Split Vector by Index Subset
#'
#' @param x `<character>` vector
#'
#' @param y `<character>` vector; default is `x`
#'
#' @param start `<integer>` index start
#'
#' @param stop `<integer>` index end
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
#' split_at(x = end, y = beg, start = 1, stop = 2)
#'
#' @autoglobal
#'
#' @export
split_at <- \(x, y = x, start, stop) collapse::rsplit(x, sf_sub(x = y, start = start, stop = stop), use.names = FALSE)

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
split_1 <- \(x) split_at(x, y = x, start = 1, stop = 1)

#' Split Character Vector at End
#'
#' @param x `<character>` vector
#'
#' @returns `<list>` of character vectors
#'
#' @examples
#' split_end(c("0741T", "E0628", "L4387", "0360T", "1127F", "0002M"))
#'
#' @autoglobal
#'
#' @export
split_end <- \(x) {

  if (any(sf_detect(x, "[A-Z]$"))) {

    end <- sf_extract(x, "[A-Z]$")
    beg <- paste0(take_at(end, 5), sf_remove(end, "[A-Z]$"))

    c(split_at(x = x[!sf_detect(x, "[A-Z]$")], start = 1, stop = 1),
      split_at(x = end, y = beg, start = 1, stop = 2))

  } else {

    split_at(x, start = 1, stop = 1)

  }
}

#' Delist, Unname and Split a String
#'
#' @param x `<character>` string or named `<list>`
#'
#' @returns Unnamed `<list>` of split `<character>` vectors
#'
#' @examples
#' # unnamed vector
#' desplit("XYZ")
#'
#' # named vector
#' desplit(c(x = "XYZ"))
#'
#' # unnamed list with one element
#' desplit(list("XYZ"))
#'
#' # unnamed list with multiple elements
#' desplit(list("YYY", "ZZZ"))
#'
#' # named list with one element
#' desplit(list(x = "XYZ"))
#'
#' # named list with multiple elements
#' desplit(list(x = "YYY", xx = "ZZZ"))
#'
#' @autoglobal
#'
#' @export
desplit <- \(x) {

  res <- strsplit(delist(x), "")

  if (length(res) == 1) return(res[[1]])

  res
}

#' Split String into Pieces
#'
#' Splits each string in a character vector into a fixed number of pieces,
#' returning a character matrix.
#'
#' @param x `<character>` vector
#'
#' @returns `<list>` of character vectors
#'
#' @examples
#' fruits <- c("apples and oranges and pears and bananas", "pineapples and mangos and guavas")
#'
#' split_max_vlen(fruits)
#'
#' @autoglobal
#'
#' @export
split_max_vlen <- \(x) stringr::str_split_fixed(string = x, pattern = "", n = max_vlen(x))
