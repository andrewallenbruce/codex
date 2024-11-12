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
#' @importFrom purrr list_c
#' @importFrom stringr str_extract_all str_sort regex
#'
#' @autoglobal
#'
#' @export
sort_order <- \(x) {
  sort <- str_sort(x, numeric = TRUE)

  alph <- list_c(str_extract_all(sort, regex("[A-Z]")))

  numb <- list_c(str_extract_all(sort, regex("[0-9]")))

  smush(smush(alph), smush(numb))
}
