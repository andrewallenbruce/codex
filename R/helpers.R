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

#' Sort, Order and Collapse Vector
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

#' Replace Pattern Occurrences
#'
#' @param x `<character>` vector; strings to search in
#'
#' @param p `<character>` vector; search patterns
#'
#' @param r `<character>` vector with replacements for matched patterns
#'
#' @returns `<character>` vector
#'
#' @examples
#' replace_regex(c('stringi R', 'REXAMINE', '123'), '( R|R.)', ' r ')
#'
#' @autoglobal
#'
#' @export
replace_regex <- \(x, p, r) stringi::stri_replace_all_regex(str = x, pattern = p, replacement = r, vectorize_all = FALSE)

#' Predicate Length Greater Than 1
#'
#' @param x `<character>` string
#'
#' @returns `<logical>`
#'
#' @examples
#' len_gt_one(c("XYZ"))
#'
#' @autoglobal
#'
#' @export
len_gt_one <- \(x) length(x) > 1

#' Predicate Length Equals 1
#'
#' @param x `<character>` string
#'
#' @returns `<logical>`
#'
#' @examples
#' len_eq_one(c("XYZ"))
#'
#' @autoglobal
#'
#' @export
len_eq_one <- \(x) length(x) == 1
