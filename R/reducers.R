#' Reduce Runs
#'
#' @param x `<list>` of character vectors
#'
#' @returns `<list>` of character vectors
#'
#' @examples
#' x <- random_hcpcs(20) |>
#'    split_lengths() |>
#'    split_first() |>
#'    process_groups()
#'
#' delist(x$g2) |>
#'    split_max_vlen() |>
#'    as.data.frame() |>
#'    purrr::map(uniq_narm) |>
#'    purrr::map(sort_order) |>
#'    purrr::map(reduce_runs)
#'
#' @importFrom collapse %!in% fgroup_by fungroup fcount fsubset join groupid fselect frename
#' @importFrom purrr map list_c
#' @importFrom data.table data.table
#' @importFrom stringr str_glue_data
#' @importFrom stats setNames
#' @importFrom dplyr slice_min slice_max
#'
#' @autoglobal
#'
#' @export
reduce_runs <- function(x) {
  if (sf_nchar(x) == 1)
    return(x)

  test <- setNames(rep(0, 37), c(0:9, "&", LETTERS))

  vec <- test[c(desplit(x), "&")]

  vec <- vec[not_na(vec)]

  test[names(vec)] <- 1

  test[names(test) == "&"] <- 0

  groups <- data.table(value = names(test),
                       keys = test,
                       group = groupid(test)) |>
    fgroup_by(group)

  groups <- join(groups,
                 fcount(groups, group),
                 on = "group",
                 verbose = 0) |>
    fungroup() |>
    fsubset(keys == 1) |>
    fsubset(N >= 3) |>
    fselect(value, group)

  if (empty(groups))
    return(x)

  xgroups <- gchop(groups$value, groups$group) |>
    map(smush) |>
    list_c()

  if (all(xgroups == smush(c(0:9, "&", LETTERS))))
    return("[A-Z0-9]")

  replacements <- join(
    slice_min(groups, by = group, order_by = value) |> frename(start = value),
    slice_max(groups, by = group, order_by = value) |> frename(end = value),
    on = "group",
    verbose = 0
  ) |>
    str_glue_data("{start}-{end}") |>
    as.vector()

  bracket(replace_regex(x, xgroups, replacements))
}

#' Reduce Length 1
#'
#' @param x `<list>` of character vectors
#'
#' @returns `<list>` of character vectors
#'
#' @examples
#' random_hcpcs(20) |>
#'    split_lengths() |>
#'    split_first() |>
#'    process_groups() |>
#'    red1()
#'
#' @importFrom stringr str_glue
#'
#' @autoglobal
#'
#' @export
red1 <- function(x) {
  x <- getelem(x, "g1")

  if (empty(x))
    return(character(0))

  re <- sort_order(x) |>
    reduce_runs()

  if (re == "[A-Z0-9]")
    return(str_glue("^{re}{{5}}"))

  post <- iif_else(sf_detect(re, "\\[?[0-9]{1}"), "[A-Z0-9]", "[0-9]")

  str_glue("^{re}{post}{{4}}")
}

#' Reduce Length 2
#'
#' @param x `<list>` of character vectors
#'
#' @returns `<list>` of character vectors
#'
#' @examples
#' random_hcpcs(20) |>
#'    split_lengths() |>
#'    split_first() |>
#'    process_groups() |>
#'    red2()
#'
#' @importFrom purrr map map_chr modify_if
#'
#' @autoglobal
#'
#' @export
red2 <- function(x) {
  x <- getelem(x, "g2")

  if (empty(x))
    return(character(0))

  modify_if(x, len_gt_one, function(x) {
    parts <- split_max_vlen(x) |>
      as.data.frame() |>
      map(uniq_narm) |>
      map(sort_order) |>
      map(reduce_runs) |>
      delist()

    multi <- sf_nchar(parts) > 1
    nobrk <- sf_ndetect(parts, "\\[|\\]")

    if (any(multi[nobrk]))
      parts[multi[nobrk]] <- map_chr(parts[multi[nobrk]], bracket)

    smush(parts)

  })
}

#' Reduce Length 3
#'
#' @param x `<list>` of character vectors
#'
#' @returns `<list>` of character vectors
#'
#' @examples
#' random_hcpcs(20) |>
#'    split_lengths() |>
#'    split_first() |>
#'    process_groups() |>
#'    red3()
#'
#' @importFrom purrr map map_chr modify_if
#'
#' @autoglobal
#'
#' @export
red3 <- function(x) {
  x <- getelem(x, "g3")

  if (empty(x))
    return(character(0))

  map(x, function(x) {
    modify_if(x, len_gt_one, function(x) {
      parts <- split_max_vlen(x) |>
        as.data.frame() |>
        map(uniq_narm) |>
        map(sort_order) |>
        map(reduce_runs) |>
        delist()

      multi <- sf_nchar(parts) > 1
      nobrk <- sf_ndetect(parts, "\\[|\\]")

      if (any(multi[nobrk]))
        parts[multi[nobrk]] <- map_chr(parts[multi[nobrk]], bracket)

      smush(parts)
    })
  })
}

#' Reduce Length 4
#'
#' @param x `<list>` of character vectors
#'
#' @returns `<list>` of character vectors
#'
#' @examples
#' random_hcpcs(20) |>
#'    split_lengths() |>
#'    split_first() |>
#'    process_groups() |>
#'    red4()
#'
#' @importFrom purrr map map_chr modify_if
#'
#' @autoglobal
#'
#' @export
red4 <- function(x) {
  x <- getelem(x, "g4")

  if (empty(x))
    return(character(0))

  map(x, function(x) {
    modify_if(x, len_gt_one, function(x) {
      parts <- split_max_vlen(x) |>
        as.data.frame() |>
        map(uniq_narm) |>
        map(sort_order) |>
        map(reduce_runs) |>
        delist()

      multi <- sf_nchar(parts) > 1
      nobrk <- sf_ndetect(parts, "\\[|\\]")

      if (any(multi[nobrk]))
        parts[multi[nobrk]] <- map_chr(parts[multi[nobrk]], bracket)

      smush(parts)

    })
  })
}

#' Reduce Length 5
#'
#' @param x `<list>` of character vectors
#'
#' @returns `<list>` of character vectors
#'
#' @examples
#' random_hcpcs(20) |>
#'    split_lengths() |>
#'    split_first() |>
#'    process_groups() |>
#'    red5()
#'
#' @importFrom purrr map map_chr modify_if
#'
#' @autoglobal
#'
#' @export
red5 <- function(x) {
  x <- getelem(x, "g5")

  if (empty(x))
    return(character(0))

  map(x, function(x) {
    modify_if(x, len_gt_one, function(x) {
      parts <- split_max_vlen(x) |>
        as.data.frame() |>
        map(uniq_narm) |>
        map(sort_order) |>
        map(reduce_runs) |>
        delist()

      multi <- sf_nchar(parts) > 1
      nobrk <- sf_ndetect(parts, "\\[|\\]")

      if (any(multi[nobrk]))
        parts[multi[nobrk]] <- map_chr(parts[multi[nobrk]], bracket)

      smush(parts)
    })
  })
}

#' Reduce Groups
#'
#' @param x `<list>` of character vectors
#'
#' @returns `<list>` of character vectors
#'
#' @examples
#' random_hcpcs(20) |>
#'    split_lengths() |>
#'    split_first() |>
#'    process_groups() |>
#'    reduce_groups()
#'
#' @autoglobal
#'
#' @export
reduce_groups <- function(x) {
  list(
    g1 = red1(x),
    g2 = red2(x),
    g3 = red3(x),
    g4 = red4(x),
    g5 = red5(x)
  )
}
