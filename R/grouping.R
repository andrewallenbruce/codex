#' Group 3
#'
#' @param x `<list>` of character vectors
#'
#' @returns `<list>` of character vectors
#'
#' @examples
#' c("C75", "C97", "G02", "G04") |>
#'    split_lengths() |>
#'    remove_redundant() |>
#'    split_first() |>
#'    group_3()
#'
#' @importFrom collapse %!in% get_elem fgroup_by fmutate fungroup fcount fsubset join groupid
#' @importFrom cheapr is_na
#' @importFrom purrr map
#' @importFrom data.table data.table
#'
#' @export
#'
#' @autoglobal
group_3 <- function(x) {

  x <- get_elem(x, "x3")

  if (empty(x)) return(x)

  map(x, function(x) {

    idx <- data.table(
      code = x,
      grp = sf_sub(x, 1, 2),
      a1 = take_at(x),
      a2 = take_at(x, 2),
      a3 = take_at(x, 3),
      i1 = groupid(take_at(x))) |>
      fgroup_by(a1) |>
      fmutate(i2 = groupid(a2)) |>
      fgroup_by(a1, a2) |>
      fmutate(i3 = groupid(a3)) |>
      fungroup()

    lone <- fcount(idx, i1, add = TRUE) |>
      fsubset((i2 + i3 + N) == 3)

    last <- fsubset(idx, code %!in% lone[["code"]])

    last <- join(
      fcount(last, a1, a2),
      fcount(last, a1, name = "G"),
      on = "a1", verbose = 0) |>
      fsubset(N == G) |>
      join(last, on = c("a1", "a2"),
           how = "right",
           verbose = 0) |>
      fsubset(!is_na(N))

    rest <- fsubset(idx, code %!in% c(lone[["code"]], last[["code"]]))

    c(if (empty(lone)) NULL else list(lone[["code"]]),
      if (empty(last)) NULL else gchop(last[["code"]], last[["a1"]]),
      if (empty(rest)) NULL else gchop(rest[["code"]], rest[["grp"]]))
  })
}
