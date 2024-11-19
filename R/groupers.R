#' Group Length 3
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
#' @importFrom collapse %!in% fgroup_by fmutate fungroup fcount fsubset join groupid
#' @importFrom purrr map
#' @importFrom data.table data.table
#'
#' @export
#'
#' @autoglobal
group_3 <- function(x) {
  x <- getelem(x, "x3")

  if (empty(x))
    return(x)

  map(x, function(x) {
    idx <- data.table(
      code = x,
      grp = sf_sub(x, 1, 2),
      a1 = take_at(x),
      a2 = take_at(x, 2),
      a3 = take_at(x, 3),
      i1 = groupid(take_at(x))
    ) |>
      fgroup_by(a1) |> fmutate(i2 = groupid(a2)) |>
      fgroup_by(a1, a2) |> fmutate(i3 = groupid(a3)) |>
      fungroup()

    lone <- fcount(idx, i1, add = TRUE) |>
      fsubset((i2 + i3 + N) == 3)

    last <- fsubset(idx, code %!in% lone[["code"]])

    last <- join(
      fcount(last, a1, a2),
      fcount(last, a1, name = "G"),
      on = "a1",
      verbose = 0
    ) |>
      fsubset(N == G) |>
      join(last,
           on = c("a1", "a2"),
           how = "right",
           verbose = 0) |>
      fsubset(not_na(N))

    rest <- fsubset(idx, code %!in% c(lone[["code"]], last[["code"]]))

    c(
      if (empty(lone))
        NULL
      else
        list(lone[["code"]]),
      if (empty(last))
        NULL
      else
        gchop(last[["code"]], last[["a1"]]),
      if (empty(rest))
        NULL
      else
        gchop(rest[["code"]], rest[["grp"]])
    )
  })
}

#' Group Length 4
#'
#' @param x `<list>` of character vectors
#'
#' @returns `<list>` of character vectors
#'
#' @examples
#' c("C751", "C971", "G020", "G040") |>
#'    split_lengths() |>
#'    remove_redundant() |>
#'    split_first() |>
#'    group_4()
#'
#' @importFrom collapse %!in% fgroup_by fmutate fungroup fcount fsubset join groupid
#' @importFrom purrr map
#' @importFrom data.table data.table
#'
#' @export
#'
#' @autoglobal
group_4 <- function(x) {
  x <- getelem(x, "x4")

  if (empty(x))
    return(x)

  map(x, function(x) {
    idx <- data.table(
      code = x,
      grp1 = sf_sub(x, 1, 2),
      grp2 = sf_sub(x, 1, 3),
      a1 = take_at(x),
      a2 = take_at(x, 2),
      a3 = take_at(x, 3),
      a4 = take_at(x, 4),
      i1 = groupid(take_at(x))
    ) |>
      fgroup_by(a1) |>
      fmutate(i2 = groupid(a2)) |>
      fgroup_by(a1, a2) |>
      fmutate(i3 = groupid(a3)) |>
      fgroup_by(a1, a2, a3) |>
      fmutate(i4 = groupid(a4)) |>
      fungroup()

    lone <- fcount(idx, i1, add = TRUE) |>
      fsubset((i2 + i3 + i4 + N) == 4)

    last <- fsubset(idx, code %!in% lone[["code"]])

    last <- join(
      fcount(last, grp1, a3),
      fcount(last, grp1, name = "G"),
      on = "grp1",
      verbose = 0
    ) |>
      fsubset(N == G) |>
      join(
        last,
        on = c("grp1", "a3"),
        how = "right",
        verbose = 0
      ) |>
      fsubset(not_na(N))

    rest <- fsubset(idx, code %!in% c(lone[["code"]], last[["code"]]))

    c(
      if (empty(lone))
        NULL
      else
        list(lone[["code"]]),
      if (empty(last))
        NULL
      else
        gchop(last[["code"]], last[["grp1"]]),
      if (empty(rest))
        NULL
      else
        gchop(rest[["code"]], rest[["grp2"]])
    )
  })
}

#' Group Length 5 Numeric
#'
#' @param x `<list>` of character vectors
#'
#' @returns `<list>` of character vectors
#'
#' @examples
#' c("C7510", "C9710", "G0200", "G0400") |>
#'    split_lengths() |>
#'    remove_redundant() |>
#'    split_first() |>
#'    group_5()
#'
#' @importFrom collapse %!in% fgroup_by fmutate fungroup fcount fsubset join groupid
#' @importFrom purrr map
#' @importFrom data.table data.table
#'
#' @autoglobal
#'
#' @export
group_5_num <- function(x) {
  map(x, function(x) {
    idx <- data.table(
      code = x,
      grp1 = sf_sub(x, 1, 2),
      grp2 = sf_sub(x, 1, 3),
      grp3 = sf_sub(x, 1, 4),
      a1 = take_at(x),
      a2 = take_at(x, 2),
      a3 = take_at(x, 3),
      a4 = take_at(x, 4),
      a5 = take_at(x, 5),
      i1 = groupid(take_at(x))
    ) |>
      fgroup_by(a1) |>
      fmutate(i2 = groupid(a2)) |>
      fgroup_by(a1, a2) |>
      fmutate(i3 = groupid(a3)) |>
      fgroup_by(a1, a2, a3) |>
      fmutate(i4 = groupid(a4)) |>
      fgroup_by(a1, a2, a3, a4) |>
      fmutate(i5 = groupid(a5)) |>
      fungroup()

    lone <- fcount(idx, i1, add = TRUE) |>
      fsubset((i2 + i3 + i4 + i5 + N) == 5)

    last <- fsubset(idx, code %!in% lone[["code"]])

    last <- join(
      fcount(last, grp2, a4),
      fcount(last, grp2, name = "G"),
      on = "grp2",
      verbose = 0
    ) |>
      fsubset(N == G) |>
      join(
        last,
        on = c("grp2", "a4"),
        how = "right",
        verbose = 0
      ) |>
      fsubset(not_na(N))

    rest <- fsubset(idx, code %!in% c(lone[["code"]], last[["code"]]))

    c(
      if (empty(lone))
        NULL
      else
        list(lone[["code"]]),
      if (empty(last))
        NULL
      else
        gchop(last[["code"]], last[["grp2"]]),
      if (empty(rest))
        NULL
      else
        gchop(rest[["code"]], rest[["grp3"]])
    )
  })
}

#' Group Length 5 Character
#'
#' @param x `<list>` of character vectors
#'
#' @returns `<list>` of character vectors
#'
#' @examples
#' c("C7510", "C9710", "G0200", "G0400") |>
#'    split_lengths() |>
#'    remove_redundant() |>
#'    split_first() |>
#'    group_5()
#'
#' @importFrom collapse %!in% fgroup_by fmutate fungroup fcount fsubset join groupid
#' @importFrom purrr map map2
#' @importFrom data.table data.table
#'
#' @autoglobal
#'
#' @export
group_5_chr <- function(x) {
  original <- x

  reversed <- map(original, \(x) paste0(take_at(x, 5), sf_remove(x, "[A-Z]$")))

  map2(reversed, original, function(x, y) {
    idx <- data.table(
      orig = y,
      code = x,
      grp1 = sf_sub(x, 1, 2),
      grp2 = sf_sub(x, 1, 3),
      grp3 = sf_sub(x, 1, 4),
      a1 = take_at(x),
      a2 = take_at(x, 2),
      a3 = take_at(x, 3),
      a4 = take_at(x, 4),
      a5 = take_at(x, 5),
      i1 = groupid(take_at(x))
    ) |>
      fgroup_by(a1) |>
      fmutate(i2 = groupid(a2)) |>
      fgroup_by(a1, a2) |>
      fmutate(i3 = groupid(a3)) |>
      fgroup_by(a1, a2, a3) |>
      fmutate(i4 = groupid(a4)) |>
      fgroup_by(a1, a2, a3, a4) |>
      fmutate(i5 = groupid(a5)) |>
      fungroup()

    lone <- fcount(idx, i1, add = TRUE) |>
      fsubset((i2 + i3 + i4 + i5 + N) == 5)

    last <- fsubset(idx, code %!in% lone[["code"]])

    last <- join(
      fcount(last, grp2, a4),
      fcount(last, grp2, name = "G"),
      on = "grp2",
      verbose = 0
    ) |>
      fsubset(N == G) |>
      join(
        last,
        on = c("grp2", "a4"),
        how = "right",
        verbose = 0
      ) |>
      fsubset(not_na(N))

    rest <- fsubset(idx, code %!in% c(lone[["code"]], last[["code"]]))

    c(
      if (empty(lone))
        NULL
      else
        list(lone[["orig"]]),
      if (empty(last))
        NULL
      else
        gchop(last[["orig"]], last[["grp1"]]),
      if (empty(rest))
        NULL
      else
        gchop(rest[["orig"]], rest[["grp2"]])
    )
  })
}

#' Group Length 5
#'
#' @param x `<list>` of character vectors
#'
#' @returns `<list>` of character vectors
#'
#' @examples
#' c("C7510", "C9710", "G0200", "G0400") |>
#'    split_lengths() |>
#'    remove_redundant() |>
#'    split_first() |>
#'    group_5()
#'
#' @importFrom purrr map compact
#'
#' @autoglobal
#'
#' @export
group_5 <- function(x) {
  x <- getelem(x, "x5")

  if (empty(x))
    return(x)

  chr <- map(x, \(x) sf_extract(x, "[A-Z]$")) |> compact()
  num <- map(x, \(x) sf_extract(x, "[0-9]$")) |> compact()

  c(if (empty(chr))
    NULL
    else
      group_5_chr(chr), if (empty(num))
        NULL
    else
      group_5_num(num))
}

#' Process Groups
#'
#' @param x `<list>` of character vectors
#'
#' @returns `<list>` of character vectors
#'
#' @examples
#' random_hcpcs(10) |>
#'    split_lengths() |>
#'    remove_redundant() |>
#'    split_first() |>
#'    process_groups()
#' @autoglobal
#'
#' @export
process_groups <- function(x) {
  list(
    g1 = x$x1,
    g2 = x$x2,
    g3 = group_3(x),
    g4 = group_4(x),
    g5 = group_5(x)
  )
}
