#' Expand tibble across columns with default values
#'
#' The data.frame `d` is expanded in such a manner that all rows with `NA` in
#' the columns named in the `...` arguments are extended to repeat for all
#' elements listed in the `...` arguments.  Columns with specified data will
#' overwrite extended ones.  Columns are extended in the order of the `...`
#' arguments.
#'
#' @param d A [`data.frame`].
#'
#' @param ... Named arguments with character vectors to expand columns with.
#'     Argument names must match column names.
#'
#' @return A [`tibble`][tibble::tibble].
#'
#' @examples
#' \dontrun{
#' d <- tribble(
#'     ~scenario,   ~region,   ~value,
#'     NA,          NA,        0,
#'     NA,          'AAA',     1,
#'     'foo',       NA,        2,
#'     'foo',       'BBB',     3) %>%
#'     print()
#'
#' scenarios <- c('foo', 'bar', 'buzz')
#' regions   <- c('AAA', 'BBB', 'ZZZ')
#'
#' # expand scenarios, then regions
#' tool_expand_tibble(d, scenario = scenarios, region = regions) %>%
#'     arrange(value, scenario, region)
#'
#' # expand regions, then scenarios
#' tool_expand_tibble(d, region = regions, scenario = scenarios) %>%
#'     arrange(value, scenario, region)
#' }
#'
#' @importFrom dplyr anti_join arrange bind_rows distinct filter select
#' @importFrom magrittr %>%
#' @importFrom rlang !! !!! sym syms :=
#' @importFrom tibble tribble
#' @importFrom tidyr expand_grid
#'
#' @export
tool_expand_tibble <- function(d, ...)
{
    .dots <- list(...)

    if (length(names(.dots)) != length(.dots)) {
        stop('All `...` arguments must be named.')
    }

    for (i in which(sapply(.dots, Negate(is.null)))) {
        d <- bind_rows(
            d %>%
                filter(!is.na(!!sym(names(.dots)[[i]]))),

            d %>%
                filter(is.na(!!sym(names(.dots)[[i]]))) %>%
                select(-names(.dots)[[i]]) %>%
                distinct() %>%
                expand_grid(!!sym(names(.dots)[[i]]) := .dots[[i]]) %>%
                select(all_of(colnames(d))) %>%
                anti_join(
                    d %>%
                        filter(!is.na(!!sym(names(.dots)[[i]]))),

                    names(.dots)
                )
        ) %>%
            arrange(!!!syms(names(d)))
    }

    return(d)
}
