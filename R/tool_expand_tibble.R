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
#'     Argument names must match column names.  Use `NULL` arguments for columns
#'     that will be preserved with their current set of values.
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
#' @importFrom cli pluralize
#' @importFrom dplyr anti_join arrange bind_rows distinct filter relocate select
#' @importFrom magrittr %>%
#' @importFrom rlang := !! !!! sym syms
#' @importFrom tibble tribble
#' @importFrom tidyr expand_grid
#'
#' @export
tool_expand_tibble <- function(d, ...)
{
    dots <- list(...)

    # arguments can also be passed as a single list
    if (is.list(dots[[1]]) && 1 == length(dots))
        dots <- dots[[1]]

    stopifnot('All `...` arguments must be named.' = all('' != names(dots)))

    if (length(unknowns <- setdiff(names(dots), colnames(d)))) {
        unknowns <- paste0('`', unknowns, '`')
        stop(pluralize('All `...` arguments must be columns of `d`. ',
                       '{unknowns} {?is/are} not.'))
    }

    for (i in seq_along(dots)) {
        column <- names(dots)[[i]]
        value  <- sort(dots[[i]])

        if (is.null(value))
            next

        d <- bind_rows(
            d %>%
                filter(!is.na(!!sym(column))),

            d %>%
                filter(is.na(!!sym(column))) %>%
                select(-all_of(column)) %>%
                distinct() %>%
                expand_grid(!!sym(column) := value) %>%
                relocate(all_of(colnames(d))) %>%
                anti_join(
                    d %>%
                        filter(!is.na(!!sym(column))),

                    intersect(names(dots), colnames(d))
                )
        ) %>%
            filter(!!sym(column) %in% value) %>%
            arrange(!!!syms(names(d)))
    }

    return(d)
}
