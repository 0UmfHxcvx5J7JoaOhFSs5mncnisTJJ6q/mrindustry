#' Industry Expert Guesses
#'
#' Return specific data that controls industry demand scenarios.
#'
#' @md
#' @param subtype Expert Guess to return.  One of
#'   - `Chinese_Steel_Production`: "Smooth" production estimates by Robert
#'     Pietzcker (2022).
#'   - `industry_max_secondary_steel_share`: Maximum share of secondary steel
#'     production in total steel production and years between which a linear
#'     convergence from historic to target shares is to be applied.
#'   - `cement_production_convergence_parameters`: Convergence year and level
#'     (relative to global average) to which per-capita cement demand converges.
#'     Expanded to all regions in
#'     `toolGetMapping('regionmapping_21_EU11-columnname.csv', where = 'madrat')`.
#'
#' @returns A [`tibble`][tibble::tibble].
#'
#' @importFrom dplyr pull
#' @importFrom madrat toolGetMapping
#' @importFrom magrittr %>%
#' @importFrom readr read_delim
#' @importFrom tibble as_tibble

#' @export
toolIndustryExpertGuess <- function(subtype)
{
    path <- switch(
        subtype,
        `Chinese_Steel_Production` = toolGetMapping(
            name = 'Chinese_Steel_Production.csv',
            where = 'mrindustry', returnPathOnly = TRUE),

        `cement_production_convergence_parameters` = toolGetMapping(
            name = 'cement_production_convergence_parameters.csv',
            where = 'mrindustry', returnPathOnly = TRUE),

        `industry_max_secondary_steel_share` = toolGetMapping(
            name = 'industry_max_secondary_steel_share.csv',
            where = 'mrindustry', returnPathOnly = TRUE),

        stop('Unknown subtype ', subtype)
    )

    x <- read_delim(file = path, delim = ',', comment = '#', trim_ws = TRUE,
                    show_col_types = FALSE) %>%
        as_tibble()   # remove readr attributes

    regions <- toolGetMapping(
        name = 'regionmapping_21_EU11-columnname.csv', where = 'madrat') %>%
        as_tibble() %>%
        pull('X21EU11') %>%
        unique() %>%
        sort()

    if ('cement_production_convergence_parameters' == subtype) {
        x <- tool_expand_tibble(x, region = regions)
    }

    return(x)
}
