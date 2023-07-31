#' Open slides in browser
#'
#' Opening the slides of the corresponding course (from 01 to 10) in browser.
#'
#' @param cours Course number, from 01 to 10.
#'
#' @return Returns nothing but opens the slides in browser.
#' @export
#'
#' @examples
#' \dontrun{
#' open_slides(cours = 01)
#' }

open_slides <- function (cours) {

    num <- stringr::str_pad(string = cours, width = 2, pad = "0")

    if (as.numeric(num) > 0 & num < 11) {

        utils::browseURL(
            url = paste0("https://www.barelysignificant.com/IMSB2022/slides/html/cours", num)
            )

    } else {

        stop ("I am sorry, I am not aware of this course... I only know Courses 01 to 10.")

    }
}

#' Open data
#'
#' Opening data from `R` packages.
#'
#' @param df The desired dataset (from a R package).
#'
#' @return Returns the desired dataset in a dataframe.
#' @export
#'
#' @examples
#' \dontrun{
#' open_data(absence)
#' }

open_data <- function (df) {

    return (suppressWarnings(get(utils::data(df) ) ) )

}

#' Find the mode of a distribution from its samples
#'
#' Find the mode of a distribution from its samples.
#'
#' @param samples Numeric, samples from some distribution.
#' @param ... Extra parameters to be passed to the `stats::density()` function.
#'
#' @return Returns the mode of the distribution.
#' @export
#'
#' @examples
#' \dontrun{
#' samples <- rnorm(n = 1e3, mean = 0, sd = 1)
#' find_mode(samples = samples)
#' }

find_mode <- function (samples, ...) {

    dd <- stats::density(samples, ...)

    return (dd$x[which.max(dd$y)])

}

#' An implementation of Freedman-Diaconis' rule
#'
#' An implementation of Freedman-Diaconis' rule to find the "best" number
#' of bins for a histogram.
#'
#' @param samples Numeric, samples from some distribution.
#'
#' @return Returns the number of bins according to Freedman-Diaconis' rule.
#' @export
#'
#' @examples
#' \dontrun{
#' samples <- rnorm(n = 1e3, mean = 0, sd = 1)
#' fd_nbins(samples = samples)
#' }

fd_nbins <- function (samples) {

    nbins <- diff(range(samples) ) /
        (2 * stats::IQR(samples) / length(samples)^(1 / 3) )

    return (round(nbins) )

}
