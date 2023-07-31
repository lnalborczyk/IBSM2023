#' Plotting posterior samples in the style of the `BEST` package
#'
#' Plotting posterior samples in the style of the `BEST` package.
#'
#' @param samples Numeric, samples from some distribution.
#' @param credmass Numeric, credibility mass (default to 0.91).
#' @param usemode Logical, indicating whether we should use the using the mean
#'   (default) or the mode?
#' @param compval Numeric, to what value comparing the posterior?
#' @param rope Numeric, defining the region of practical equivalence
#'   (such as c(-0.1, 0.1) ).
#' @param showcurve Logical, indicates whether we should plot a density (TRUE)
#'   instead of the histogram (FALSE).
#' @param maincolour Character string indicating the colour of the
#'   histogram (or density).
#' @param compvalcolour Character string indicating the colour the comparison
#'   value's text.
#' @param ROPEcolour Character string indicating the colour of the ROPE's text
#' @param textsize Numeric, defining the size of the text elements.
#' @param nbins Numeric, defining the number of bins.
#'
#' @return A ggplot histogram or density of posterior samples.
#'
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \dontrun{
#' # getting samples for a normal distribution
#' samples <- rnorm(n = 1e3, mean = 0, sd = 1)
#'
#' # plotting it
#' posterior_plot(
#'   samples = samples, credmass = 0.96, usemode = FALSE,
#'   compval = 1, rope = c(0.8, 1.2), showcurve = FALSE
#'   ) +
#'   # the resulting plot is a ggplot than can be customised
#'   labs(x = expression(theta) )
#' }

posterior_plot <- function (
        samples, credmass = 0.91, usemode = FALSE,
        compval = NULL, rope = NULL, showcurve = FALSE,
        maincolour = "steelblue", compvalcolour = "darkgreen",
        ROPEcolour = "darkred", textsize = 5, nbins = NULL
        ) {

    # some tests for variable types
    stopifnot("samples must be a numeric..." = is.numeric(samples) )
    stopifnot("credmass must be a numeric..." = is.numeric(credmass) )
    stopifnot("usemode must be a logical..." = is.logical(usemode) )
    stopifnot("compval must be a numeric..." = is.null(compval) | is.numeric(compval) )
    stopifnot("rope must be a numeric..." = is.null(rope) | is.numeric(rope) )
    stopifnot("showcurve must be a logical..." = is.logical(showcurve) )
    stopifnot("maincolour must be a character..." = is.character(maincolour) )
    stopifnot("compvalcolour must be a character..." = is.character(compvalcolour) )
    stopifnot("ROPEcolour must be a character..." = is.character(ROPEcolour) )
    stopifnot("textsize must be a numeric..." = is.numeric(textsize) )
    stopifnot("nbins must be a numeric..." = is.null(nbins) | is.numeric(nbins) )

    # testing whether compval is a unique value
    if (!is.null(compval) ) {

        stopifnot("compval must be a numeric of length 1..." = length(compval) == 1)

    }

    # testing whether the comparison value is inside the ROPE
    if (!is.null(rope) && !is.null(compval) ) {

        if (compval < min(rope) | compval > max(rope) ) {

            stop ("The comparison value should be within the ROPE.")

        }

    }

    # issuing a warning when the number of samples is low
    if (length(samples) < 1e2) {

        warning ("
        The number of posterior samples is low. Beware that the estimation
        of the HDI bounds or the % of samples in ROPE may be unreliable.
                 ")

    }

    # warning if the number of bins does not sound sensible
    if (!is.null(nbins) ) {


        if (nbins < 5 | nbins > 100) {

            warning ("
            The number of bins does not sound sensible, you should probably
            define a better number of bins. A good rule of thumbs is to use
            Freedman-Diaconis' rule (the default).
                     ")

            }

    }

    # if null, define nbins using Freedman-Diaconis' rule
    if (is.null(nbins) ) nbins <- imsb::fd_nbins(samples)

    # computing the credible interval (HDI)
    hdis <- bayestestR::hdi(x = samples, ci = credmass) |> data.frame()
    hdi_text <- hdis |> tidyr::pivot_longer(cols = 2:3)

    # computing the density to scale the position of elements
    densCurve <- stats::density(x = samples, adjust = 2, n = 2048)

    # computing the posterior central tendency (mean or mode)
    if (usemode) central_tendency <- imsb::find_mode(samples)
    if (!usemode) central_tendency <- mean(samples)

    # if a comparison value is specified
    if (!is.null(compval) ) {

        # computing the percentage of samples above the comparison value
        lower_than_compval <- round(x = mean(samples < compval) * 100, digits = 2)
        higher_than_compval <- round(x = mean(samples > compval) * 100, digits = 2)

        # preparing the compval text
        compval_text <- paste0(
            lower_than_compval, "% < ", compval, " < ",
            higher_than_compval, "%"
            )

    }

    # if a comparison value is specified
    if (!is.null(rope) ) {

        # computing the percentage of samples in ROPE
        pc_rope <- round(
            x = mean(samples > min(rope) & samples < max(rope) ) * 100,
            digits = 2
            )

    }

    # plotting it
    samples |>
        data.frame() |>
        ggplot2::ggplot(ggplot2::aes(x = .data$samples, y = .data$..density..) ) +
        {if (!showcurve) ggplot2::geom_histogram(
            bins = nbins,
            alpha = 0.4,
            colour = "white", fill = maincolour
            )} +
        {if (showcurve) ggplot2::geom_density(
            alpha = 0.4, size = 2, colour = maincolour
            )} +
        ggplot2::geom_errorbarh(
            data = hdis,
            ggplot2::aes(xmin = .data$CI_low, xmax = .data$CI_high, y = 0),
            height = 0, size = 2,
            inherit.aes = FALSE, show.legend = FALSE
            ) +
        ggplot2::geom_text(
            data = hdi_text,
            ggplot2::aes(
                x = .data$value, y = 0,
                label = round(x = .data$value, digits = 2)
                ),
            nudge_y = 0.05 * max(densCurve$y),
            size = textsize,
            inherit.aes = FALSE, show.legend = FALSE
            ) +
        ggplot2::geom_text(
            ggplot2::aes(
                x = mean(c(hdis$CI_low, hdis$CI_high) ), y = 0,
                label = paste0(100 * credmass, "% HDI")
                ),
            nudge_y = 0.1 * max(densCurve$y),
            size = textsize,
            inherit.aes = FALSE, show.legend = FALSE
            ) +
        ggplot2::geom_text(
            ggplot2::aes(
                x = central_tendency, y = 0.9 * max(densCurve$y),
                label = ifelse(
                    test = usemode,
                    yes = paste("mode =", round(x = central_tendency, digits = 2) ),
                    no = paste("mean =", round(x = central_tendency, digits = 2) )
                    )
                ),
            size = textsize,
            inherit.aes = FALSE, show.legend = FALSE
            ) +
        {if (!is.null(compval) ) ggplot2::geom_segment(
            ggplot2::aes(
                x = compval, xend = compval,
                y = 0, yend = 0.7 * max(densCurve$y)
                ),
            linetype = 2,
            colour = compvalcolour
            )} +
        {if (!is.null(compval) ) ggplot2::geom_text(
            ggplot2::aes(
                x = compval, y = 0.7 * max(densCurve$y),
                label = compval_text
                ),
            colour = compvalcolour,
            size = textsize,
            nudge_y = 0.05 * max(densCurve$y),
            inherit.aes = FALSE, show.legend = FALSE
            )} +
        {if (!is.null(rope) ) ggplot2::geom_segment(
            ggplot2::aes(
                x = min(rope), xend = min(rope),
                y = 0, yend = 0.55 * max(densCurve$y)
                ),
            linetype = 3,
            colour = ROPEcolour
            )} +
        {if (!is.null(rope) ) ggplot2::geom_segment(
            ggplot2::aes(
                x = max(rope), xend = max(rope),
                y = 0, yend = 0.55 * max(densCurve$y)
                ),
            linetype = 3,
            colour = ROPEcolour
            )} +
        {if (!is.null(rope) ) ggplot2::geom_text(
            ggplot2::aes(
                x = mean(range(rope) ), y = 0.55 * max(densCurve$y),
                label = paste0(pc_rope, "% in ROPE")
                ),
            colour = ROPEcolour,
            size = textsize,
            nudge_y = 0.05 * max(densCurve$y),
            inherit.aes = FALSE, show.legend = FALSE
            )} +
        ggplot2::labs(y = "") +
        ggplot2::theme_classic(base_size = 12) +
        ggplot2::theme(
            axis.text.y = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank(),
            axis.line.y = ggplot2::element_blank()
            )

}
