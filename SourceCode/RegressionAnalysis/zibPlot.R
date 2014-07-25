#' Function to plot parameter posteriors from Zero-inflated Beta Regressions
#'
#' @param obj model object from \code{zoib} where \code{one.inflation = FALSE}
#' and \code{joint = FALSE}.
#' @param max maximum number of simulations post burn-in
#' @param params_labels character vector of parameter lables. Must be in the
#' same order and the same length as the number of parameters watched in the
#' model, including intercepts, the dispersion, and sigma.
#'

zibPlot <- function(obj, max, params_labels = NULL){
    require(reshape2)
    require(dplyr)
    require(ggplot2)

    # Extract simulations
    sims_subset <- obj$oripara
    sims_subset <- sims_subset[[2]][1:max,]

    # Melt
    sims_subset_molten <- melt(sims_subset)

    # Find median and CI
    sims_subset_molten <- group_by(sims_subset_molten, Var2)
    Summed <- mutate(sims_subset_molten, medians = median(value))
    Summed <- mutate(Summed, lower95 = quantile(value, 0.025))
    Summed <- mutate(Summed, lower90 = quantile(value, 0.05))
    Summed <- mutate(Summed, upper90 = quantile(value, 0.95))
    Summed <- mutate(Summed, upper95 = quantile(value, 0.975))

    Summed <- Summed[!duplicated(Summed[, 'Var2']), ]

    # Label
    if (!is.null(params_labels)){
        if (length(params_labels) != nrow(Summed)){
                stop('Parameter labels are a different length than expected',
                .call = FALSE)
            }
        Summed$Var2 <- factor(Summed$Var2, labels = params_labels)
    }

    # Indicator of whether beta or logit part
    Summed$part <- NA
    Summed$part <- ifelse(grepl('b\\[', as.character(Summed$Var2)),
                        'logit(mean of beta)', Summed$part)
    Summed$part <- ifelse(grepl('b0\\[', as.character(Summed$Var2)),
                        'logit(Pr = 0)', Summed$part)
    Summed$part <- factor(Summed$part)

    # Reverse order
    clevels <- levels(Summed$Var2) %>% rev

    # Plot
    pp <- ggplot(Summed, aes(x = medians, y = Var2, xmin = lower95,
                            xmax = upper95, color = part, group = part)) +
        geom_point(size = 3) +
        geom_segment(aes(x = lower95, xend = upper95, yend = Var2),
                     size = 0.5) +
        geom_segment(aes(x = lower90, xend = upper90,
                         yend = Var2), size = 1.5) +
        scale_y_discrete(limits = clevels) +
        geom_vline(xintercept = 0, linetype = 'dotted') +
        scale_color_brewer(palette = 'Set1', name = '') +
        xlab('\nCoefficient Estimates') + ylab('') +
        theme_bw()

    return(pp)
}
