# ---------------------------------------------------------------------------- #
# Functions for exploring/presenting Zero-inflated Beta Regression Output
# See Liu and Kong (under review) for backgroud
# Christopher Gandrud
# All functions available under the MIT License
# ---------------------------------------------------------------------------- #

#' Function to plot parameter posteriors from Zero-inflated Beta Regressions
#'
#' @param obj model object from \code{zoib} where \code{one.inflation = FALSE}
#' and \code{joint = FALSE}.
#' @param max maximum number of simulations post burn-in
#' @param variable_names character vector of variable names. Must be in the
#' same order and the same length as variables enterd into the \code{zoib}
#' formula.
#' @param title character string title for the plot
#'

zibPlot <- function(obj, max, variable_names = NULL, xlab = '\nCoef. Estimates',
                    title = ''){
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

    # Indicator of whether beta or logit part
    Summed$part <- NA
    Summed$part <- ifelse(grepl('b\\[', as.character(Summed$Var2)),
                        'logit(mean of beta)', Summed$part)
    Summed$part <- ifelse(grepl('b0\\[', as.character(Summed$Var2)),
                        'logit(Pr(y = 0))', Summed$part)
    Summed$part <- ifelse(grepl('d', as.character(Summed$Var2)),
                          'shared', Summed$part)
    Summed$part <- ifelse(grepl('sigma', as.character(Summed$Var2)),
                          'shared', Summed$part)
    Summed$part <- factor(Summed$part)

    # Label
    if (!is.null(variable_names)){
        n_model_vars <- (nrow(Summed) - 4)/2
        if (length(variable_names) != n_model_vars){
            stop('variable_names is a different length than expected.\n',
                 .call = FALSE)
        }
        Summed$Labels <- factor(ReLabel(Summed, variable_names = variable_names))

        Summed$Var2 <- 1:nrow(Summed)
        Summed$Var2 <- factor(Summed$Var2, labels = Summed$Labels)
        clevels <- levels(Summed$Var2)
    }
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
        scale_color_manual(values = c('#a6bddb', '#2b8cbe', 'gray'),
                            guide = FALSE) +
        xlab(xlab) + ylab('') + ggtitle(title) +
        theme_bw(base_size = 15)

    return(pp)
}

#' Internal function for relabelling the parameters
#'
#' @param obj Object passed from \code{zibPlot}.
#' @param variable_names character vector of variable names. Must be in the
#' same order and the same length as variables enterd into the \code{zoib}
#' formula.
#'
#' @keywords internal
#'

ReLabel <-function(obj, variable_names){
    obj$nn <- NA
    obj$nn <- ifelse(grepl('b\\[1\\]', obj$Var2), 'Intercept [Beta]', obj$nn)
    obj$nn <- ifelse(grepl('b0\\[1\\]', obj$Var2), 'Intercept [Pr(y = 0)]',
                    obj$nn)
    obj$nn <- ifelse(grepl('d', obj$Var2), 'log(dispersion of beta)', obj$nn)
    obj$nn <- ifelse(grepl('sigma', obj$Var2), 'sigma', obj$nn)
    for (i in 1:length(variable_names)){
        i2 <- i + 1
        id <- paste0('\\[', i2, '\\]')
        obj$nn <- ifelse(grepl(id, obj$Var2) & grepl('b\\[', obj$Var2),
                        paste0(variable_names[i], ' [Beta]'), obj$nn)
        obj$nn <- ifelse(grepl(id, obj$Var2) & grepl('b0\\[', obj$Var2),
                        paste0(variable_names[i], ' [Pr(y = 0)]'), obj$nn)
    }
    return(obj$nn)
}

#' Extract simulations in suitable form for Gelman-Rubin test and summary
#' @param obj model object from \code{zoib} where \code{one.inflation = FALSE}
#' and \code{joint = FALSE}.
#' @param max maximum number of simulations post burn-in
#'

GetzibPost <- function(obj, max){
    require(coda)
    post.sample <- obj$oripara
    sample.c1<- post.sample[[1]][1:max,]
    sample.c2<- post.sample[[2]][1:max,]
    sample12 <- mcmc.list(as.mcmc(sample.c1),as.mcmc(sample.c2))
    return(sample12)
}

#' Extract Gelman-Rubin diagnostics as data frame

GelmanDiag <- function(obj, iter){
    require(zoib)
    obj <- GetzibPost(obj, max = iter/2)
    gr <-  gelman.diag(obj)
    grDF <- gr[[1]]
    return(grDF)
}


#' Extract and summarize posterior distribution

SummaryZib <- function(obj, iter){
    require(zoib)
    obj <- GetzibPost(obj, max = iter/2)
    Sum <-  summary(obj)
    return(Sum)
}
