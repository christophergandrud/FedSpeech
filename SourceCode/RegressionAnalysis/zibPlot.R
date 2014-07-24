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
    medians <- summarise(sims_subset_molten, median(value))
    lower95 <- summarise(sims_subset_molten, quantile(value, 0.025))
    lower90 <- summarise(sims_subset_molten, quantile(value, 0.05))
    upper90 <- summarise(sims_subset_molten, quantile(value, 0.95))
    upper95 <- summarise(sims_subset_molten, quantile(value, 0.975))
    comb <- suppressMessages(inner_join(lower95, lower90))
    comb <- suppressMessages(inner_join(comb, medians))
    comb <- suppressMessages(inner_join(comb, upper90))
    comb <- suppressMessages(inner_join(comb, upper95))
    names(comb) <- c('params', 'lower95', 'lower90', 'medians', 'upper90',
                     'upper95')
    
    # Label
    if (!is.null(labels)){
        comb$params <- factor(comb$params, labels = params_labels)
    }
    
    # Reverse order
    clevels <- levels(comb$params) %>% rev

    # Plot
    pp <- ggplot(comb, aes(x = medians, y = params,
                           xmin = lower95,
                           xmax = upper95)) +
        geom_point(size = 3) +
        geom_segment(aes(x = lower95, xend = upper95, yend = params),
                     size = 0.5) +
        geom_segment(aes(x = lower90, xend = upper90,
                         yend = params), size = 1.5) +
        scale_y_discrete(limits = clevels) +
        geom_vline(xintercept = 0, linetype = 'dotted') +
        xlab('\nCoefficient Estimates') + ylab('') +
        theme_bw()
    
    return(pp)
}

pLabels <- c('Beta_intercept', 'Beta_Scrutiny_Med', 'Beta_Scrutiny_High',
             'P=0_intercept', 'P=0_Scrutiny_Med', 'P=0_Scrutiny_High',
             'd', 'sigma')

zibPlot(HD2, max = 250, params_labels = pLabels)


