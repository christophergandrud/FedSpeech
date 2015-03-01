#' Wrapper function for running Stan model in parallel on 4 cores
#'

parallel_4 <- function(fit, data, iter = 2000, pars = c('a', 'b')){
    sflist <-
        mclapply(1:4, mc.cores = 4,
                 function(i) stan(fit = fit, data = data,
                                  seed = i, chains = 1,
                                  iter = iter, chain_id = i,
                                  pars = pars
                 )
        )

    # Collect in to Stan fit object
    fit <- sflist2stanfit(sflist)
    return(fit)
}
