#' A simple function for running Stan in parallel

parallel_4 <- function(fit, data, iter = 2000,
                        pars = c('alpha', 'beta', 'a', 'log_lik'), cores = 4)
{
    sflist <-
        mclapply(1:cores, mc.cores = cores,
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
