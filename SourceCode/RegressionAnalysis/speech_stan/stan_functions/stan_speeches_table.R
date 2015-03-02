#' Create data frame of results from stanfit speeches for tables
#'
#' @importFrom rstan extract
#' @importFrom dplyr %>%

stan_speeches_param_est <- function(stanfit, model_pars = c('beta', 'alpha'))
{
    # Extract posterior
    sims <- rstan::extract(stanfit, pars = model_pars) %>% as.data.frame
    
    medians <- sapply(1:ncol(sims), function(x) median(sims[, x]) %>% 
                          round(digits = 2))
    lower_95 <- sapply(1:ncol(sims), function(x) 
                    quantile(sims[, x], probs = 0.025) %>% 
                    round(digits = 2))
    upper_95 <- sapply(1:ncol(sims), function(x) 
                    quantile(sims[, x], probs = 0.975) %>% 
                    round(digits = 2))
    
    cred_interval <- sprintf('(%s, %s)', lower_95, upper_95)
    
    comb <- rbind(medians, cred_interval) %>% c()
    
    comb <- waic(fit_housing)$waic[1] %>% round(digits = 2) %>% as.vector %>%
            c(comb, '', .)
    
    return(comb)    
}


test <- stan_speeches_param_est(fit_housing)

sims <- test

sprintf('(%s, %s)', lower_95, upper_95)
