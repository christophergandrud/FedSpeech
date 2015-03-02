#' Create data frame of results from stanfit speeches for tables
#'
#' @importFrom rstan extract
#' @importFrom dplyr %>%

stan_speeches_param_est <- function(stanfit, model_pars = c('beta', 'alpha'),
                                    pars_labels, col_labels, obs)
{
    combined <- data.frame()
    for (i in 1:length(stanfit)) {
        sims <- rstan::extract(stanfit[[i]], pars = model_pars) %>% 
                    as.data.frame
        
        temp <- est_1(sims = sims) %>% as.data.frame
            
        combined <- cbind.fill(combined, temp)
        
        combined <- sapply(1:ncol(combined), function(x)
                    c(combined[, x], '', obs))
        
        combined <- sapply(1:ncol(combined), function(x) 
                        c(combined[, x], 
                         as.vector(round(waic(stanfit[[i]])$waic[1], digits = 2))))
    }
    
    if (missing(pars_labels)) pars_labels <- names(sims)
    names <- rbind(pars_labels, rep('', length(pars_labels))) %>% c
    labels <- c(names, '', 'Obs.', 'WAIC') 
    
    if (missing(col_labels)) 

    combined <- cbind(labels, combined)
    
    return(combined)    
}

#' Internal function for finding individual model runs

est_1 <- function(sims = sims) {
    medians <- sapply(1:ncol(sims), function(x) median(sims[, x]) %>% 
                          round(digits = 2))
    lower_95 <- sapply(1:ncol(sims), function(x) 
                    quantile(sims[, x], probs = 0.025) %>% 
                    round(digits = 2))
    upper_95 <- sapply(1:ncol(sims), function(x) 
                    quantile(sims[, x], probs = 0.975) %>% 
                    round(digits = 2))
    
    cred_interval <- sprintf('(%s, %s)', lower_95, upper_95)
    
    comb <- rbind(medians, cred_interval) %>% c
    return(comb)
}

#' Internal for combining vectors of different lengths

cbind.fill<-function(...){
    nm <- list(...) 
    nm <- lapply(nm, as.matrix)
    n <- max(sapply(nm, nrow)) 
    do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n-nrow(x), ncol(x))))) 
}

test <- stan_speeches_param_est(stanfit = list(first = fit_housing), obs = 918)

