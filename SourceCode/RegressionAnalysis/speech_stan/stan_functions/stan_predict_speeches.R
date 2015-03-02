
fitted_1 <- c(0.023, 0, 0, 0.619, 1.0124, 0.9643)
fitted_2 <- c(0.023, 1, 0, 0.619, 1.0124, 0.9643)

fitted_matrix <- rbind(fitted_1, fitted_2)

library(dplyr)
library(boot)
library(tidyr)
library(ggplot2)


pred_prob_out <- data.frame()
for (i in 1:nrow(fitted_matrix)) {
    temp <- fitted_matrix[i, ]
    temp_predict <- predict_1(stanfit = fit_housing, 
                               data = speeches_data_housing,
                               fitted_coefs = temp, a_num = 3)
    
    pred_prob_out <- cbind.all(pred_prob_out, temp_predict)
}

pred_gathered <- pred_prob_out %>% as.data.frame %>% 
                    gather(fitted, prediction)

ggplot(pred_gathered, aes(fitted, prediction, group = 1)) +
    geom_point() +
    stat_smooth(method = 'lm') +
    theme_bw()


#' Helper function
cbind.all <- function (...) 
{
    nm <- list(...)
    nm <- lapply(nm, as.matrix)
    n <- max(sapply(nm, nrow))
    do.call(cbind, lapply(nm, function(x) 
        rbind(x, matrix(, n - nrow(x), ncol(x)))))
}

#' Predict probability for one set of fitted values
#' 
#' @importFrom rstan extract
#' @importFrom dplyr %>%
#' @importFrom boot inv.logit
predict_1 <- function(stanfit, data, fitted_coefs, a_num, betas = 2:7,
                      model_pars = c('beta', 'alpha', 'a')) 
{
    sims <- rstan::extract(stanfit, pars = model_pars) %>% as.data.frame()
    alpha <- sims[, 'alpha']
    betas_coef <- sims[, betas]
    a <- sims[, sprintf('a.%s', a_num)]
    
    fitted_full <- t(replicate(nrow(sims), fitted_coefs))
    
    betas_x <- sapply(1:ncol(betas_coef),
                      function(x) betas_coef[, x] * fitted_full[, x])
    
    raw <- sapply(1:nrow(betas_x),
                  function(x) sum(betas_x[x, ]) + alpha[x] + a[x])
    
    pred_prob <- boot::inv.logit(raw)
    return(pred_prob)
}

