
fitted_1 <- c(0.023, 0, 0, 0.619, 1.0124, 0.9643)
fitted_2 <- c(0.023, 1, 0, 0.619, 1.0124, 0.9643)

fitted_matrix <- rbind(fitted_1, fitted_2)

library(dplyr)
library(boot)
library(ggplot2)


pred_prob_out <- data.frame()
for (i in 1:nrow(fitted_matrix)) {
    temp <- fitted_matrix[i, ]
    temp_predict <- predict_1(stanfit = fit_housing, 
                               data = speeches_data_housing,
                               fitted_coefs = temp, a_num = 3)
    
    pred_prob_out <- rbind(pred_prob_out, temp_predict)
}

pred_prob_out <- cbind(x_value = 0:1, pred_prob_out)
    
    
ggplot(pred_prob_out, aes(x_value, median, group = 1)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower_50, ymax = upper_50), alpha = 0.1) +
    geom_ribbon(aes(ymin = lower_95, ymax = upper_95), alpha = 0.1) +
    theme_bw()

#' Predict probability for one set of fitted values
#' 
#' @importFrom rstan extract
#' @importFrom dplyr %>%
#' @importFrom boot inv.logit
#' @importFrom SPIn SPIn
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
    
    spin_95 <- SPIn(pred_prob, conf = 0.95)
    spin_50 <- SPIn(pred_prob, conf = 0.5)
    spin_lower95 <- 
    
    pred_prob_summary <- data.frame(
        lower_95 = spin_95$spin[1],
        lower_50 = spin_50$spin[1],
        median = median(pred_prob),
        upper_50 = spin_50$spin[2],
        upper_95 = spin_95$spin[2]
    )
    return(pred_prob_summary)
}

