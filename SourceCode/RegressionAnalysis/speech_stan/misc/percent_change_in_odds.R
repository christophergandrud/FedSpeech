# ---------------------------------------------------------------------------- #
# Function to convert logistic regression coefficients to percent odds change
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

# Create function

pred_change <- function(x) {
    require(dplyr)
    exp_x <- exp(x)
        (exp_x - 1) * 100 %>%
        round(digits = 2) %>%
        return
}

# Find percentage change in the odds.
# Coefficients from Stan output

# Speaking about monetary policy for change in housing prices
pred_change(0.06)

# Speaking about financial markets for change in housing prices
pred_change(-0.06)

# Speaking about financial markets during periods of high congressional stress
pred_change(1.29)

# Speaking about monetary policy at a donor event
pred_change(-3.67)

# Speaking about banking regulation at a donor event
pred_change(2.67)

