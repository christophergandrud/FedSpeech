////////////////////////////////
// Fed Speech-Topic Stan Model
// Model Version 0.1
// Stan Version 0.2.6
// Christopher Gandrud
// 2 March 2015
// MIT License
////////////////////////////////

data {
    int<lower=0> N;                     // number of speeches
    int<lower=0> K;                     // number of predictors
    int<lower=0> J;                     // number of speakers
    int<lower=0,upper=J> name[N];       // name of speakers
    matrix[N,K] X;                      // predictor matrix
    int<lower=0,upper=1> y[N];          // topic spoken about
}

parameters {
    vector[K] beta;                     // coefficients for predictors
    real alpha;                         // intercept
    vector[J] a;                        // speaker intercepts
    real<lower=0,upper=100> sigma_a;    // scale of speaker intercept
}

transformed parameters {
    vector[N] y_hat;

    for (n in 1:N)
        y_hat[n] <- X[n] * beta + alpha + a[name[n]];
}


model {
    beta ~ normal(0, 100);
    alpha ~ normal(0, 100);
    a ~ normal(0, sigma_a);

    y ~ bernoulli_logit(y_hat);
}

// For finding WAIC
generated quantities {
    vector[N] log_lik;

    for (n in 1:N) {
        log_lik[n] <- bernoulli_logit_log(y[n], X[n]*beta + alpha + a[name[n]]);
    }
}
