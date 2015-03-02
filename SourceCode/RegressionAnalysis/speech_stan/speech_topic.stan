////////////////////////////////
// Fed Speech-Topic Stan Model
// Model Version 0.0
// Stan Version 0.2.6
// Christopher Gandrud
// 26 February 2015
// MIT License
////////////////////////////////

data {
    int<lower=0> N;                       // number of speeches
    int<lower=0> n_names;                 // number of speakers
    int<lower=0,upper=n_names> name[N];   // name of speakers
    vector<lower=0,upper=1>[N] fed_venue; // speech at fed venue
    vector<lower=0>[N] donor;             // donor connectivity
    vector<lower=0,upper=1>[N] scrutiny;  // scrutiny level
    int<lower=0,upper=1> y[N];            // topic spoken about
}

parameters {
    real alpha;                         // intercept
    vector[3] beta;                     // coefficients
    vector[n_names] a;                  // speaker intercept
    real<lower=0,upper=100> sigma_a;    // scale of speaker intercept
}

transformed parameters {
    vector[N] y_hat;

    for (i in 1:N)
        y_hat[i] <- alpha +
                    beta[1] * fed_venue[i] +
                    beta[2] *  donor[i] +
                    beta[3] * scrutiny[i] +
                    a[name[i]];
}

model {
    a ~ normal(0, sigma_a);
    beta ~ normal(0, 100);

    y ~ bernoulli_logit(y_hat);
}
