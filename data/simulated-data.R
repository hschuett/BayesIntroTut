library(brms)
options("brms.backend" = "cmdstanr")
# ---------------------------------------------------------------------------------------------


set.seed(492486)
n_firms <- 10
n_quarters <- 12
firm_size <- round(runif(n_firms, min = 10000, 200000))
firm_int <- rnorm(n_firms, -9.5, 0.16)
firm_train <- rnorm(n_firms, -0.25, 0.30)
n_obs <- n_firms * n_quarters
dta0 <- data.frame(
  firm_id = rep(1:n_firms, each = n_quarters),
  quarter = rep(1:n_quarters, times = n_firms),
  firm_size = rep(firm_size, each = n_quarters),
  firm_int = rep(firm_int, each = n_quarters),
  firm_train = rep(firm_train, each = n_quarters),
  training = ifelse(rbinom(n_obs, size = 1, prob = 0.25) > 0, 0, rpois(n_obs, lambda = 3)),
  noise = rnorm(n_obs, 0.1, .05)
)
dta0$eta = log(dta0$firm_size) + dta0$firm_int + dta0$firm_train * dta0$training
dta0$contrdef = rnbinom(n_obs, size = 1, mu = exp(dta0$eta))

table(dta0$training)
table(dta0$contrdef)
collapse::qsu(dta0)

dta0 |> saveRDS("Documents/BayesCoursePres/data/contrdef.rds")



# ---------------------------------------------------------------------------------------------


# prior_summary(simulModel)
dta <- dta0
# dta$contrdef <- newY

bprior2 <- c(
  prior(normal(0, 10), class = "Intercept"),
  prior(normal(0, 2), class="b", coef = "training"),
  prior(normal(0, 1), class = "sd")
)

testModel = brm(
  contrdef | rate(firm_size) ~ 1 + training  + (1 + training| firm_id),
  family = negbinomial,
  data = dta0,
  prior = bprior2,
  chains = 4, cores = 4
)
summary(testModel)
# prior_summary(testModel)
