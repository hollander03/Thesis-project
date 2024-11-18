# ATS Lecture on bsvars package

library(bsvars)


# Need to go through how ot select priors myself as it has already been covered and ask Henri for
# his notes

# Exclustion and zero sign restrictions
# Heteroskedatic restrictions

# Narrative and how things originated
# first wave was exclusion [Sims (1981)] all based on theoretic relationships

#hetero restrictions
# Wagner and Zhu in 1994/3 this is based on volatility restrictions
# in addition to making exclusionary restriction we can also make restrictions on the model
# based on the level of volatility (Regime switching)

# Sign restrictions (Uhlig (2005))

# Narrative sign restrictions
# we have infromation about specific events that helps us identify specific shocks
# on the historical deconompostions

# forecasting package that is really good: nixtlar
data("us_fiscal_lsuw")

spec = specify_bsvar$new(us_fiscal_lsuw) # New just means it is new and then the data is
# the brackets

# The ordering of the variables is importatnt go from exogenouse to endogenous

burn = estimate(spec, S = 1000)
post = estimate(burn, S = 10000)

# We do not set the prior, there is a standard one that is set already, but we can change it
#  burn is trying to get into the postier convergence space

# What things are really good from post is posterior and the last draw usting the $

# STructural analysis
irfs = compute_impulse_responses(post , horizon = 12)
fevd = compute_variance_decompositions(post, horizon = 12)
hds  = compute_historical_decompositions(post)
ss   = compute_structural_shocks(post)
csds = compute_conditional_sd(post)
sddr = verify_identification(post)

# Predictive analysis
fvs  = compute_fitted_values(post)
fore = forecast(post, horizon = 12)

# Plots and summaries
plot(irfs)
summary(irfs)

# Dont really play with the prior just yet, we can just run it as is for the time being

#  to not have 0s everywhere you can instead have two covariance matrices that represent
# different regimes i.e. during a crisis vs not during a crisis therefore the structural
# shocks are hetrodkedastic with covariances diag(σ21) and diag(σ22)

# why would the B0 be the same, the structural relationship between variables does not
# change in different periods of volatility, this means that no zero restrictions are
# required

# Is the nominal interest rate determined endogenously or exogenously


# interpolation
# monthly is more jagged therefore you want use yoy rather or use both and see what the
# results are. Also be careful about seasonly adjusted or not

library(bsvars)
load("bsvars_data.rda")
set.seed(123)

N       = ncol(y)
p       = 12
S_burn  = 1e4
S       = 2e4
thin    = 2


spec_lt = specify_bsvar_sv$new(   # specify an SVAR-SV
    as.matrix(y),                   # endogenous variables
    p = p,                          # lag order
    exogenous = x                   # exogenous variables
)
A_ols           = t(solve(        # compute A_ols
    tcrossprod(spec_lt$data_matrices$X),
    tcrossprod(spec_lt$data_matrices$X, spec_lt$data_matrices$Y)
))
spec_lt$prior$A = A_ols           # prior mean of A set to A_ols
spec_lt

# Specify the extended SVAR-SV.

B = matrix(TRUE, N, N)
B[upper.tri(B)] = FALSE
B[3,4] = TRUE
B

spec_ex = specify_bsvar_sv$new(
    as.matrix(y),
    B = B,                          # extended identification
    p = p,
    exogenous = as.matrix(x)
)
spec_ex$prior$A = A_ols

B = matrix(TRUE, N, N)
B

spec_ur = specify_bsvar_sv$new(
    as.matrix(y),
    B = B,                          # unrestricted identification
    p = p,
    exogenous = x
)
spec_ur$prior$A = A_ols

# Estimating the unrestricted
spec_lt %>%
    estimate(S = S_burn, show_progress = FALSE) %>%
    estimate(S = S, thin = thin) -> post_lt


spec_ex %>%
    estimate(S = S_burn, show_progress = FALSE) %>%
    estimate(S = S, thin = thin) -> post_ex

post_ex %>%  compute_impulse_responses(horizon = 24) %>%  plot(probability = 0.68)



