# example of a count model

library(tidyverse)
library(ggeffects)
library(marginaleffects)
vdem<-vdemdata::vdem

# getting a count of coups since 2000
data_subset<-vdem|>
  group_by(country_name)|>
  arrange(year)|>
  filter(year>=1945)|>
  mutate(
    e_pt_coup_attempts = if_else(is.na(e_pt_coup_attempts), 0, e_pt_coup_attempts),
    cumulative_coups = lag(cumsum(e_pt_coup_attempts)))|>
  ungroup()|>
  filter(year >= 2000)|>
  arrange(year)|>
  group_by(country_name)|>
  summarise(coup_attempts = sum(e_pt_coup_attempts), 
            liberal_democracy =v2x_libdem[1] ,      # liberal democracy
            gdp = e_gdppc[1],                       # GDP
            corruption = v2xnp_regcorr[1],          # regime corruption
            cumulative_coups = cumulative_coups[1], # count of coups from 1945 - 1999
            logpop = log(e_pop)
  )|>
  drop_na()

# Count model with poisson data 
 
poisson_model<-glm(coup_attempts ~ liberal_democracy + gdp +  cumulative_coups  ,  data = data_subset, family='poisson')

summary(poisson_model)



check_model(poisson_model)
check_overdispersion(poisson_model)



# Count model with negative binomial data 

negbin_model<-MASS::glm.nb(coup_attempts ~ liberal_democracy + gdp + cumulative_coups,
                           data = data_subset)


summary(negbin_model)

check_model(negbin_model)


predict_response(negbin_model)|>
  plot()

# getting average marginal effects


avg_comparisons(negbin_model)



# This will be slow ! ----
library(rstanarm)

negbin_bayes<-stan_glm.nb(coup_attempts ~ liberal_democracy + gdp + cumulative_coups
                          data =data_subset)

avg_comparisons(negbin_bayes)


