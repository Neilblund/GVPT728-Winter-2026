library(tidyverse)
library(modelsummary)
library(mice)

rescale_neg1_to_1 <- function(x) {
  min_val <- min(x, na.rm = TRUE)
  max_val <- max(x, na.rm = TRUE)
  
  scaled_x <- 2 * ((x - min_val) / (max_val - min_val)) - 1
  
  return(scaled_x)
}

vdem <- vdemdata::vdem |>
  filter(year == 2000) |>
  select(
    gdpp = e_gdppc,
    civil_liberties = v2x_civlib ,
    population = e_pop  ,
    democracy = e_chga_demo,
    oil_income = e_total_oil_income_pc,
    life_expectancy = e_pelifeex,
    education_15plus = e_peaveduc,
    educational_inequality = e_peedgini,
    mass_mobilization = v2cagenmob,
    public_corruption =v2x_pubcorr
  ) |>
  mutate(population = log(population),
         gdpp = log(gdpp)
         )|>
  drop_na()|>
  ungroup()


set.seed(6000)


vdem_mcar<-vdem
r<-rbinom(nrow(vdem),1,.5)
vdem_mcar$civil_liberties<-ifelse(r == 1, NA,vdem$civil_liberties)


vdem_mar<-vdem

scaled<-scale(1+ vdem$educational_inequality * vdem$public_corruption + vdem$gdpp^2  *vdem$democracy)
r <- rbinom(nrow(vdem), size = 1, prob = pnorm(scaled))
vdem_mar$civil_liberties<-ifelse(r == 1, NA,vdem$civil_liberties)

vdem_mnar<-vdem
r <- rbinom(nrow(vdem), size = 1, prob = pnorm(scale(vdem$public_corruption)))
vdem_mnar$civil_liberties<-ifelse(r == 1, NA,vdem$civil_liberties)



model_form<-formula(public_corruption ~ civil_liberties)
main_model<-lm(model_form, data=vdem)
mcar <- lm(model_form, data=vdem_mcar)
mar <- lm(model_form, data = vdem_mar)
mnar <- lm(model_form, data = vdem_mnar)

mlist<-list("full" = main_model, 
            "MCAR" = mcar, 
            "MAR" = mar, 
            "MNAR" = mnar)

modelsummary(mlist, 
             gof_map = 'nobs') # only showing number of observations



# MCAR imputation-----

imputed_mcar <- mice(vdem_mcar, m=10, method='pmm',
                    seed = 233,
                    print=TRUE)

imputed_mcar$data


mcar_imputed <- with(data=imputed_mcar, exp=lm(public_corruption ~ civil_liberties))


# Pool the results
pooled_results <- pool(mar_imputed)


mlist<-list("full" = main_model,
            "MCAR" = mcar,
            "MCAR imputed" = pooled_results
)

modelsummary(mlist)


# MAR imputation-----

imputed_mar <- mice(vdem_mar, m=10, method='pmm',
                    seed = 233,
                     print=TRUE)


mar_imputed <- with(data=imputed_mar, exp=lm(public_corruption ~ civil_liberties))

# Pool the results
pooled_results <- pool(mar_imputed)


mlist<-list("full" = main_model,
            "MAR" = mar,
            "MAR imputed" = pooled_results
            )


modelsummary(mlist)


# MNAR imputation-----


imputed_mnar <- mice(vdem_mnar, m=10, method='pmm',
                    seed = 233,
                    print=TRUE)


mnar_imputed <- with(data=imputed_mnar, exp=lm(public_corruption ~ civil_liberties))

# Pool the results
pooled_results <- pool(mnar_imputed)


mlist<-list("full" = main_model,
            "MNAR" = mnar,
            "MNAR imputed" = pooled_results
)


modelsummary(mlist)


