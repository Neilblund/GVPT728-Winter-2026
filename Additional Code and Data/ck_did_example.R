library(tidyverse)

# data from Card and Krueger 1994. 
# Card's original data is here, but its in wide format: 
# https://davidcard.berkeley.edu/data_sets


url<-'https://github.com/Neilblund/GVPT728_Winter24/raw/refs/heads/main/Additional%20Code%20and%20Data/ck_longformat.rds'
file<-tempfile()
download.file(url, dest=file, mode='wb')
ckdata<-readRDS(file)|>
  ungroup()


# dv = FTE 
# time period indicator = period
# state indicator = state


# looking at outcomes: 
ggplot(data=ckdata, aes(x=state, y=fte, fill=period)) + 
  geom_boxplot(notch=TRUE) +
  theme_bw() +
  scale_fill_brewer(palette='Dark2')





## Baseline DiD
model<-lm(fte ~ period  * state, data=ckdata )
summary(model)

# DID with control for bonus programs for new workers at time 1



lm(fte ~ period * state + bonus, data=ckdata)|>
  summary()

lm(fte ~state , data=ckdata)|>
  summary()


# DiD with temporarily closed cases in New Jersey set to zero employment

ckdata$fte_temp_closed<-ifelse(ckdata$status %in% c("closed for renovations",
                                                    "closed due to Mall fire",
                                                    "closed for highway construction"
                                                    ) & ckdata$period == "Wave 2"
                               & ckdata$state == "NJ"
                               , 0, ckdata$fte)




lm(fte_temp_closed ~ period * state , data=ckdata)|>
  summary()

lm(fte_temp_closed ~ period * state + bonus  , data=ckdata)|>
  summary()


# Accounting for clustering-------

# We should probably account for clustering by restaurant here...
library(sandwich)  
library(lmtest)    

model_clustered  <- coeftest(model, 
                             vcov = vcovCL,
                             type = "HC1",
                             cluster = ~sheet)




model_clustered


# Equivalent in feols with fixed effects for period and state------
library(fixest)

ckdata|>
  mutate(minwage = ifelse(period == "Wave 2" & state=="NJ", "treated", "not treated"))|> 
  feols( fte ~ minwage | period  + state, data=_, 
       cluster=~sheet)|>
  broom::tidy()





# Equivalent using difference in employment at Wave 2 - Wave 1
# (note that this setup only really works with panel data because we need to get 
# the differences for individual cases)
ckdata|>
  select(state, sheet,  period, status, fte)|>
  pivot_wider(names_from = period, values_from =fte)|>
  mutate(difference = `Wave 2` - `Wave 1`)|>
  lm(difference ~ state, data=_)|>
  broom::tidy()








