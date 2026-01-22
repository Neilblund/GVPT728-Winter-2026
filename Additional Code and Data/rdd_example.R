library(tidyverse)

# Loading and cleaning the data 
results<-read_csv('https://raw.githubusercontent.com/fivethirtyeight/election-results/refs/heads/main/election_results_house.csv')


# only general elections after 1990
elections_subset<-results|>
  filter(cycle>=1990)|>
  filter(stage == "general")|>
  filter(special==FALSE)|>
  filter(unopposed==FALSE | is.na(unopposed))|>
  group_by(office_id, cycle)|>
  arrange(votes)|>
  slice_tail(n =2)|>
  filter(all(c("DEM", "REP") %in% ballot_party))|>
  filter(all(is.na(ranked_choice_round )))|>
  ungroup()


elections_wide<-elections_subset|>
  select(cycle, state_abbrev, office_seat_name,  ballot_party, votes)|>
  pivot_wider(names_from = ballot_party ,values_from =votes)|>
  mutate(demshare = (DEM/(DEM+REP)) -.5,
         repshare = (REP/(DEM  + REP)) -.5,
         dem_winner = ifelse(DEM>REP, TRUE, FALSE)
         )|>
  group_by(state_abbrev, office_seat_name)|>
  arrange(cycle)|>
  mutate(dem_incumbent = lag(dem_winner),
         lag_demshare = lag(demshare)
         )|>
  drop_na()



# Assuming a linear effect ----
ggplot(elections_wide, aes(x=lag_demshare, y=demshare, color=dem_incumbent)) + 
  geom_point(alpha=.05) +
  theme_bw() +
  labs(x="Prior D. voteshare", y="D. voteshare", color='Incumbent party') +
  geom_smooth(method='lm') +
  geom_vline(xintercept=0, lty=2)



model_simple <- lm(demshare ~ lag_demshare + dem_incumbent,
                   data = elections_wide)
broom::tidy(model_simple)
 
## Fitting on a narrow range around the discontinuity - the blowouts  ----
## don't tell us anything

elections_wide|>
  filter(lag_demshare >=-.1 & lag_demshare <=.1)|>
ggplot(aes(x=lag_demshare, y=demshare, color=dem_incumbent)) + 
  geom_point(alpha=.05) +
  theme_bw() +
  labs(x="Prior D. voteshare", y="D. voteshare", color='Incumbent party') +
  geom_smooth(method='lm') +
  geom_vline(xintercept=0, lty=2)

model_simple <- lm(demshare ~ lag_demshare + dem_incumbent,
                   data = elections_wide|>filter(lag_demshare >=-.1 & lag_demshare <=.1))
broom::tidy(model_simple)


## Non-parametric estimation with local smoothing and automatic bin selection

library(rdrobust)
rdrobust(y = elections_wide$demshare,
         x = elections_wide$lag_demshare, 
         c = 0)|>
  summary()



rdplot(y = elections_wide$demshare,
         x = elections_wide$lag_demshare, 
         c = 0
       )

# View a bunch of different options for the bandwidth

rdbwselect(y = elections_wide$demshare,
           x = elections_wide$lag_demshare, 
           c = 0, all=TRUE)|>
  summary()


# try a different bandwidth setting to see what changes
rdrobust(y = elections_wide$demshare,
         x = elections_wide$lag_demshare, 
         c = 0,
         h = 0.065
         )|>
  summary()





