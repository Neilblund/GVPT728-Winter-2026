library(haven)
library(survival)
library(modelsummary)
# Survival model 
if(!file.exists('DurationWar-R00A.zip')){
  download.file('http://slantchev.ucsd.edu/published/files/DurationWar-R00A.zip',
                destfile='DurationWar-R00A.zip'
  )
}




data<-read_dta(unz('DurationWar-R00A.zip', '1perwar.dta'))|>
  as_factor()


# survival models often have "censoring", where the outcome of interest didn't 
# occur for some respondents by the time the study ends. This isn't the case
# for this data, so we just set the "status" to 1 for all observations:
data$status<-1

model1<-survreg(Surv(months, status) ~  parmil + parpop, 
                data=data, robust=TRUE,
                dist = "loglogistic")
model2<-survreg(Surv(months, status) ~  parmil + parpop + terrain + contig , 
                robust=TRUE,
                data=data, 
                dist = "loglogistic")
model3<-survreg(Surv(months, status) ~  parmil + parpop + terrain + contig +nstates + tpop + tmilper  +demo, 
                robust=TRUE,
                data=data, 
                dist = "loglogistic")


predict(model1, type='response')


mlist<-list('baseline' = model1, 'territory' = model2, 'full' = model3)
modelsummary(mlist, coef_rename=TRUE,
             stars=TRUE,
             statistic = c("conf.int")
             )

# note, our standard errors will be "off" here, but we can get bootstrapped SE 
# for the predicted marginal effects later



nd <- datagrid(
  demo = 0:1,
  parmil = seq(0, 1, by=.1),
  grid_type = "counterfactual",
  model = model3
)

p <- avg_predictions(model3,
                     type = "response",
                     by  =c('parmil','demo'),
                     newdata = nd,
                     vcov = "rsample"  # getting bootstrapped CIs
)


p |>
  mutate(demo = factor(
    demo,
    levels = c(0, 1),
    labels = c("Non-democratic initiator", "Democratic initator")
  )) |>
  ggplot(aes(
    x = parmil,
    y = estimate,
    color = demo,
    fill = demo,
    ymin = conf.low,
    ymax = conf.high
  )) +
  geom_line() +
  geom_ribbon(alpha = .3) +
  theme_bw() +
  scale_color_manual(values=c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values=c("#00AFBB", "#E7B800")) +
  labs(x= "Military parity", y='Predicted duration',
       color='Initiator', 
       fill='Initiator')







