library(tidyverse)
library(performance)
library(modelsummary)

spread<-haven::read_dta("https://github.com/Neilblund/GVPT728-Winter-2026/raw/refs/heads/main/Additional%20Code%20and%20Data/braitwaite_data.dta")|>
  mutate(logsize = log(jointsize))


plot(density(spread$radius))



model1<-lm(log_radius_area ~ 
             territory + 
             host_resource  + 
             host_mt +  
             host_for +  
             water +   
             bord_vital  + 
             logsize  + 
             cwpceyrs , 
           data=spread)

model2<-lm(log_radius_area ~ 
             logcap_ratio + 
             allies +
             joint_democ +
             territory + 
             host_resource  + 
             host_mt +  
             host_for +  
             water +   
             bord_vital  + 
             logsize  + 
             cwpceyrs , 
           data=spread)



model3<-lm(log_radius_area ~ 
             territory + 
             host_resource  + 
             host_mt +  
             water +   
             host_for +  
             bord_vital  + 
             logsize  + 
             cwpceyrs +
             incidents + 
             logdurat +  
             final_hostile 
             , 
           data=spread)

# checking model 1
check_model(model1)

# comparing models
compare_performance(model1, model2, model3)


mlist<-list("Model 1"= model1, 
            "Model 2" = model2, 
            "Model 3" = model3)


# visualizing model results (without any modification)
modelsummary(mlist)


coefnames<-c("territory" = "Territory",
             "host_resource" ='Resources',
             "host_mt" = "Mountains",
             "host_for" = "Forests",
             "water" = "Ocean",
             "bord_vital" = "Vital border",
             "logsize" = "Logged size of states",
             "cwpceyrs" = "Peace years",
             "incidents"= "Number of incidents",
             "logdurat" = "Logged duration",
             "final_hostile" = "Hostility level",
             "allies" = "Alliance", 
             "joint_democ" ="Join democracy",
             "logcap_ratio" ='Logged GDPPC ratio'
             )

# visualizing model results (with modifications)
modelsummary(mlist, 
            coef_rename = coefnames,
            stars=TRUE,
            vcov='robust',
            coef_omit ='Intercept'
            )



modelplot(model1, 
          coef_rename =coefnames, 
          vcov="robust",
          coef_omit ='Intercept'
          
          )







