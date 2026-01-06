library(tidyverse)
library(performance)
library(ggrepel)


florida<-read_csv("https://raw.githubusercontent.com/Neilblund/GVPT728-Winter-2026/refs/heads/main/Additional%20Code%20and%20Data/florida_votes.csv")|>
  mutate(N = Bush + Gore + Brow + Nade + Har + Hag + Mc + Ph + Mo,
         Buc_sqrt = sqrt(Buc)
         )

model_data<-florida|>
  mutate(N = N^.5, 
         whit = Whi * N,
         lhisp = log(Hisp) * N,
         o65 = `Over 65` * N,
         hsed = HS * N,
         inco = Inc * N,
         pbrow = (Brow/(Bush + Gore + Brow + Nade + Har + Hag + Mc + Ph + Mo + Buc)) * N
         )



model<-model_data|>
  filter(County !="Palm Beach")|>
  lm(Buc_sqrt ~ 0 + N + whit + o65 + hsed + inco + pbrow, data=_)


model_data$preds<-predict(model, newdata=model_data) ^ 2

ggplot(model_data, aes(x=preds, y=Buc, label=County)) + geom_point() +
  ylab("Actual vote") +
  xlab("Predicted vote") +
  geom_label_repel()


# predicted value for Palm Beach:

predict(model, newdata = model_data|>filter(County =="Palm Beach"), interval='prediction' ) ^ 2


# model fit

check_model(model)



model2<-model_data|>
  lm(Buc_sqrt ~ 0 + N + whit + o65 + hsed + inco + pbrow, data=_)

check_model(model2)



