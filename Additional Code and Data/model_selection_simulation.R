
modelSelection<-function(reps= 100, N=100,bic =FALSE){
  # model selection function: performs stepwise selection based on the AIC (default)
  # or the BIC. The actual data generating model is Y ~ X1 * 2  + e
  formulas<-character(reps)
  pvalues<-matrix(ncol=5, nrow=reps)
  colnames(pvalues) <- c("X1", "X2", "X3", "X4", "X5")
  for(i in 1:reps){
    if(bic ==TRUE){
      k<-log(N)
    }else{
      k<-2
    }
    X<-replicate(5, rnorm(N))
    Y<-1 + X[,1] * 2 + rnorm(N, 0, 1) # actual model
    data<-data.frame(Y, X)
    model<-lm(Y ~. , data=data)
    
    best<-step(model, 
               direction = "both", 
               scope = formula(~ .),
               k = k,
               trace=FALSE
    )
    f<-deparse(formula(best)) # return selected formula as a string
    p <-coef(summary(best))   
    # get p-values for selected model:
    pvalues[i, ] <-    p[match( colnames(pvalues), rownames(p)),][,4]
    formulas[i] <- f
    
  }
  
  data<-data.frame(formulas, pvalues)
  
  return(data)
}

set.seed(100)

# Model selection using the AIC: 
aic_reps<-modelSelection(reps=500)  # correct model chosen ~ 50% of the time
aic_reps$formulas|>
  table()|>
  sort(decreasing=TRUE)|>
  prop.table()



rowSums(aic_reps[,3:ncol(aic_reps)]<.05, na.rm=T) |> 
  table()|>
  prop.table()|>
  sort()|>
  cumsum()              # 20% of models have at least one spurious correlation


# Model selection using the BIC

bic_reps<-modelSelection(reps=500, bic=TRUE) 
bic_reps$formulas|>   # correct model chosen ~ 80%
  table()|>
  sort(decreasing=TRUE)|>
  prop.table()


rowSums(bic_reps[,3:ncol(bic_reps)]<.05, na.rm=T)|> 
  table()|>
  prop.table()|>
  sort()|>
  cumsum()            # ~14% of models have a spurious correlation

