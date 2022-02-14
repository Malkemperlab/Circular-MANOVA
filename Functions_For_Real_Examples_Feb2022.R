

###### FUNCTIONS for MANOVA analyses 

#defining two helper functions 
Reducing_to_modulo_rad <- function (x) {x=deg(x); x=ifelse((x>(360)),(x-360),(x));rad(x)}
DoublingRad <- function (x) {x=Reducing_to_modulo_rad(x*2)}

################
#MANOVA model selection for angular data _  AIC based  (pre-selection based on effect sizes possible)

####################
# input below for function testing only, commented out 
#RESPONSE_Mat<-RESPONSE_deer;MODEL_FULL<-MANOVA_deer_full

#input a response matrix with two columns (cosine and sine of the circular response variable in radians )
#  the full model (result of  summary function on manova, without intercept - see examples) 
# and the maximum number of terms to be evaluated (terms are ranked based on effect size 
# and the input order the lowest ones are deleted, in order to reach the desired number 
# - larger number of terms slows down the function by a lot)
#returns a list with all the models tested, a manova summary for the top ranked model, as well as the effect size (partial eta squared) for each of the terms.
#can be accessed using list[[1]] for the table with the AICs for each model,
#list[[2]] for the manova summary object of the "best" model
#and list [[3]] for the effect sizes 

manova_AIC_selection<-function(RESPONSE_Mat,MODEL_FULL,n=NULL) {
  
  #libraries that are required
  require(stringi)
 
   # define the variables to be tested (also explicitly add interaction terms) and 
  # determine the order of the terms and delete terms which will not be evaluted based on "n" input 
  variable_1<-paste("RESPONSE_Mat ~ ",paste(MODEL_FULL[["row.names"]][-length(MODEL_FULL[["row.names"]])],collapse="+"))
  formula_1<-as.formula(variable_1)
  effect_1<-lapply(summary(aov(formula_1)), effectsize::eta_squared) #calculate effect size for each anova 
  effect_1<-effect_1$` Response 1`$Eta2_partial+effect_1$` Response 2`$Eta2_partial #calculate  effect size for the terms 
  
  # order after effect size 
  variable_effect<-cbind(MODEL_FULL[["row.names"]][-length(MODEL_FULL[["row.names"]])],effect_1)
  variable_effect_sorted <- variable_effect[order(-effect_1),] 
  
  #set the default if n is not given
  if(is.null(n))
    n <- nrow(variable_effect_sorted)
  
  #remove terms that are not used in AIC based model selection 
  
  n_u <-if (n>nrow(variable_effect_sorted)) {nrow(variable_effect_sorted)} else {n} #if one would put in a larger number than terms present in the model the number of model terms in the input model are used 
  variables<-variable_effect_sorted[c(1:n_u),1]
  
  #create all combinations and collapse adding "+" and store the text in the file forumlas 
  id <- unlist(lapply(1:length(variables), function(i)combn(1:length(variables),i,simplify=FALSE)),recursive=FALSE)
  formulas <- sapply(id,function(i) paste("RESPONSE_Mat[,x]~ ",paste(variables[i],collapse="+")))
  formulas <- c(formulas,"RESPONSE_Mat[,x]~ 1") #add the intercept model 
  
  #prepare the data frame for the result 
  AIC_mean_df_collection<-data.frame(formulas,rep("NA",length(formulas)));colnames(AIC_mean_df_collection)<-c("formula","AIC")
  
  
  #test<-t(as.data.frame(formulas))
  AIC_mean_df_collection[,2]<-sapply (1:length(formulas), function (i) { mean(sapply(1:ncol(RESPONSE_Mat),
                                                                                     function(x) AIC(aov(as.formula(formulas[i])))))})
  
  #order using the number of terms
  #first add column with termnumber 
  AIC_mean_df_collection$number.of.terms = stringi::stri_count(AIC_mean_df_collection$formula, fixed = "+")
   #order using AIC value and number of terms (best model has highest number of terms and lowest p-value)
  AIC_mean_df_collection<-AIC_mean_df_collection[order(AIC_mean_df_collection$AIC,-AIC_mean_df_collection$number.of.terms),]
  
  #use best model
  
  BEST_MODEL<-unlist(gsub(",x",",", AIC_mean_df_collection[1,1]))
  
  MANOVA_summary<-summary(manova(as.formula(BEST_MODEL))) #after eliminating non significant variables in a step-wise manner 
  
  #calculate effect size for best model  
  effect_best<-lapply(summary(aov(as.formula(BEST_MODEL))), effectsize::eta_squared) #calculate effect size for each anova 
  effect_best<-effect_best$` Response 1`$Eta2_partial+effect_best$` Response 2`$Eta2_partial #calcaulte  efefct size for the terms 
  
  #order after effect size
  
  variable_best<-paste("RESPONSE_Mat ~ ",paste(MANOVA_summary[["row.names"]][-length(MANOVA_summary[["row.names"]])],collapse="+"))
  formula_best<-as.formula(variable_best)
  
  variable_effect_best<-cbind(MANOVA_summary[["row.names"]][-length(MANOVA_summary[["row.names"]])],effect_best)
  variable_effect_sorted_best <- variable_effect_best[order(-effect_best),]
  Effect_size_final<-variable_effect_sorted_best
  
  variable_best_final<-paste("RESPONSE_Mat ~ ",paste(variable_effect_sorted_best[,1],collapse="+"))
  MANOVA_summary_final<-summary(manova(as.formula(variable_best_final)),intercept = T) #after eliminating non significant variables in a step-wise manner 
  
  return(list(AIC_mean_df_collection,MANOVA_summary_final,Effect_size_final))
}





