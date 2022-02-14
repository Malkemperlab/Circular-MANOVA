#hypothetical animal study with year effect 

#load libraries
library(compiler)
library(circular)
library(NPCirc)
library(CircMLE)
library(foreach)
library(parallel)
options("scipen" = 10)
enableJIT(1)

##########
rans <- 9999 #set the number of iterations 

#below the sample size (n) sequence is set for the looping 
nvseq_1 <- round(seq(1,10,length.out =5),0) #takes 5 n numbers between 1 and 10 -> c(1,3,6,8,10) in this case 
n_list_1 <- list(nvseq_1,nvseq_1,nvseq_1,nvseq_1,nvseq_1,nvseq_1,nvseq_1,nvseq_1)

#Preparing the directions for the samples 
dir <- rad(180) #setting the direction all the others are based on
mn <- rep(dir,10) #vector with 10 times base direction (both groups have same mean direction)
mg <- c(rep(dir,5),rep(dir+rad(90),5)) #setting the directions for both groups 90 degree apart
mg2 <- c(rep(dir,5),rep(dir+rad(180),5)) #setting the directions for both groups 90 degree apart
m_list_1 <- list(mn,mn,mn,mn,mg,mg,mg2,mg2) #arranging the scenarios in a list for the looping

#Preparing kappa (strength of orientation) for each of the groups and years 

#the base scenarios more and less clustered, each for an n=5, for each of the two groups  
k_more_clustered <- seq(2,4,length.out = 5) #the "more clustered" base level ranging form kappa=2 to 4
k_less_clustered <- seq(0,2,length.out = 5) #the "less clustered" base level ranging form kappa=0 to 2

#preparing the possible combinations between the base scenarios, adding a random scenario (k0)
k_equal_less <- c(k_less_clustered,k_less_clustered)
k_equal_more <- c(k_more_clustered,k_more_clustered)
k_unequal <- c(k_more_clustered,k_less_clustered)
k0 <- rep(0,10)

#arranging all scenarios in a list in order to loop through them 
k_list_1 <- list(k0,k_equal_less,k_equal_more,k_unequal,k_equal_less,k_unequal,k_equal_less,k_unequal)

#Two types of distributions von Mises and uniform
type_vm <- rep("vm",10)
type_unif <- rep("unif",10)

#arranging the distribution types for each of the loops
type_list_1 <- list(type_unif,type_vm,type_vm,type_vm,type_vm,type_vm,type_vm,type_vm)  

#preparing a vector to call the correct "param" argument in the rcircmix function
param_vm <- c("param_vm1","param_vm2","param_vm3","param_vm4","param_vm5","param_vm6","param_vm7","param_vm8","param_vm9","param_vm10")
param_unif <- rep("param_unif",10) 

#arrange them in a list for looping
param_list_1 <- list(param_unif,param_vm,param_vm,param_vm,param_vm,param_vm,param_vm,param_vm)  

#preparing the names for the files - which will be added automatically at the end of the script
Name=c("Hypothetical_example_Type1error","Hypothetical_example_equal_less",
       "Hypothetical_example_equal_more","Hypothetical_example_unequal",
       "Hypothetical_example_equal_less_90degdiff","Hypothetical_example_unequal_90degdiff",
       "Hypothetical_example_equal_less_180degdiff","Hypothetical_example_unequal_180degdiff")

#start the outer loop one loop for each of the scenarios tested 
for (i in 1:length(Name)) {
  
  #grabbing the correct vectors for the simulation from the list
  nseq <- n_list_1[[i]]
  k1 <- k_list_1[[i]]
  m1 <- m_list_1[[i]]
  distt <- type_list_1[[i]]
  plist <- param_list_1[[i]]
  
   ####### Prepare environment for the foreach loop (setting cores and parallel processing)
  ncore<-detectCores()
  cl <- parallel::makeCluster(ncore-1, setup_strategy = "sequential")#first number changes the cores used
  doParallel::registerDoParallel(cl)
  
  #starting the foreach loop, results will be 'rbound'  
Power_df <- foreach(e=1:length(nseq), .combine='rbind',.packages=c("circular","NPCirc","CircMLE")) %dopar%{
  
  n <- nseq[e] #grabbing the appropriate sample number from the vector set above

  #here the distributions param's are prepared    
param_vm1 <- function(x) {list(p=1,  mu=m1[1], con=k1[1])}
param_vm2 <- function(x) {list(p=1,  mu=m1[2], con=k1[2])}
param_vm3 <- function(x) {list(p=1,  mu=m1[3], con=k1[3])}
param_vm4 <- function(x) {list(p=1,  mu=m1[4], con=k1[4])}
param_vm5 <- function(x) {list(p=1,  mu=m1[5], con=k1[5])}
param_vm6 <- function(x) {list(p=1,  mu=m1[6], con=k1[6])}
param_vm7 <- function(x) {list(p=1,  mu=m1[7], con=k1[7])}
param_vm8 <- function(x) {list(p=1,  mu=m1[8], con=k1[8])}
param_vm9 <- function(x) {list(p=1,  mu=m1[9], con=k1[9])}
param_vm10 <- function(x) {list(p=1,  mu=m1[10], con=k1[10])}

param_unif <- function(x) {list(p=1,  mu=m1[1], con=k1[1])}


###### set the distribution for each year and then combine the distributions   
Year1 <- sapply(1:rans, function(x) rcircmix(n,dist = distt[1],
                                             param=get(plist[1])()))
Year2 <- sapply(1:rans, function(x) rcircmix(n,dist = distt[2],
                                                    param=get(plist[2])()))
Year3 <- sapply(1:rans, function(x) rcircmix(n,dist = distt[3],
                                                    param=get(plist[3])()))
Year4 <- sapply(1:rans, function(x) rcircmix(n,dist = distt[4],
                                                    param=get(plist[3])()))
Year5 <- sapply(1:rans, function(x) rcircmix(n,dist = distt[5],
                                                    param=get(plist[5])()))
Year1_2 <- sapply(1:rans, function(x) rcircmix(n,dist = distt[6],
                                             param=get(plist[6])()))
Year2_2 <- sapply(1:rans, function(x) rcircmix(n,dist = distt[7],
                                             param=get(plist[7])()))
Year3_2 <- sapply(1:rans, function(x) rcircmix(n,dist = distt[8],
                                             param=get(plist[8])()))
Year4_2 <- sapply(1:rans, function(x) rcircmix(n,dist = distt[9],
                                             param=get(plist[9])()))
Year5_2 <- sapply(1:rans, function(x) rcircmix(n,dist = distt[10],
                                             param=get(plist[10])()))

#combining the distributions in one overall distributions 
Distribution <- rbind(Year1,Year2,Year3,Year4,Year5,Year1_2,Year2_2,Year3_2,Year4_2,Year5_2)

#preparing the linear (year) and factorial covariates (group)
year_cov <- c(rep(1,n),rep(2,n),rep(3,n),rep(4,n),rep(5,n),rep(1,n),rep(2,n),rep(3,n),rep(4,n),rep(5,n))
group_fact <- c(rep("g1",length(year_cov)/2),rep("g2",length(year_cov)/2))

#preparing the random samples (for calculating the randomization versions of the test) 
random_sample <- sapply(1:rans, function(x)rcircmix(nrow(Distribution),model = 1))

#applying the HR Test

#calculating T-value for random sample 
random_T <- apply(random_sample,2,function(x) CircMLE:::HermansRasson2T(x)) #here change the fucntion 

#calculating T-value for sample of interest
HR.res.T <- apply(Distribution,2,function(x) CircMLE:::HermansRasson2T(x)) # here change the function

#calculating the p-value 
HR.res.p <- vapply(1:rans,function(x) sum(1*(HR.res.T[x])< random_T)/rans, FUN.VALUE = numeric(1))

#applying the Rayleigh test on the samples 
Rayleigh <- (apply(Distribution,2,rayleigh.test))
Ray <- array(as.matrix(unlist(Rayleigh)), dim=c(4, rans))
RayP <- Ray[2,]

#applying the MANOVA approach, only adding intercept
funManCirc <- function(x) summary(manova(cbind(cos(x),sin(x)) ~ 1),intercept=T)
MANOVA_approach <- apply(Distribution,2, FUN=funManCirc)
MANOVA_P <- sapply(c(1:rans),function(x) MANOVA_approach[[x]][["stats"]][1,6])

#applying the MANOVA approach using randomization to calculate the P-value 
random_MANOVA <- apply(random_sample,2,function(x) funManCirc(x))
random_MANOVA_F <- sapply(c(1:rans),function(x) random_MANOVA[[x]][["stats"]][1,3])
MANOVA_F <- sapply(c(1:rans),function(x) MANOVA_approach[[x]][["stats"]][1,3])
random_MANOVA_P <- vapply(1:rans,function(x) sum(1*(MANOVA_F[x]< random_MANOVA_F))/rans, FUN.VALUE = numeric(1))

#applying the MANOVA approach, adding intercept and linear covariate
funManCirc_lin <- function(x) summary(manova(cbind(cos(x),sin(x)) ~ year_cov),intercept=T)
MANOVA_approach_lin <- apply(Distribution,2, FUN=funManCirc_lin)
MANOVA_P_lin <- sapply(c(1:rans),function(x) MANOVA_approach_lin[[x]][["stats"]][1,6])
MANOVA_P_lin_cov <- sapply(c(1:rans),function(x) MANOVA_approach_lin[[x]][["stats"]][2,6])

#applying the MANOVA approach using randomization to calculate the P-value - with added linear covariate
random_MANOVA_lin<-apply(random_sample,2,function(x) funManCirc_lin(x))
random_MANOVA_F_lin <- sapply(c(1:rans),function(x) random_MANOVA_lin[[x]][["stats"]][1,3])
MANOVA_F_lin <- sapply(c(1:rans),function(x) MANOVA_approach_lin[[x]][["stats"]][1,3])
random_MANOVA_P_lin<-vapply(1:rans,function(x) sum(1*(MANOVA_F_lin[x]< random_MANOVA_F_lin))/rans, FUN.VALUE = numeric(1))

#now calculate P value for linear covariate
random_MANOVA_F_lin_cov <- sapply(c(1:rans),function(x) random_MANOVA_lin[[x]][["stats"]][2,3])
MANOVA_F_lin_cov <- sapply(c(1:rans),function(x) MANOVA_approach_lin[[x]][["stats"]][2,3])
random_MANOVA_P_lin_cov <- vapply(1:rans,function(x) sum(1*(MANOVA_F_lin_cov[x]< random_MANOVA_F_lin_cov))/rans, FUN.VALUE = numeric(1))

#applying the MANOVA approach, added linear and grouping factor
funManCirc_lin_group <- function(x) summary(manova(cbind(cos(x),sin(x)) ~ year_cov+group_fact),intercept=T)
MANOVA_approach_lin_group <- apply(Distribution,2, FUN=funManCirc_lin_group)
MANOVA_P_lin_group <- sapply(c(1:rans),function(x) MANOVA_approach_lin_group[[x]][["stats"]][1,6])
MANOVA_P_lin_group_cov <- sapply(c(1:rans),function(x) MANOVA_approach_lin_group[[x]][["stats"]][2,6])
MANOVA_P_lin_group_cov_group <- sapply(c(1:rans),function(x) MANOVA_approach_lin_group[[x]][["stats"]][3,6])


#applying the MANOVA approach using randomization to calculate the P-values for intercept, covariate and grouping variable
random_MANOVA_lin_group <- apply(random_sample,2,function(x) funManCirc_lin_group(x))
random_MANOVA_F_lin_group <- sapply(c(1:rans),function(x) random_MANOVA_lin_group[[x]][["stats"]][1,3])
MANOVA_F_lin_group <- sapply(c(1:rans),function(x) MANOVA_approach_lin_group[[x]][["stats"]][1,3])
random_MANOVA_P_lin_group <- vapply(1:rans,function(x) sum(1*(MANOVA_F_lin_group[x]< random_MANOVA_F_lin_group))/rans, FUN.VALUE = numeric(1))

random_MANOVA_F_lin_group_cov <- sapply(c(1:rans),function(x) random_MANOVA_lin_group[[x]][["stats"]][2,3])
MANOVA_F_lin_group_cov <- sapply(c(1:rans),function(x) MANOVA_approach_lin_group[[x]][["stats"]][2,3])
random_MANOVA_P_lin_group_cov <- vapply(1:rans,function(x) sum(1*(MANOVA_F_lin_group_cov[x]< random_MANOVA_F_lin_group_cov))/rans, FUN.VALUE = numeric(1))

random_MANOVA_F_lin_group_cov <- sapply(c(1:rans),function(x) random_MANOVA_lin_group[[x]][["stats"]][3,3])
MANOVA_F_lin_group_cov_group <- sapply(c(1:rans),function(x) MANOVA_approach_lin_group[[x]][["stats"]][3,3])
random_MANOVA_P_lin_group_cov_group <- vapply(1:rans,function(x) sum(1*(MANOVA_F_lin_group_cov_group[x]< random_MANOVA_F_lin_group_cov))/rans, FUN.VALUE = numeric(1))


#####Calculate Power for each of the approaches   
Rayleigh <- sum(1*(RayP< 0.05))/rans
HR <- sum(1*(HR.res.p< 0.05))/rans
MANOVA <- sum(1*(MANOVA_P < 0.05))/rans
MANOVA_random <- sum(1*(random_MANOVA_P < 0.05))/rans
MANOVA_lin <- sum(1*(MANOVA_P_lin < 0.05))/rans
MANOVA_random_lin <- sum(1*(random_MANOVA_P_lin < 0.05))/rans
MANOVA_lin_cov <- sum(1*(MANOVA_P_lin_cov < 0.05))/rans
MANOVA_random_lin_cov <- sum(1*(random_MANOVA_P_lin_cov < 0.05))/rans
MANOVA_lin_group <- sum(1*(MANOVA_P_lin_group < 0.05))/rans
MANOVA_random_lin_group <- sum(1*(random_MANOVA_P_lin_group < 0.05))/rans
MANOVA_lin_group_cov <- sum(1*(MANOVA_P_lin_group_cov < 0.05))/rans
MANOVA_random_lin_group_cov <- sum(1*(random_MANOVA_P_lin_group_cov < 0.05))/rans
MANOVA_lin_group_cov_group <- sum(1*(MANOVA_P_lin_group_cov_group < 0.05))/rans
MANOVA_random_lin_group_cov_group <- sum(1*(random_MANOVA_P_lin_group_cov_group < 0.05))/rans

#prepare data.frame to store the power results in and then add the data 
Power <- data.frame(matrix(nrow = 1, ncol = 14))
names(Power) <-c("Rayleigh","HR","MANOVA_intercept","MANOVA_random_intercept",
                "MANOVA_linear_interceptP","MANOVA_random_linear_interceptP",
                "MANOVA_linear_covariateP","MANOVA_random_linear_covariateP",
                "MANOVA_linear_group_interceptP","MANOVA_random_linear_group_interceptP", 
                "MANOVA_linear_group_covariateP","MANOVA_random_linear_group_covariateP", 
                "MANOVA_linear_group_groupP","MANOVA_random_linear_group_groupP")
Power[1,] <- c(Rayleigh,HR,MANOVA,MANOVA_random,MANOVA_lin,MANOVA_random_lin,
             MANOVA_lin_cov,MANOVA_random_lin_cov,
             MANOVA_lin_group,MANOVA_random_lin_group,
             MANOVA_lin_group_cov,MANOVA_random_lin_group_cov,
             MANOVA_lin_group_cov_group,MANOVA_random_lin_group_cov_group)
Power
}#e
stopCluster(cl) #stop the parallelization

#adding the sample size to the resulting data.frame 
dataaa <- data.frame(nseq,Power_df)

#automatically set the names for the file
text <- paste(Name[i],"_n_",min(nseq),"-",max(nseq),"_its_",rans,"_Dist_",
             plist[1],"_",distt[1], "_m=",min(deg(m1)),"-",max(deg(m1)),"_conc=",min(k1), "-",max(k1),".csv",sep="")

#write the data to the working directory as a csv
write.csv(dataaa, file = text,row.names=FALSE )

}#i #end of out loop

