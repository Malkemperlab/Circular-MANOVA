
#Script to test statistical power of the MANOVA approach in standard situations (compared to traditional tests)

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

#setting sample size for distributions and combine in a list for looping
nvseq_1 <- c(5,10,15,25,50,100)
nvseq_2 <- c(10,20,30,50,100,200)
nvseq_3 <- c(15,30,45,75,150,300)
n_list_1 <- list(nvseq_1,nvseq_1,nvseq_1,nvseq_2,nvseq_2,nvseq_3,nvseq_3) 

#defining kappa/concentration values
k0 <- 0; k2 <- 1; kwsn <- 2

#preparing the kappa/conc sequences for looping
k_matrix_1 <- c(k0,k2,kwsn,k2,k2,k2,k2)

#preparing the mean direction sequence (all toward zero) for looping
m0 <- 0; m_matrix_1 <- c(m0,m0,m0,m0,m0,m0,m0)

#defining the distribution types
#therefore we prepared distribution type characters and then combined them. The code is looping through them.
type_vm <- "vm"; type_unif <- "unif"; type_vm_2 <- c("vm","vm"); type_vm_3 <- c("vm","vm","vm"); type_wsn <- "wsn"
type_list_1 <- list(type_unif,type_vm,type_wsn,type_vm_2,type_vm_2,type_vm_3,type_vm_3)  

##### preparing a vector to call the correct "param" argument in the rcircmix function 
param_matrix_1 <- c("param_unif","param_vm","param_wsn","param_axial","param_bim","param_trim_sym","param_trim_asym")  

#preparing the names for the files - which will be added automatically at the end of the script
Type_1_errorn <- c("Type_1_error"); Powern=c("Power")
Name <- c(Type_1_errorn,Powern,Powern,Powern,Powern,Powern,Powern)


###########
#each outer loop is stored in one csv file, this means its one panel in the plot. 
for (i in 1:length(Name)) {
  
  #grabbing the correct numbers for the simulation from the lists prepared above
  nseq <- n_list_1[[i]]
  k1 <- k_matrix_1[i]
  m1 <- m_matrix_1[i]
  distt <- type_list_1[[i]]
  plist <- param_matrix_1[i]

###### Prepare environment for the foreach loop (setting cores and parallel processing)
ncore<-detectCores()
cl <- parallel::makeCluster(ncore-1, setup_strategy = "sequential")#first number changes the cores used
doParallel::registerDoParallel(cl)

#starting the foreach loop, results will be 'rbound' 
Power_df <- foreach(e=1:length(nseq), .combine='rbind',.packages=c("circular","NPCirc","CircMLE")) %dopar%{
  
  n <- nseq[e]
  
  #set up the functions for the different params 
  param_vm <- function(x) {list(p=1,  mu=m1, con=k1)}
  param_wsn <- function(x) {list(p=1,  mu=m1, con=k1,sk=c(30))}
  param_axial <- function(x) {list(p=c(0.5,0.5), mu=c(m1,m1-pi), con=c(k1,k1))}
  param_bim <- function(x) {list(p=c(0.5,0.5), mu=c(m1,m1-rad(120)), con=c(k1,k1))}
  param_trim_sym <- function(x) {list(p=c((1/3),(1/3),(1/3)), mu=c(m1,m1-rad(120),m1+rad(120)), con=c(k1,k1,k1))}
  param_trim_asym <- function(x) {list(p=c((1/3),(1/3),(1/3)), mu=c(m1,m1-rad(90),m1+rad(120)), con=c(k1,k1,k1))}
  param_unif <- function(x) {list(p=1,  mu=m1[1], con=k1[1])}
  
  
  
  ###### set the sample distribution (rans times)
  Distribution <- rad(round(deg(sapply(1:rans, function(x) rcircmix(n,dist = distt,param=get(plist)()))),digits=-1))
  
  #generate the random sample (rans times)
  random_sample<- sapply(1:rans, function(x)rcircmix(n,model = 1))
  
  #applying the HR Test
  
  #calculating T-value for random sample 
  random_T<-apply(random_sample,2,function(x) CircMLE:::HermansRasson2T(x)) #here change the fucntion 
  
  #calculating T-value for sample of interest
  HR.res.T<-apply(Distribution,2,function(x) CircMLE:::HermansRasson2T(x)) # here change the function
  
  #calculating the p-value (rans times)
  HR.res.p<-vapply(1:rans,function(x) sum(1*(HR.res.T[x])< random_T)/rans, FUN.VALUE = numeric(1))
  
  #applying the MANOVA approach (intercept)
  funManCirc<-function(x) summary(manova(cbind(cos(x),sin(x)) ~ 1),intercept=T)
  MANOVA_approach <- apply(Distribution,2, FUN=funManCirc)
  MANOVA_P <- sapply(c(1:rans),function(x) MANOVA_approach[[x]][["stats"]][1,6])
  
  #applying the MANOVA approach (intercept) calulating P-value by randomization
  random_MANOVA<-apply(random_sample,2,function(x) funManCirc(x))
  random_MANOVA_F <- sapply(c(1:rans),function(x) random_MANOVA[[x]][["stats"]][1,3])
  MANOVA_F <- sapply(c(1:rans),function(x) MANOVA_approach[[x]][["stats"]][1,3])
  random_MANOVA_P<-vapply(1:rans,function(x) sum(1*(MANOVA_F[x]< random_MANOVA_F))/rans, FUN.VALUE = numeric(1))
  
  #applying the Rayleigh test on the samples 
  Rayleigh <- (apply(Distribution,2,rayleigh.test))
  Ray <- array(as.matrix(unlist(Rayleigh)), dim=c(4, rans))
  RayP<- Ray[2,]
  
  #####Calculate Power for each of the approaches   
  MANOVA<- sum(1*(MANOVA_P < 0.05))/rans
  Rayleigh<- sum(1*(RayP< 0.05))/rans
  HR<- sum(1*(HR.res.p< 0.05))/rans
  MANOVA_random<- sum(1*(random_MANOVA_P < 0.05))/rans
  
  #prepare data.frame to store the power results in and then add the data 
  Power<- data.frame(matrix(nrow = 1, ncol = 4))
  names(Power)<-c("MANOVA", "Rayleigh","HR","MANOVA_random")
  Power[1,]<-c(MANOVA,Rayleigh,HR,MANOVA_random)
  Power

} #e
stopCluster(cl) #stop the parallelization

#adding the sample size to the resulting data.frame 
dataaa <- data.frame(nseq,Power_df)

#adding the automatoc names 
text <- paste(Name[i],"_n_",min(nseq),"-",max(nseq),"_its_",rans,"_Dist_",
             plist,"_",distt[1], "_m=",deg(m1), "_conc=",k1,"BINNED",".csv",sep="")

write.csv(dataaa, file = text,row.names=FALSE )

}#i #end of out loop
