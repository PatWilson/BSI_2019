#Example SWD (STILL UNDER CONSTRUCTION)
#Code is stripped of filepaths


EFFECT <- 1.2
NSIM <- 500
NADAPT <- 500
NUPDATE <- 1500
NITER <- 2500
THIN <- 1
target_pop_probability <- 0.15

set.seed(2019-02-20)


library(doMC)
library(rjags)
library(tidyverse)
library(lubridate)


registerDoMC()
options(cores=32)


################
## JAGS Model ##
################
poisson_wedge <- "
model {
for(i in 1:N){
y[i] ~ dpois(lambda[i])
log(lambda[i]) <- mu[i]
mu[i] <- beta*treatment[i] + theta.clust[CID[i]] + u[TID[i]] + 1 * logoffset[i]
}

# random effects distributions (note: non-centered)
for (j in 1:Nprim) {
theta.clust[j] ~ dnorm(alpha, tau.clust)
RE.clust[j] <- theta.clust[j]-alpha
}

u[1] ~ dnorm(0, tau.time)
for (t in 2:Ntime){ u[t] ~ dnorm(rho * u[t - 1], tau.time)}


# priors on regression coefficients and variances
tau.clust ~ dgamma(1, 1) # between cluster variance
tau.time ~ dgamma(1, 0.05)  # time series variance

sigma2.clust <- 1/tau.clust
sigma2.time <- 1/tau.time

rho ~ dunif(0,1)  #Some AR(1) parameter 

alpha ~ dnorm (0, 0.0001) # Intercept
beta ~ dnorm (0, 0.0001 ) # Treatment

meanu <- mean(u)
sd.clust <- sqrt(1/tau.clust)
sd.time <- sqrt(1/tau.time)
MU <- meanu+alpha


}"


################
## JAGS Model ##
################
hurdle_wedge <- "
model {
for(i in 1:N){

B[i] ~ dbern(pi[i])
logit(p[i]) <- beta*treatment[i] + theta.clust[CID[i]] + u[TID[i]]
}

# random effects distributions (note: non-centered)
for (j in 1:Nprim) {

theta.clust[j] ~ dnorm(alpha, tau.clust)

RE.clust[j] <- theta.clust[j]-alpha

}

u[1] ~ dnorm(0, tau.time)
for (t in 2:Ntime){ 
u[t] ~ dnorm(rho * u[t - 1], tau.time)
}


for(j in 1:M){
y[j] ~ dlnorm( mu[j] , prec.gamma[j])
mu[j] <- gamma*treatment[j] + gamma.clust[CID[j]] + gamma.u[TID[j]]

}

# random effects distributions (note: non-centered)
for (j in 1:Nprim2) {

gamma.clust[j] ~ dnorm(psi, eta.clust)
}

gamma.u[1] ~ dnorm(0, eta.time)

for (t in 2:Ntime2){ 
u[t] ~ dnorm(kappa * gamma.u[t - 1], eta.time)

}


#Logistic Priors

# priors on regression coefficients and variances
tau.clust ~ dgamma(1, 1) # between cluster variance
tau.time ~ dgamma(1, 0.05)  # time series variance

rho ~ dunif(0,1)  #Some AR(1) parameter 

alpha ~ dnorm (0, 0.0001) # Intercept
beta ~ dnorm (0, 0.0001 ) # Treatment


#Log Normal Priors

# priors on regression coefficients and variances
eta.clust ~ dgamma(1, 1) # between cluster variance
eta.time ~ dgamma(1, 0.05)  # time series variance

kappa ~ dunif(0,1)  #Some AR(1) parameter 

psi ~ dnorm (0, 0.0001) # Intercept
gamma ~ dnorm (0, 0.0001 ) # Treatment


prec.gamma ~ dgamma(1, .05)  #What to put 


}"


####################################################################################
### Bring in Training Data and Model Predictions for Simulation Creation############
####################################################################################



#Create crosswalk and do some renaming
pc_eval_unit_crosswalk <- pc_eval_tdc %>% 
  rename(UNIT.TRANSFER = unit) %>% 
  select(UNIT.TRANSFER, nurse.unit.name) %>% 
  unique()



##Bringing in Prob and determine capacity scenarios
event.encounter.p <- readRDS(file="") 
id <- readRDS(file="") 

prob.max.p <- as.numeric(by(pred.7d.t, id, max))
encounter.pred <- data.frame(encounter_id=unique(id), max.p=prob.max.p, event=event.encounter.p) %>% 
  mutate(encounter_id = as.character(encounter_id))



#Replace with new data
pc_eval_tdc <- readRDS(file="") %>% 
  inner_join(.,pc_eval_unit_crosswalk, by ="UNIT.TRANSFER") %>% 
  rename(encounter_id = id) %>% 
  inner_join(., encounter.pred, by ="encounter_id")




###Create Cluster

scenario_2 <- pc_eval_tdc %>% 
  mutate(
    cluster =  case_when(
      grepl("EAST TOWER", nurse.unit.name) ~ "EAST TOWER",
      grepl("MARY BRIGH 4D", nurse.unit.name) |grepl("MARY BRIGH 4E", nurse.unit.name)  ~ "MARY BRIGH 4 D/E",
      grepl("MARY BRIGH 6B", nurse.unit.name) |grepl("MARY BRIGH 6G", nurse.unit.name)  ~ "MARY BRIGH 6 B/G",
      grepl("MARY BRIGH 5B", nurse.unit.name) |grepl("MARY BRIGH 5G", nurse.unit.name)  ~ "MARY BRIGH 5 B/G"
    )
  ) %>%
  select(nurse.unit.name, cluster) %>% 
  unique() %>% 
  filter(!is.na(cluster)) 


cluster2_id <- scenario_2 %>%
  select(cluster) %>% 
  unique() %>% 
  sample_frac(size = 1)

cluster2_id$id <-seq(1:nrow(cluster2_id))


cluster2 <- cluster2_id %>% 
  inner_join(.,scenario_2, by = "cluster") %>% 
  inner_join(.,pc_eval_tdc, by = "nurse.unit.name")


scenario_3 <- pc_eval_tdc %>% 
  mutate(
    cluster =  case_when(
      grepl("EAST TOWER", nurse.unit.name) ~ "EAST TOWER",
      grepl("MARY BRIGH 4D", nurse.unit.name) |grepl("MARY BRIGH 4E", nurse.unit.name)  ~ "MARY BRIGH 4 D/E",
      grepl("MARY BRIGH 6B", nurse.unit.name) |grepl("MARY BRIGH 6G", nurse.unit.name)  ~ "MARY BRIGH 6 B/G",
      grepl("MARY BRIGH 5B", nurse.unit.name) |grepl("MARY BRIGH 5G", nurse.unit.name)  ~ "MARY BRIGH 5 B/G",
      grepl("UNIT 4-3", nurse.unit.name) ~ "UNIT 4-3",
      grepl("UNIT 4-4 ONCOLOGY", nurse.unit.name) ~ "UNIT 4-4 ONCOLOGY",
      grepl("UNIT 10-3 CRIT. CARE", nurse.unit.name) ~ "UNIT 10-3 CRIT. CARE",
      grepl("UNIT 10-4 SPECIAL", nurse.unit.name) ~ "UNIT 10-4 SPECIAL"
      
    )
  ) %>%
  select(nurse.unit.name, cluster) %>% 
  unique() %>% 
  filter(!is.na(cluster)) 


cluster3_id <- scenario_3 %>%
  select(cluster) %>% 
  unique() %>% 
  sample_frac(size = 1)

cluster3_id$id <-seq(1:nrow(cluster3_id))


cluster3 <- cluster3_id %>% 
  inner_join(.,scenario_3, by = "cluster") %>% 
  inner_join(.,pc_eval_tdc, by = "nurse.unit.name")




scenario_4 <- pc_eval_tdc %>% 
  mutate(
    cluster =  case_when(
      grepl("EAST TOWER", nurse.unit.name) ~ "EAST TOWER",
      grepl("MARY BRIGH 4D", nurse.unit.name) |grepl("MARY BRIGH 4E", nurse.unit.name)  ~ "MARY BRIGH 4 D/E",
      grepl("MARY BRIGH 6B", nurse.unit.name) |grepl("MARY BRIGH 6G", nurse.unit.name)  ~ "MARY BRIGH 6 B/G",
      grepl("MARY BRIGH 5B", nurse.unit.name) |grepl("MARY BRIGH 5G", nurse.unit.name)  ~ "MARY BRIGH 5 B/G",
      grepl("UNIT 4-3", nurse.unit.name) ~ "UNIT 4-3",
      grepl("UNIT 4-4 ONCOLOGY", nurse.unit.name) ~ "UNIT 4-4 ONCOLOGY",
      grepl("UNIT 10-3 CRIT. CARE", nurse.unit.name) ~ "UNIT 10-3 CRIT. CARE",
      grepl("UNIT 10-4 SPECIAL", nurse.unit.name) ~ "UNIT 10-4 SPECIAL",
      grepl("DOMITILLA 2D", nurse.unit.name)  ~ "DOM 2",
      grepl("MARY BRIGH 9D", nurse.unit.name) |grepl("MARY BRIGH 9E", nurse.unit.name)  ~ "MARY BRIGH 9 D/E")
  ) %>%
  select(nurse.unit.name, cluster) %>% 
  unique() %>% 
  filter(!is.na(cluster)) 


cluster4_id <- scenario_4 %>%
  select(cluster) %>% 
  unique() %>% 
  sample_frac(size = 1)

cluster4_id$id <-seq(1:nrow(cluster4_id))


cluster4 <- cluster4_id %>% 
  inner_join(.,scenario_4, by = "cluster") %>% 
  inner_join(.,pc_eval_tdc, by = "nurse.unit.name")

###Wedge Treatment Matrix

#This creates a design matrix
sw.design.mat <-function (I, J, H = NULL) {
  if (sum(sapply(list(I, J, H), is.null)) != 1) {
    warning("exactly one of 'I', 'J' and 'H' must be NULL")
  }
  if (is.null(I)) {
    I <- H * J
  }
  if (is.null(J)) {
    J <- I/H
  }
  if (is.null(H)) {
    H <- I/J
  }
  X <- matrix(0, I, (J + 1))
  for (i in 2:(J + 1)) {
    X[1:((i - 1) * H), i] <- 1
  }
  row.names(X) <- sample(1:I, I)
  colnames(X) <- c("Baseline", paste0("Time ", 1:J))
  return(X)
}




#Create Additional scenarios here
#The idea is to increase the number of clusters until all parties are suitably happy

#Scenario 2
X2 <-sw.design.mat(4,4) %>% 
  as.tibble() %>% 
  rownames_to_column(var = "id") %>% 
  gather(time, treatment, Baseline:`Time 4`) %>% 
  mutate(id = as.numeric(id))

#Scenario 3
X3 <-sw.design.mat(8,4) %>% 
  as.tibble() %>% 
  rownames_to_column(var = "id") %>% 
  gather(time, treatment, Baseline:`Time 4`) %>% 
  mutate(id = as.numeric(id))

#Scenario
X4 <-sw.design.mat(10,4) %>% 
  as.tibble() %>% 
  rownames_to_column(var = "id") %>% 
  gather(time, treatment, Baseline:`Time 4`) %>% 
  mutate(id = as.numeric(id))





##DEFINE study period Scenarios##
#Each study period needs three things:  Start date, length of trial, and number of wedges


#In all scenarios we are using the assumption that we will have about 2 extra months of data for the baseline
#This is due to fact that baseline data can be obtained retrospectively as long as long infrastructure is rdy


#Scenario 1  6 Months  JAN 01, 2017 - JUL 01, 2017  5 wedges 36 days each
#Scenario 2  9 Months  JAN 01, 2017 - OCT 01, 2017  5 wedges 55 days each 
#Scenario 3  12 Months JAN 01, 2017 - DEC 31, 2017  5 wedges 73 days each 

time_cuts <- function(days, num_days, num_wedges, cluster, wedge_matrix) {
  
  tm <- seq(as.POSIXct("2017-01-01 00:00:00", tz ="UTC"), by = days, length.out = num_wedges + 1)
  
  sim_data  <- cluster %>% 
    mutate(global_time = as.numeric(as.factor(round_date(admit.time + days(DAYCOV), "day")))) %>% 
    mutate(exposure =  (tstop -tstart)/24) %>% 
    arrange(ClinicNumber, admit.time, DAYCOV) %>% 
    filter(admit.time <= (tm[6])) %>%  #Time range for whole study 
    mutate(
      time = case_when(
        admit.time >= tm[5] ~"Time 4",
        admit.time >= tm[4] ~"Time 3",
        admit.time >= tm[3] ~"Time 2",
        admit.time >= tm[2] ~"Time 1",
        admit.time >= tm[1] ~"Baseline"
      )
    ) %>% 
    filter(global_time <= (interval(tm[1], tm[6]) %/% days(1))) %>%   #Censor global times
    inner_join(., wedge_matrix, by = c("id", "time")) 
  
  
  return(sim_data)
}


sim_2_6m <- time_cuts("36 days", 36, 5, cluster2, X2)
sim_2_9m <- time_cuts("55 days", 55, 5, cluster2, X2)
sim_2_12m <- time_cuts("73 days", 73, 5, cluster2, X2)



sim_3_6m <- time_cuts("36 days", 36, 5, cluster3, X3)
sim_3_9m <- time_cuts("55 days", 55, 5, cluster3,  X3)
sim_3_12m <- time_cuts("73 days", 73, 5, cluster3,  X3)


sim_4_6m <- time_cuts("36 days", 36, 5, cluster4,  X4)
sim_4_9m <- time_cuts("55 days", 55, 5, cluster4,  X4)
sim_4_12m <- time_cuts("73 days", 73, 5, cluster4,  X4)


#################################################################################
### Determining Capacity for treatment effect. (Function in progress)############
#################################################################################

if(0){
  
  
  #Twenty percent sample
  #sample20 <- sim_data %>%  
  #  select(ClinicNumber) %>%
  #  unique() %>% 
  #  sample_frac(size = 0.2)
  
  #sim_data20 <- sample20 %>%
  #  left_join(., sim_data, by ="ClinicNumber")
  
  
  # Test cutoffs
  
  table(encounter.pred$max.p>0.18, encounter.pred$event)/365.25
  table(I(encounter.pred$max.p>0.2), encounter.pred$event)/365.25
  table(I(encounter.pred$max.p>0.3),encounter.pred$event)/365.25
  table(I(encounter.pred$max.p>0.5),encounter.pred$event)/365.25
  table(I(encounter.pred$max.p>0.75),encounter.pred$event)/365.25
  
  #Need to define cutoffs for specific scenarios
  capacity <- sim_4_12m %>% 
    group_by(encounter_id) %>% 
    slice(1) %>% 
    ungroup()
  
  table(capacity$max.p>=0.18, capacity$event)/365.25  
  table(capacity$max.p>=0.15, capacity$event)/365.25
  table(capacity$max.p>=0.13, capacity$event)/365.25
  
}


##########################################
### Filter based on probability###########
##########################################

filter_prob <- function(data){
  prob_filter <- data %>% 
    filter(max.p >=target_pop_probability) 
  return(prob_filter)
}

sim_2_6m_prob <- filter_prob(sim_2_6m)
sim_2_9m_prob <- filter_prob(sim_2_9m)
sim_2_12m_prob<- filter_prob(sim_2_12m)



sim_3_6m_prob <- filter_prob(sim_3_6m)
sim_3_9m_prob <- filter_prob(sim_3_9m)
sim_3_12m_prob <- filter_prob(sim_3_12m)


sim_4_6m_prob <- filter_prob(sim_4_6m)
sim_4_9m_prob <- filter_prob(sim_4_9m)
sim_4_12m_prob <- filter_prob(sim_4_12m)


##########################################################
### Estimate Parameters (Function in progress)############
##########################################################

#Cluster Rate estimation Use only 12 months
rate_maker <- function(data){
  rate <- data %>% 
    select(ClinicNumber, admit.time, pc, cluster) %>%
    arrange(ClinicNumber,admit.time,pc) %>% 
    group_by(ClinicNumber, admit.time) %>% 
    slice(n()) %>% 
    ungroup() %>%
    group_by(cluster) %>% 
    summarise(cluster_rate =mean(pc)) 
  return(rate)
}


cluster_rate2<- rate_maker(sim_2_12m_prob)
cluster_rate3<- rate_maker(sim_3_12m_prob)
cluster_rate4<- rate_maker(sim_4_12m_prob)

### Obtain empirical estimates for sigma^2 and rho of time variation process. 


estimate_time <- function(simdata){
  
  run.jags.par <- function(model.str, data, n.chains, n.adapt, n.burn, n.iter, thin, inits, variable.names){
    
    samples.list <- foreach(l=1:n.chains)%dopar%{
      init0 <- inits[[l]]
      init0$.RNG.name <- "base::Wichmann-Hill"
      init0$.RNG.seed <- 100+l
      bc.model <- jags.model(textConnection(model.str), data=data, n.chains=1, n.adapt=n.adapt, inits=init0)
      update(bc.model, n.burn)
      bc.samples <- coda.samples(bc.model, variable.names=variable.names, n.iter=n.iter, thin=thin)
    }
    all.samples <- mcmc.list()
    for(l in 1:n.chains)
      all.samples[[l]] <- samples.list[[l]][[1]]  
    return(all.samples)
  }
  
  
  ### Initialize the model ###
  wedge.model <- run.jags.par(poisson_wedge, data=list(y=as.numeric(simdata$pc), N=nrow(simdata),treatment=rep(0,nrow(simdata)),  CID = simdata$id, TID = simdata$global_time , 
                                                       logoffset = log(simdata$exposure), Nprim =max(simdata$id), Ntime =max(simdata$global_time)), 
                              n.chains=1, n.adapt=250, n.burn=250, n.iter=2000, thin=1, inits=NULL, variable.names=c("rho", "sd.time"))
  return(wedge.model)
}


time_sim2 <-estimate_time(sim_2_12m_prob)
time_sim3 <-estimate_time(sim_3_12m_prob)
time_sim4 <-estimate_time(sim_4_12m_prob)

#Create estimates of Rho and Sigma.e 

time_sim2_sum <- summary(time_sim2)
RHO.HAT2 <- time_sim2_sum$statistics["rho","Mean"]
SIGMA.E.HAT2 <- time_sim2_sum$statistics["sd.time","Mean"]

time_sim3_sum <- summary(time_sim3)
RHO.HAT3 <- time_sim3_sum$statistics["rho","Mean"]
SIGMA.E.HAT3 <- time_sim3_sum$statistics["sd.time","Mean"]

time_sim4_sum <- summary(time_sim4)
RHO.HAT4 <- time_sim4_sum$statistics["rho","Mean"]
SIGMA.E.HAT4 <- time_sim4_sum$statistics["sd.time","Mean"]  

#Generate time series effect on predictor scale
generate.expu <- function(data, RHO.HAT, SIGMA.E.HAT){
  T <- max(data$global_time)
  u <- rep(0,T)
  sig.e <- SIGMA.E.HAT
  rho <- RHO.HAT
  sig <- sig.e/sqrt(1-rho^2)
  u[1] <- rnorm(1,0,sig)
  for(i in 2:T)
    u[i] <- rnorm(1, rho*u[i-1], sig.e) 
  return(exp(u))
}

#Bring data together and estimate lambda

lambda_maker <- function(data, rate_cluster, RHO.HAT, SIGMA.E.HAT){
  
  expu <- generate.expu(data, RHO.HAT, SIGMA.E.HAT) %>% 
    as.data.frame()
  colnames(expu)[1] <-"expu"
  expu <-mutate(expu, gt_id = row_number())
  
  
  data_hat <- data %>% 
    left_join(., rate_cluster, by="cluster") %>%
    left_join(., expu, by =c("global_time"="gt_id")) %>%
    
    #Join to Treatment specification
    mutate(lambda = if_else(treatment==1, 
                            (EFFECT*cluster_rate*expu),
                            cluster_rate*expu)) 
  return(data_hat)
}


sim_2_6m_data <- lambda_maker(sim_2_6m_prob, cluster_rate2, RHO.HAT2, SIGMA.E.HAT2)
sim_2_9m_data <- lambda_maker(sim_2_9m_prob, cluster_rate2, RHO.HAT2, SIGMA.E.HAT2)
sim_2_12m_data<- lambda_maker(sim_2_12m_prob, cluster_rate2, RHO.HAT2, SIGMA.E.HAT2)


sim_3_6m_data <- lambda_maker(sim_3_6m_prob, cluster_rate3, RHO.HAT3, SIGMA.E.HAT3)
sim_3_9m_data <- lambda_maker(sim_3_9m_prob, cluster_rate3, RHO.HAT3, SIGMA.E.HAT3)
sim_3_12m_data <- lambda_maker(sim_3_12m_prob, cluster_rate3, RHO.HAT3, SIGMA.E.HAT3)


sim_4_6m_data <- lambda_maker(sim_4_6m_prob, cluster_rate4, RHO.HAT4, SIGMA.E.HAT4)
sim_4_9m_data <- lambda_maker(sim_4_9m_prob, cluster_rate4, RHO.HAT4, SIGMA.E.HAT4)
sim_4_12m_data <- lambda_maker(sim_4_12m_prob, cluster_rate4, RHO.HAT4, SIGMA.E.HAT4)



#################################################################################
### Validation of Plasmode simulations. (Function in progress)###################
#################################################################################

if(0){
  
  test_sim <- function(data) {  
    #Lets see the numbers
    
    print("2by2")
    print(table(data$cluster, data$time))
    
    # aggregate
    
    print("Did time work as expected?")
    
    print("Min Time")
    print(aggregate(admit.time ~ time, data = data, min)) 
    
    
    print("Max Time")
    print(aggregate(discharge.time ~ time, data = data, max))
    
    
    
    print("Funky Clusters")
    
    print("Min Time")
    print(aggregate(admit.time ~ time + cluster, data = data, min)) 
    
    
    print("Max Time")
    print(aggregate(admit.time ~ time + cluster, data = data, max))
    
    
    #Did your rates turn out the way you wanted
  }
  
  
  test_sim(sim_2_6m)
  
}



#START THE SIMULATION#
#START THE SIMULATION#
#START THE SIMULATION#
#START THE SIMULATION#


#Currently there are three time scenarios and three cluster scenarios, yielding nine scenarios
sim_wedge <- function(data, expu_data){
  
  foreach(i=1:NSIM)%dopar%{
    #Get simulated values
    
    sim_value <-  data %>%
      rowwise() %>%
      mutate(y = rexp(1, rate = lambda)) %>%
      ungroup() %>% 
      mutate(event = case_when(
        exposure > y ~"1",
        TRUE ~ "0"
      ),
      expose_new = case_when(
        event == 1 ~ y,
        TRUE ~ exposure
      )
      )
    
    #Delete extra rows
    #Time to event simulations require this step 
    sim_event  <- sim_value %>% 
      filter(event ==1) %>% 
      group_by(ClinicNumber, admit.time) %>% 
      slice(1) %>% 
      ungroup %>% 
      mutate(global_event_time = global_time) %>% 
      select(ClinicNumber, admit.time, global_event_time)
    
    sim_final <- sim_value %>% 
      left_join(., sim_event, by =c("ClinicNumber","admit.time")) %>% 
      filter(global_time <= global_event_time | is.na(global_event_time))
    
    ### Initialize the model ###
    wedge.model <- jags.model(textConnection(poisson_wedge), data=list(y=as.numeric(sim_final$event), N=nrow(sim_final),treatment = sim_final$treatment,  CID = sim_final$id, 
                                                                       TID = sim_final$global_time , logoffset = log(sim_final$expose_new), Nprim =max(sim_final$id), 
                                                                       Ntime =max(sim_final$global_time)), n.adapt=NADAPT, inits=NULL)
    
    ### Burn in iterations (these will be discarded) ###
    update(wedge.model, NUPDATE); # Burnin for N.burn samples
    
    ### Collect a sample from the posterior distribution (after burn in) ###
    
    coda.samples(wedge.model, variable.names=c("alpha","beta", "rho"), n.iter=NITER, thin=THIN)
    
  }
}

#RUN SIMS##RUN SIMS##RUN SIMS##RUN SIMS##RUN SIMS#
#RUN SIMS##RUN SIMS##RUN SIMS##RUN SIMS##RUN SIMS#
#RUN SIMS##RUN SIMS##RUN SIMS##RUN SIMS##RUN SIMS#

pvalue_scenario1 <- sim_wedge(sim_2_6m_data)
pvalue_scenario2 <- sim_wedge(sim_2_9m_data)
pvalue_scenario3 <- sim_wedge(sim_2_12m_data)

pvalue_scenario4 <- sim_wedge(sim_3_6m_data)
pvalue_scenario5 <- sim_wedge(sim_3_9m_data)
pvalue_scenario6 <- sim_wedge(sim_3_12m_data)

pvalue_scenario7 <- sim_wedge(sim_4_6m_data)
pvalue_scenario8 <- sim_wedge(sim_4_9m_data)
pvalue_scenario9 <- sim_wedge(sim_4_12m_data)



#Save all monte carlo simulations and time simulations

fname <- paste0("simulation_multisite_EFFECT_",format(EFFECT,nsmall=2),".RData")
save(pvalue_scenario1, pvalue_scenario2, pvalue_scenario3,  
     pvalue_scenario4, pvalue_scenario5, pvalue_scenario6,
     pvalue_scenario7, pvalue_scenario8, pvalue_scenario9, 
     time_sim2, time_sim3, time_sim4, 
     file=fname)
