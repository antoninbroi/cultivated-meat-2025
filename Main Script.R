
#Prepare working environment
rm(list = ls())
set.seed(1234)
options(scipen=999)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

parallel_dummy=TRUE

#Libraries
library(purrr)
library(dplyr)
if(parallel_dummy) library(pbmcapply)

#Set seed
set.seed(123)

#Import files
source("Data.R")
source("Functions.R")
source("Util functions.R")
source("Functions Simulations.R")

#Set parameters of interest
S=10000 #Number of simulations

### Results ###                          

percentage_integration = 0.1              
options(pillar.sigfig = 10)  # Increase significant digits when visualizing data

## SCENARIO 0 ## 
# Utilitarian total approach                    
# Animal welfare in uniform integration scenarios
results_uniform_utilitarian = computeAQALYs(animalsData) 
results_uniform_utilitarian

##  SCENARIO 1 ##                             
# Animal welfare in differentiated integration scenarios
# Premium substitution, percentage_bovine = 0.8                    

animalsData_scenario1=data_with_differentiated_animals(animalsData, percentage_integration = 0.1, percentage_bovine = 0.8)

results_differentiated_premium_utilitarian = computeAQALYs(animalsData_scenario1) 
results_differentiated_premium_utilitarian

##  SCENARIO 2 ##
# Low-cost substitution, percentage_bovine = 0.2                    
animalsData_scenario2=data_with_differentiated_animals(animalsData, percentage_integration = 0.1, percentage_bovine = 0.2)

results_differentiated_lowcost_utilitarian = computeAQALYs(animalsData_scenario2)  
results_differentiated_lowcost_utilitarian

##  SCENARIO 3 ##
# Killing-adjusted total utilitarian approach                    
# Definition of the killing penalty for the different categories of animals
# Animal welfare in uniform integration scenarios
animalsData_scenario3=data_with_killing_penalty(animalsData, psi_funct=1, killing_penalty_in_days=365.25)

results_uniform_killing_adjusted = computeAQALYs(animalsData_scenario3, killing_penalty=TRUE)  
results_uniform_killing_adjusted

##  SCENARIO 4 ##
# Animal welfare in differentiated integration scenarios
# Premium substitution, percentage_bovine = 0.8                    
animalsData_scenario4=data_with_differentiated_animals(animalsData, percentage_integration = 0.1, percentage_bovine = 0.8)
animalsData_scenario4=data_with_killing_penalty(animalsData_scenario4, psi_funct=1, killing_penalty_in_days=365.25)

results_differentiated_premium_killing_adjusted=computeAQALYs(animalsData_scenario4, killing_penalty=TRUE)  
results_differentiated_premium_killing_adjusted


##  SCENARIO 5 ##
# Low-cost substitution, percentage_bovine = 0.2                    
animalsData_scenario5=data_with_differentiated_animals(animalsData, percentage_integration = 0.1, percentage_bovine = 0.2)
animalsData_scenario5=data_with_killing_penalty(animalsData_scenario5, psi_funct=1, killing_penalty_in_days=365.25)

results_differentiated_lowcost_killing_adjusted=computeAQALYs(animalsData_scenario5, killing_penalty=TRUE)  
results_differentiated_lowcost_killing_adjusted


### Sensitivity analysis ###

# We want to assess how our main results – total net (killing-adjusted) AQALY in the six different scenarios – vary if we modify some of our assumptions

# Calculation of individual welfare
#1) Vary W0 
#2) Vary minimal negative score (called delta)
#On retire l'hypothèse de linéarité : on autorise toute fonction concave/convexe 
#Qui respecte ces principes : 
#First, when there is no violation, the welfare score should be equal to 1 (Wt = 0 : q(t) = 1). 
#Second, when there are W0 violation points, the score must be equal to 0 (Wt = W0 : q(t) = 0). 
#Third, the welfare score must decrease in the number of violation points (∀Wt: 2aWt+b < 0). 
#Last, we impose a minimal value for q(.), which we note δ (with Wt = 20 : q(t) = δ). 

# Value of the killing penalty

# First set up the scenario (moderate/severe, utilitarian/killing-adjusted, uniform/premium meat/lowcost), then run a series of simulations, which of them yield results for each scenario: in each simulation, choose the parameters, and then compute the net welfare for every scenario and category and then sum them up. 

#Robustness check wrt to q:
if(!parallel_dummy){
  simRes_q=lapply(1:S, function(i){
    set.seed(i)
    #message(paste0(i/S*100, "%"))
    progress_printer(i, S)
    #PUT HERE WHAT CHANGES
    modelW0_loop=randomModelW0()
    jointDraw_loop=joint_W0_delta(modelW0_loop)
    W0_loop=jointDraw_loop$W0
    delta_loop=jointDraw_loop$delta
    #Compute estimates
    res_all_loop=sim_all_estimates(i_funct=i,jointDraw_funct=jointDraw_loop)
    return(res_all_loop)
  })
  simRes_q_DF=do.call(rbind.data.frame, simRes_q)
}

if(parallel_dummy){
  simRes_q=pbmclapply(1:S,function(i){
    set.seed(i)
    progress_printer(i, S)
    modelW0_loop=randomModelW0()
    jointDraw_loop=joint_W0_delta(modelW0_loop)
    W0_loop=jointDraw_loop$W0
    delta_loop=jointDraw_loop$delta
    #Compute estimates
    res_all_loop=sim_all_estimates(i_funct=i,jointDraw_funct=jointDraw_loop)
    return(res_all_loop)
  },  mc.cores = min(detectCores()-1,20))
  simRes_q_DF=do.call(rbind.data.frame, simRes_q)
}

head(simRes_q_DF)

simRes_q_DF %>% 
  filter(category == "Total") %>%  
  select(scenario, category, AQALY_moderate) %>% 
  group_by(scenario) %>%  
  summarise(lb=quantile(AQALY_moderate,c(0.025), na.rm = TRUE),
            ub=quantile(AQALY_moderate,c(0.975), na.rm = TRUE),
            median=quantile(AQALY_moderate,c(0.5), na.rm = TRUE),
            average=mean(AQALY_moderate, na.rm = TRUE))

simRes_q_DF %>% 
  filter(category == "Total") %>%  
  select(scenario, category, AQALY_severe) %>% 
  group_by(scenario) %>%  
  summarise(lb=quantile(AQALY_severe,c(0.025), na.rm = TRUE),
            ub=quantile(AQALY_severe,c(0.975), na.rm = TRUE),
            median=quantile(AQALY_severe,c(0.5), na.rm = TRUE),
            average=mean(AQALY_severe, na.rm = TRUE))

#Robustness check wrt to psi:
if(!parallel_dummy){
  simRes_psi=lapply(1:S, function(i){
    set.seed(i)
    message(paste0(i/S*100, "%"))
    psi_loop=randomPsi()
    res_all_loop=sim_all_estimates(i_funct=i,psi_funct=psi_loop)
    return(res_all_loop)
  
  })
  simRes_psi_DF=do.call(rbind.data.frame, simRes_psi)
}

if(parallel_dummy){
  simRes_psi=pbmclapply(1:S,function(i){
    set.seed(i)
    psi_loop=randomPsi()
    res_all_loop=sim_all_estimates(i_funct=i,psi_funct=psi_loop)
    return(res_all_loop)
  },  mc.cores = min(detectCores()-1,20))
  simRes_psi_DF=do.call(rbind.data.frame, simRes_psi)
}

head(simRes_psi_DF)

simRes_psi_DF %>% 
  filter(category == "Total") %>%  
  select(scenario, category, AQALY_moderate) %>% 
  group_by(scenario) %>%  
  summarise(lb=quantile(AQALY_moderate,c(0.025), na.rm = TRUE),
            ub=quantile(AQALY_moderate,c(0.975), na.rm = TRUE),
            median=quantile(AQALY_moderate,c(0.5), na.rm = TRUE),
            average=mean(AQALY_moderate, na.rm = TRUE))

simRes_psi_DF %>% 
  filter(category == "Total") %>%  
  select(scenario, category, AQALY_severe) %>% 
  group_by(scenario) %>%  
  summarise(lb=quantile(AQALY_severe,c(0.025), na.rm = TRUE),
            ub=quantile(AQALY_severe,c(0.975), na.rm = TRUE),
            median=quantile(AQALY_severe,c(0.5), na.rm = TRUE),
            average=mean(AQALY_severe, na.rm = TRUE))

#Robustness check wrt to killing:
if(!parallel_dummy){
  simRes_kp=lapply(1:S, function(i){
    set.seed(i)
    message(paste0(i/S*100, "%"))
    #PUT HERE WHAT CHANGES
    kp_loop=randomKillingpenalty()
    
    #Compute estimates
    res_all_loop=sim_all_estimates(i_funct=i,kp_funct=kp_loop)
    return(res_all_loop)
    
  })
  simRes_kp_DF=do.call(rbind.data.frame, simRes_kp)
}

if(parallel_dummy){
  simRes_kp=pbmclapply(1:S,function(i){
    set.seed(i)
    kp_loop=randomKillingpenalty()
    res_all_loop=sim_all_estimates(i_funct=i,kp_funct=kp_loop)
    return(res_all_loop)
  },  mc.cores = min(detectCores()-1,20))
  simRes_kp_DF=do.call(rbind.data.frame, simRes_kp)
}

head(simRes_kp_DF)

simRes_kp_DF %>% 
  filter(category == "Total") %>%  
  select(scenario, category, AQALY_moderate) %>% 
  group_by(scenario) %>%  
  summarise(lb=quantile(AQALY_moderate,c(0.025), na.rm = TRUE),
            ub=quantile(AQALY_moderate,c(0.975), na.rm = TRUE),
            median=quantile(AQALY_moderate,c(0.5), na.rm = TRUE),
            average=mean(AQALY_moderate, na.rm = TRUE))

simRes_kp_DF %>% 
  filter(category == "Total") %>%  
  select(scenario, category, AQALY_severe) %>% 
  group_by(scenario) %>%  
  summarise(lb=quantile(AQALY_severe,c(0.025), na.rm = TRUE),
            ub=quantile(AQALY_severe,c(0.975), na.rm = TRUE),
            median=quantile(AQALY_severe,c(0.5), na.rm = TRUE),
            average=mean(AQALY_severe, na.rm = TRUE))



#Robustness check wrt to q, psi, and killing:
if(!parallel_dummy){
  simRes_all=lapply(1:S, function(i){
    set.seed(i)
    message(paste0(i/S*100, "%"))
    modelW0_loop=randomModelW0()
    jointDraw_loop=joint_W0_delta(modelW0_loop)
    W0_loop=jointDraw_loop$W0
    delta_loop=jointDraw_loop$delta
    psi_loop=randomPsi()
    kp_loop=randomKillingpenalty()
    #Compute estimates
    res_all_loop=sim_all_estimates(i_funct=i,jointDraw_funct=jointDraw_loop,psi_funct=psi_loop,kp_funct=kp_loop)
    return(res_all_loop)
  })
  simRes_all_DF=do.call(rbind.data.frame, simRes_all)
}

if(parallel_dummy){
  simRes_all=pbmclapply(1:S,function(i){
    set.seed(i)
    modelW0_loop=randomModelW0()
    jointDraw_loop=joint_W0_delta(modelW0_loop)
    W0_loop=jointDraw_loop$W0
    delta_loop=jointDraw_loop$delta
    psi_loop=randomPsi()
    kp_loop=randomKillingpenalty()
    #Compute estimates
    res_all_loop=sim_all_estimates(i_funct=i,jointDraw_funct=jointDraw_loop,psi_funct=psi_loop,kp_funct=kp_loop)
    return(res_all_loop)
  },  mc.cores = min(detectCores()-1,20))
  simRes_all_DF=do.call(rbind.data.frame, simRes_all)
}

head(simRes_all_DF)

simRes_all_DF %>% 
  filter(category == "Total") %>%  
  select(scenario, category, AQALY_moderate) %>% 
  group_by(scenario) %>%  
  summarise(lb=quantile(AQALY_moderate,c(0.025), na.rm = TRUE),
            ub=quantile(AQALY_moderate,c(0.975), na.rm = TRUE),
            median=quantile(AQALY_moderate,c(0.5), na.rm = TRUE),
            average=mean(AQALY_moderate, na.rm = TRUE),       
            share_benefit=mean(AQALY_moderate < 0, na.rm = TRUE))

simRes_all_DF %>% 
  filter(category == "Total") %>%  
  select(scenario, category, AQALY_severe) %>% 
  group_by(scenario) %>%  
  summarise(lb=quantile(AQALY_severe,c(0.025), na.rm = TRUE),
            ub=quantile(AQALY_severe,c(0.975), na.rm = TRUE),
            median=quantile(AQALY_severe,c(0.5), na.rm = TRUE),
            average=mean(AQALY_severe, na.rm = TRUE),
            share_benefit=mean(AQALY_severe < 0, na.rm = TRUE))


