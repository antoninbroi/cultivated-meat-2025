#Function returning new dataset with updated killing penalty
data_with_killing_penalty <- function(animalsData_funct, psi_funct=1, killing_penalty_in_days=365.25) {
  newAnimalsData_funct <- lapply(animalsData_funct, function(animal) {
    animal$killing_penalty <- killing_penalty_in_days*((animal$neurons/humanNeurons)^psi_funct)#^2 # we take one year of full welfare, so that's 1 x 1
    return(animal)
  })
  return(newAnimalsData_funct)
}

# Function to calculate the number of animals in each group for the moderate and severe intensification scenarios.
data_with_differentiated_animals <- function(animalsData_funct, 
                                             percentage_integration, 
                                             percentage_bovine) {
                                            #percentage_bovine: percentage of allocation of conventional meat tonnage reduction to beef (with the rest among pork and chicken following their respective contribution to tonnage production).    
  
  
  # Meat tonnage
  meat_chicken <- (1 - percentage_bovine) * chicken_share * total_meat_tonnes
  meat_pork <- (1 - percentage_bovine) * pork_share * total_meat_tonnes
  meat_bovine <- percentage_bovine * total_meat_tonnes
  
  # Animal numbers
  nbr_chicken <- meat_chicken * chicken_nbr / chicken_meat_tonnes
  nbr_pork <- meat_pork * pork_nbr / pork_meat_tonnes
  nbr_bovine <- meat_bovine * bovine_nbr / bovine_meat_tonnes
  
  # MODERATE scenario shares
  chicken_intensive_mod <- 0.859
  pork_intensive_mod <- 0.595

  # SEVERE scenario shares
  chicken_intensive_sev <- 0.999
  pork_intensive_sev <- 0.986
  dairy_intensive_sev <- 0.749
  beef_intensive_sev <- 0.749
  dairy_extensive_sev <- dairy_bovine_nbr * (1 - dairy_intensive_sev)
  beef_extensive_sev <- beef_bovine_nbr * (1 - beef_intensive_sev)
  
  # MODERATE assignments
  animalsData_tmp = animalsData_funct
  animalsData_tmp$chickenIntensive$nbr_moderate = nbr_chicken * chicken_intensive_mod
  animalsData_tmp$chickenExtensive$nbr_moderate = nbr_chicken * (1 - chicken_intensive_mod)
  animalsData_tmp$porkIntensive$nbr_moderate = nbr_pork * pork_intensive_mod
  animalsData_tmp$porkExtensive$nbr_moderate = nbr_pork * (1 - pork_intensive_mod)
  animalsData_tmp$dairyHerdIntensive$nbr_moderate = 0
  animalsData_tmp$dairyHerdExtensive$nbr_moderate = nbr_bovine * dairy_bovine_nbr / bovine_nbr
  animalsData_tmp$beefHerdIntensive$nbr_moderate = 0
  animalsData_tmp$beefHerdExtensive$nbr_moderate = nbr_bovine * beef_bovine_nbr / bovine_nbr
  
  # SEVERE assignments
  animalsData_tmp$chickenIntensive$nbr_severe = nbr_chicken * chicken_intensive_sev
  animalsData_tmp$chickenExtensive$nbr_severe = nbr_chicken * (1 - chicken_intensive_sev)
  animalsData_tmp$porkIntensive$nbr_severe = nbr_pork * pork_intensive_sev
  animalsData_tmp$porkExtensive$nbr_severe = nbr_pork * (1 - pork_intensive_sev)
  animalsData_tmp$dairyHerdIntensive$nbr_severe = nbr_bovine * (dairy_intensive_sev * dairy_bovine_nbr) / bovine_nbr
  animalsData_tmp$dairyHerdExtensive$nbr_severe = nbr_bovine * dairy_extensive_sev / bovine_nbr
  animalsData_tmp$beefHerdIntensive$nbr_severe = nbr_bovine * (beef_intensive_sev * beef_bovine_nbr) / bovine_nbr
  animalsData_tmp$beefHerdExtensive$nbr_severe = nbr_bovine * beef_extensive_sev / bovine_nbr
  return(animalsData_tmp)
}


#Random Model W0:
#Select one of the two samples of the online survey: either W0 when welfare becomes negative OR when life becomes not worth living
randomModelW0=function(){
  sample(c("Negative Welfare","Life Not Worth Living"), 
         prob = c(0.5,0.5),
         size = 1)
}


#Random W0: Randomly select a W0
randomW0=function(modelW0_funct){
  #Matrix of the two distributions:
  matW0_funct=matrix(byrow=TRUE,nrow=20,ncol=2,data=c(0.111, 0.044, 0.030, 0.011, 0.050, 0.049, 0.095, 0.055, 0.156, 0.087, 0.055, 0.011, 0.035, 0.011, 0.131, 0.066, 0.015, 0.005, 0.181, 0.257, 0.030, 0.033, 0.060, 0.077, 0.005, 0.011, 0.005, 0.011, 0.010, 0.109, 0.000, 0.038, 0.000, 0.000, 0.000, 0.022, 0.000, 0.005, 0.030, 0.098))
  columnToSelect_funct=ifelse(modelW0_funct=="Negative Welfare",1,2)
  valuesW0_funct=1:20
  sample(valuesW0_funct, 
         prob = matW0_funct[,columnToSelect_funct],
         size = 1)
}


#Random Psi
randomPsi=function(){
  return(runif(1,min=0.9,max=1))
}


#Random killing penalty
randomKillingpenalty=function(){
  runif(1, min = 0, max = 365.25*2)
}


#Random m: Generate random draw for m (monetary value of a daily QALY)
randomM=function(){
  matrix_funct=matrix(data=NA,nrow=8,ncol=5)
  colnames(matrix_funct)=c("BaseCase","Scenario1","Scenario2","Scenario3","Scenario4")
  rownames(matrix_funct)=c("Weighted - 2.5%","Minus10 - 2.5%","Plus10 - 2.5%", "Alternate VSL value - 2.5%", "Weighted - 4.5%","Minus10 - 4.5%","Plus10 - 4.5%", "Alternate VSL value - 4.5%")
  matrix_funct[,1]=c(147093,163437,135093,304373,201398,223776,185312,416743)
  matrix_funct[,2]=c(142218,158020,130279,294284,195960,217703,179856,405490)
  matrix_funct[,3]=c(147840,164267,135093,305918,202672,225191,185392,419379)
  matrix_funct[,4]=c(144215,160239,131104,298416,199417,221575,181289,412645)
  matrix_funct[,5]=c(138214,153571,127855,285999,189603,210670,175848,392336)
  weights_funct=runif(nrow(matrix_funct)*ncol(matrix_funct),min=0,max=1)
  weights_funct=weights_funct/sum(weights_funct)
  return(sum(weights_funct*matrix_funct))
}


#Random delta: Generate random delta, the minimum of the welfare function
randomDelta=function(){
  runif(1,min=-3,max=-1)
}


#Joint distribution: Generate a joint draw for delta and W0 that are consistent with the constraints imposed in the sensitivity analysis
joint_W0_delta=function(modelW0_funct){
  bool_funct=TRUE
  while(bool_funct){
    W0_funct=randomW0(modelW0_funct)
    delta_funct=randomDelta()
    if(is.na(q_deriv_funct(W0_funct = W0_funct,delta_funct=delta_funct, vp_funct=20))==FALSE){
      if(q_deriv_funct(W0_funct = W0_funct,delta_funct=delta_funct, vp_funct=20)<=0){
        bool_funct=FALSE
      }
    }
  }
  return(list(W0=W0_funct, delta=delta_funct))
}


#Return q(t): the instantaneous utility at time t
compute_q=function(W0_funct,delta_funct,vp_funct){
  #W0_funct: welfare threshold
  #delta_funct: minimum of the welfare function
  #vp_funct: number of violation points
  a_funct=(delta_funct+20/W0_funct-1)/(400-20*W0_funct)
  b_funct=-1/W0_funct-a_funct*W0_funct
  c_funt=1
  return(a_funct*vp_funct^2+b_funct*vp_funct+c_funt)
}


#Return q'(t): the derivative to the utility at time t
q_deriv_funct=function(W0_funct,delta_funct,vp_funct){
  #W0_funct: welfare threshold
  #delta_funct: minimum of the welfare function
  #vp_funct: number of violation points
  a_funct=(delta_funct+20/W0_funct-1)/(400-20*W0_funct)
  b_funct=-1/W0_funct-a_funct*W0_funct
  c_funt=1
  return(2*a_funct*vp_funct+b_funct)
}


#Return integral of q(t): compute the lifetime welfare of an animal
compute_integral_q=function(W0_funct,delta_funct,t_vec_funct,vp_vec_funct){
  #W0_funct: welfare threshold
  #delta_funct: minimum of the welfare function
  #t_vec_funct: vector of days in each welfare state
  #vp_vec_funct: vector of violation points in each welfare state
  t_vec_funct%*%(compute_q(W0_funct,delta_funct,vp_vec_funct))
}


#Compute v: compute the utility-potential-weighted lifetime welfare of an animal  
compute_v=function(W0_funct,delta_funct,t_vec_funct,vp_vec_funct, phi_funct){
  #W0_funct: welfare threshold
  #delta_funct: minimum of the welfare function
  #t_vec_funct: vector of days in each welfare state
  #vp_vec_funct: vector of violation points in each welfare state
  #phi_funct: utility potential of the animal
  return(phi_funct*compute_integral_q(W0_funct,delta_funct,t_vec_funct,vp_vec_funct))
}


#Compute contribution to social welfare
compute_SWcontribution=function(W0_funct,delta_funct,t_vec_funct,vp_vec_funct, phi_funct,m_funct,SWF_funct, cl_funct=NULL, beta_funct=NULL, human_utility_funct=NULL){
  #W0_funct: welfare threshold
  #delta_funct: minimum of the welfare function
  #t_vec_funct: vector of days in each welfare state
  #vp_vec_funct: vector of violation points in each welfare state
  #phi_funct: utility potential of the animal
  #m_funct: monetary value of one daily QALY for humans
  #SWF_funct: selected social welfare function
  #cl_funct: critical level (for CLU only)
  #beta_funct: calibration parameter for the prioritarian function (for Prioritarianism only)
  #human_utility_funct: average utility of a human  (for Prioritarianism only)
  if(is.null(cl_funct) & SWF_funct=="Critical-level") stop("You need to define a critical level.")
  if(is.null(cl_funct) & SWF_funct=="CL-Prioritarian") stop("You need to define a critical level.")
  if(SWF_funct=="Total") res_funct=m_funct*compute_v(W0_funct,delta_funct,t_vec_funct,vp_vec_funct, phi_funct)
  #Note: We assume instantaneous critical-level: I define a critical-level for each day of the animal's life
  if(SWF_funct=="Critical-level") res_funct=m_funct*(compute_v(W0_funct,delta_funct,t_vec_funct,vp_vec_funct, phi_funct)-cl_funct*365.25)
  if(SWF_funct=="Prioritarian") res_funct=(m_funct*365.25)*prioritarian_trans(compute_v(W0_funct,delta_funct,t_vec_funct/365.25,vp_vec_funct, phi_funct),beta_funct)/deriv_prioritarian_trans(human_utility_funct,beta_funct)
  if(SWF_funct=="CL-Prioritarian") res_funct=(m_funct*365.25)*(prioritarian_trans(compute_v(W0_funct,delta_funct,t_vec_funct/365.25,vp_vec_funct, phi_funct),beta_funct)-prioritarian_trans(cl_funct,beta_funct))/deriv_prioritarian_trans(human_utility_funct,beta_funct)
  return(res_funct)
}


#Compute contribution to monetized welfare for an animal
compute_monetizedAW=function(W0_funct,delta_funct,animal_funct,m_funct,SWF_funct,mu_funct=NULL, beta_funct=NULL, human_utility_funct=NULL,psi_funct=1){
  if(is.null(mu_funct) & SWF_funct=="Critical-level") stop("You need to define mu_funct for Critical-level SWF.")
    if(is.null(mu_funct) & SWF_funct=="CL-Prioritarian") stop("You need to define a critical level for CL-prioritarianism.")
    if(is.null(beta_funct) & SWF_funct=="Prioritarian") stop("You need to define beta_funct for prioritanianism.")
    if(is.null(beta_funct) & SWF_funct=="CL-Prioritarian") stop("You need to define beta_funct for CL-prioritanianism.")
    if(is.null(human_utility_funct) & SWF_funct=="Prioritarian") stop("You need to define human utility for prioritanianism.")
    if(is.null(human_utility_funct) & SWF_funct=="CL-Prioritarian") stop("You need to define human utility for CL-prioritanianism.")
    t_vec_funct=animalsData[[animal_funct]]$time
    vp_vec_funct=animalsData[[animal_funct]]$violationPoints
    phi_funct=(animalsData[[animal_funct]]$neurons/humanNeurons)^psi_funct
    cl_funct=NULL
    if(SWF_funct=="Critical-level" | SWF_funct=="CL-Prioritarian") cl_funct=animalsData[[animal_funct]]$lifeExpectancy*phi_funct*mu_funct
    if(SWF_funct=="Critical-level"){
      res_funct=compute_SWcontribution(W0_funct,delta_funct,t_vec_funct,vp_vec_funct, phi_funct,m_funct,SWF_funct,cl_funct)
    }else if(SWF_funct=="Total"){
      res_funct=compute_SWcontribution(W0_funct,delta_funct,t_vec_funct,vp_vec_funct, phi_funct,m_funct,SWF_funct)
    }else if(SWF_funct=="Prioritarian"){
      res_funct=compute_SWcontribution(W0_funct,delta_funct,t_vec_funct,vp_vec_funct, phi_funct,m_funct,SWF_funct,beta_funct=beta_funct,human_utility_funct=human_utility_funct)
    }else if(SWF_funct=="CL-Prioritarian"){
      res_funct=compute_SWcontribution(W0_funct,delta_funct,t_vec_funct,vp_vec_funct, phi_funct,m_funct,SWF_funct,cl_funct,beta_funct=beta_funct,human_utility_funct=human_utility_funct)
    }
    res_funct
}



### Simulations for sensitivity analysis ###

randomDraw_total_welfare=function(sensit_q=FALSE, sensit_killing_penalty=FALSE, sensit_phi=FALSE, SWF_funct="Total"){
  #sensit_q: BOOLEAN indicating whether we do sensitivity of welfare function q
  #sensit_killing_penalty: BOOLEAN indicating whether we do sensitivity of killing penalty
  #sensit_phi: BOOLEAN indicating whether we do sensitivity of utility potential phi
  #SWF_funct: indicating which social welfare function to consider
  if(sensit_q==TRUE){
    modelW0_funct=randomModelW0()
    jointDraw_funct=joint_W0_delta(modelW0_funct)
    W0_funct=jointDraw_funct$W0
    delta_funct=jointDraw_funct$delta
  }else{
    W0_funct=10
    delta_funct=-1
    modelW0_funct="Baseline"
  }
  if(sensit_phi==TRUE){
    psi_funct=randomPsi()
    #UP_funct=randomUP()  
  }else{
    psi_funct=1
  }
  if(sensit_killing_penalty==TRUE){
    animalsData <<- lapply(animalsData, function(animal) {
      animal$killing_penalty <- randomKillingpenalty()*((animal$neurons/humanNeurons)^psi_funct)
      return(animal)
    })
  }else{
    animalsData <<- lapply(animalsData, function(animal) {
      animal$killing_penalty <- 365*((animal$neurons/humanNeurons)^psi_funct) # we take one year of full welfare, so that's 1 x 1
      return(animal)
    })
  }
  if(SWF_funct=="Prioritarian"){
    HU_funct=nonrandomHU()
    Pi_funct=randomPi()
    beta_funct=compute_beta(X1_funct=10, X2_funct=40,pi_funct=Pi_funct)
    mu_funct=NULL
  }
  if(SWF_funct=="Critical-level"){
    HU_funct=NULL
    Pi_funct=NULL
    beta_funct=NULL
    mu_funct=randomMu()
  }
  if(SWF_funct=="Total"){
    HU_funct=NULL
    Pi_funct=NULL
    beta_funct=NULL
    mu_funct=NULL
  }
  if(SWF_funct=="CL-Prioritarian"){
    HU_funct=nonrandomHU()
    Pi_funct=randomPi()
    beta_funct=compute_beta(X1_funct=10, X2_funct=40,pi_funct=Pi_funct)
    mu_funct=randomMu()
  }
  
  calculate_differentiated_animals(percentage_integration = 0.1, percentage_bovine = 0.8)
  
  temp <- animalsData %>%
    imap_dfr(~ tibble(
      category = .y,
      uniform_moderate_substituted_nbr = .x$nbr_moderate*percentage_integration,
      uniform_AQALY_moderate = uniform_moderate_substituted_nbr*compute_monetizedAW(W0_funct=W0_funct,
                                                                                    delta_funct=delta_funct,
                                                                                    animal_funct=.y,
                                                                                    m_funct=1,
                                                                                    SWF_funct=SWF_funct,
                                                                                    mu_funct=mu_funct, 
                                                                                    beta_funct=beta_funct, 
                                                                                    human_utility_funct=HU_funct,
                                                                                    psi_funct=psi_funct),
      uniform_severe_substituted_nbr = .x$nbr_severe*percentage_integration,
      uniform_AQALY_severe = uniform_severe_substituted_nbr*compute_monetizedAW(W0_funct=W0_funct,
                                                                                delta_funct=delta_funct,
                                                                                animal_funct=.y,
                                                                                m_funct=1,
                                                                                SWF_funct=SWF_funct,
                                                                                mu_funct=mu_funct, 
                                                                                beta_funct=beta_funct, 
                                                                                human_utility_funct=HU_funct,
                                                                                psi_funct=psi_funct),
      
      
      premium_meat_moderate_substituted_nbr = .x$differentiated_moderate_substituted_nbr,
      premium_meat_AQALY_moderate = premium_meat_moderate_substituted_nbr*compute_monetizedAW(W0_funct=W0_funct,
                                                                                              delta_funct=delta_funct,
                                                                                              animal_funct=.y,
                                                                                              m_funct=1,
                                                                                              SWF_funct=SWF_funct,
                                                                                              mu_funct=mu_funct, 
                                                                                              beta_funct=beta_funct, 
                                                                                              human_utility_funct=HU_funct,
                                                                                              psi_funct=psi_funct),
      premium_meat_severe_substituted_nbr = .x$differentiated_severe_substituted_nbr,
      premium_meat_AQALY_severe = premium_meat_severe_substituted_nbr*compute_monetizedAW(W0_funct=W0_funct,
                                                                                          delta_funct=delta_funct,
                                                                                          animal_funct=.y,
                                                                                          m_funct=1,
                                                                                          SWF_funct=SWF_funct,
                                                                                          mu_funct=mu_funct, 
                                                                                          beta_funct=beta_funct, 
                                                                                          human_utility_funct=HU_funct,
                                                                                          psi_funct=psi_funct)        
    ))
  
  total_welfare_uniform_moderate = sum(temp$uniform_AQALY_moderate, na.rm = TRUE)
  total_welfare_uniform_severe = sum(temp$uniform_AQALY_severe, na.rm = TRUE)
  total_welfare_premium_meat_moderate = sum(temp$premium_meat_AQALY_moderate, na.rm = TRUE)
  total_welfare_premium_meat_severe = sum(temp$premium_meat_AQALY_severe, na.rm = TRUE)
  
  calculate_differentiated_animals(percentage_integration = 0.1, percentage_bovine = 0.2)
  
  temp <- animalsData %>%
    imap_dfr(~ tibble(
      category = .y,
      low_cost_moderate_substituted_nbr = .x$differentiated_moderate_substituted_nbr,
      low_cost_AQALY_moderate = low_cost_moderate_substituted_nbr*compute_monetizedAW(W0_funct=W0_funct,
                                                                                      delta_funct=delta_funct,
                                                                                      animal_funct=.y,
                                                                                      m_funct=1,
                                                                                      SWF_funct=SWF_funct,
                                                                                      mu_funct=mu_funct, 
                                                                                      beta_funct=beta_funct, 
                                                                                      human_utility_funct=HU_funct,
                                                                                      psi_funct=psi_funct),
      low_cost_severe_substituted_nbr = .x$differentiated_severe_substituted_nbr,
      low_cost_AQALY_severe = low_cost_severe_substituted_nbr*compute_monetizedAW(W0_funct=W0_funct,
                                                                                  delta_funct=delta_funct,
                                                                                  animal_funct=.y,
                                                                                  m_funct=1,
                                                                                  SWF_funct=SWF_funct,
                                                                                  mu_funct=mu_funct, 
                                                                                  beta_funct=beta_funct, 
                                                                                  human_utility_funct=HU_funct,
                                                                                  psi_funct=psi_funct)        
    ))
  
  total_welfare_low_cost_moderate = sum(temp$low_cost_AQALY_moderate, na.rm = TRUE)
  total_welfare_low_cost_severe = sum(temp$low_cost_AQALY_severe, na.rm = TRUE)
  
  
  # And now the killing-adjusted approach!
  
  calculate_differentiated_animals(percentage_integration = 0.1, percentage_bovine = 0.8)
  
  temp <- animalsData %>%
    imap_dfr(~ tibble(
      category = .y,
      uniform_moderate_substituted_nbr = .x$nbr_moderate*percentage_integration,
      uniform_AQALY_moderate = uniform_moderate_substituted_nbr*(compute_monetizedAW(W0_funct=W0_funct,
                                                                                     delta_funct=delta_funct,
                                                                                     animal_funct=.y,
                                                                                     m_funct=1,
                                                                                     SWF_funct=SWF_funct,
                                                                                     mu_funct=mu_funct, 
                                                                                     beta_funct=beta_funct, 
                                                                                     human_utility_funct=HU_funct,
                                                                                     psi_funct=psi_funct) - .x$killing_penalty),
      uniform_severe_substituted_nbr = .x$nbr_severe*percentage_integration,
      uniform_AQALY_severe = uniform_severe_substituted_nbr*(compute_monetizedAW(W0_funct=W0_funct,
                                                                                 delta_funct=delta_funct,
                                                                                 animal_funct=.y,
                                                                                 m_funct=1,
                                                                                 SWF_funct=SWF_funct,
                                                                                 mu_funct=mu_funct, 
                                                                                 beta_funct=beta_funct, 
                                                                                 human_utility_funct=HU_funct,
                                                                                 psi_funct=psi_funct) - .x$killing_penalty),
      
      
      premium_meat_moderate_substituted_nbr = .x$differentiated_moderate_substituted_nbr,
      premium_meat_AQALY_moderate = premium_meat_moderate_substituted_nbr*(compute_monetizedAW(W0_funct=W0_funct,
                                                                                               delta_funct=delta_funct,
                                                                                               animal_funct=.y,
                                                                                               m_funct=1,
                                                                                               SWF_funct=SWF_funct,
                                                                                               mu_funct=mu_funct, 
                                                                                               beta_funct=beta_funct, 
                                                                                               human_utility_funct=HU_funct,
                                                                                               psi_funct=psi_funct) - .x$killing_penalty),
      premium_meat_severe_substituted_nbr = .x$differentiated_severe_substituted_nbr,
      premium_meat_AQALY_severe = premium_meat_severe_substituted_nbr*(compute_monetizedAW(W0_funct=W0_funct,
                                                                                           delta_funct=delta_funct,
                                                                                           animal_funct=.y,
                                                                                           m_funct=1,
                                                                                           SWF_funct=SWF_funct,
                                                                                           mu_funct=mu_funct, 
                                                                                           beta_funct=beta_funct, 
                                                                                           human_utility_funct=HU_funct,
                                                                                           psi_funct=psi_funct) - .x$killing_penalty)        
    ))
  
  killing_adjusted_total_welfare_uniform_moderate = sum(temp$uniform_AQALY_moderate, na.rm = TRUE)
  killing_adjusted_total_welfare_uniform_severe = sum(temp$uniform_AQALY_severe, na.rm = TRUE)
  killing_adjusted_total_welfare_premium_meat_moderate = sum(temp$premium_meat_AQALY_moderate, na.rm = TRUE)
  killing_adjusted_total_welfare_premium_meat_severe = sum(temp$premium_meat_AQALY_severe, na.rm = TRUE)
  
  calculate_differentiated_animals(percentage_integration = 0.1, percentage_bovine = 0.2)
  temp <- animalsData %>%
    imap_dfr(~ tibble(
      category = .y,
      low_cost_moderate_substituted_nbr = .x$differentiated_moderate_substituted_nbr,
      low_cost_AQALY_moderate = low_cost_moderate_substituted_nbr*(compute_monetizedAW(W0_funct=W0_funct,
                                                                                       delta_funct=delta_funct,
                                                                                       animal_funct=.y,
                                                                                       m_funct=1,
                                                                                       SWF_funct=SWF_funct,
                                                                                       mu_funct=mu_funct, 
                                                                                       beta_funct=beta_funct, 
                                                                                       human_utility_funct=HU_funct,
                                                                                       psi_funct=psi_funct) - .x$killing_penalty),
      low_cost_severe_substituted_nbr = .x$differentiated_severe_substituted_nbr,
      low_cost_AQALY_severe = low_cost_severe_substituted_nbr*(compute_monetizedAW(W0_funct=W0_funct,
                                                                                   delta_funct=delta_funct,
                                                                                   animal_funct=.y,
                                                                                   m_funct=1,
                                                                                   SWF_funct=SWF_funct,
                                                                                   mu_funct=mu_funct, 
                                                                                   beta_funct=beta_funct, 
                                                                                   human_utility_funct=HU_funct,
                                                                                   psi_funct=psi_funct) - .x$killing_penalty)       
    ))
  
  killing_adjusted_total_welfare_low_cost_moderate = sum(temp$low_cost_AQALY_moderate, na.rm = TRUE)
  killing_adjusted_total_welfare_low_cost_severe = sum(temp$low_cost_AQALY_severe, na.rm = TRUE)
  
  
  return(list(SWF=SWF_funct,modelW0=modelW0_funct,W0=W0_funct,delta=delta_funct,psi=psi_funct,HU=HU_funct, Pi=Pi_funct, beta=beta_funct, Mu=mu_funct, total_welfare_uniform_moderate, total_welfare_uniform_severe, total_welfare_premium_meat_moderate, total_welfare_premium_meat_severe,  total_welfare_low_cost_moderate, total_welfare_low_cost_severe, killing_adjusted_total_welfare_uniform_moderate, killing_adjusted_total_welfare_uniform_severe, killing_adjusted_total_welfare_premium_meat_moderate, killing_adjusted_total_welfare_premium_meat_severe,  killing_adjusted_total_welfare_low_cost_moderate, killing_adjusted_total_welfare_low_cost_severe))
}


launchSimulation=function(S_funct, sensit_q=FALSE, sensit_killing_penalty=FALSE, sensit_phi=FALSE, SWF_funct="Total"){
  tmp_data=as.data.frame(t(sapply(1:S_funct, function(i) unlist(randomDraw_total_welfare(sensit_q, sensit_killing_penalty, sensit_phi,SWF_funct)))))
  suppressWarnings({
    tmp_data2=tmp_data %>% mutate_if(is.character,as.numeric)
  })
  tmp_data2[,1]=tmp_data[,1]
  tmp_data2[,2]=tmp_data[,2]
  tmp_data2$SWL_CL=ifelse(tmp_data2$SWF=="Critical-level",1,0)
  tmp_data2$SWL_PU=ifelse(tmp_data2$SWF=="Prioritarian",1,0)
  tmp_data2$SWL_CLPU=ifelse(tmp_data2$SWF=="CL-Prioritarian",1,0)
  return(tmp_data2)  
}


simSummary_total_welfare=function(S_funct, sensit_q=FALSE, sensit_killing_penalty=FALSE, sensit_phi=FALSE, all_parameters=FALSE){
  length_funct=12
  resMat_funct=matrix(data=NA,nrow=17,ncol=length_funct)
  #print(resMat_funct)
  for(i in 1:length_funct) resMat_funct[1,i]= randomDraw_total_welfare()[[i+9]]
  if(sensit_q==TRUE){
    for(i in 1:length_funct){
      resSim_funct=launchSimulation(S_funct=S_funct, sensit_q=sensit_q)[i+5][[1]]
      
      resMat_funct[2,i]=round(mean(resSim_funct),2)
      resMat_funct[3,i]=round(median(resSim_funct),2)
      resMat_funct[4,i]=round(quantile(resSim_funct,c(0.1)),2)
      resMat_funct[5,i]=round(quantile(resSim_funct,c(0.9)),2)
    }
  }
  if(sensit_killing_penalty==TRUE){
    for(i in 1:length_funct){
      resSim_funct=launchSimulation(S_funct=S_funct,sensit_killing_penalty=sensit_killing_penalty)[i+5][[1]]
      resMat_funct[6,i]=round(mean(resSim_funct),2)
      resMat_funct[7,i]=round(median(resSim_funct),2)
      resMat_funct[8,i]=round(quantile(resSim_funct,c(0.1)),2)
      resMat_funct[9,i]=round(quantile(resSim_funct,c(0.9)),2)
    }
  }
  if(sensit_phi==TRUE){
    for(i in 1:length_funct){
      resSim_funct=launchSimulation(S_funct=S_funct,sensit_phi=sensit_phi)[i+5][[1]]
      resMat_funct[10,i]=round(mean(resSim_funct),2)
      resMat_funct[11,i]=round(median(resSim_funct),2)
      resMat_funct[12,i]=round(quantile(resSim_funct,c(0.1)),2)
      resMat_funct[13,i]=round(quantile(resSim_funct,c(0.9)),2)
    }
  }
  if(all_parameters==TRUE){
    for(i in 1:length_funct){
      resSim_funct=launchSimulation(S_funct=S_funct,sensit_q=sensit_q, sensit_phi=sensit_phi, sensit_killing_penalty=sensit_killing_penalty, )[i+5][[1]]
      resMat_funct[14,i]=round(mean(resSim_funct),2)
      resMat_funct[15,i]=round(median(resSim_funct),2)
      resMat_funct[16,i]=round(quantile(resSim_funct,c(0.1)),2)
      resMat_funct[17,i]=round(quantile(resSim_funct,c(0.9)),2)
    }
  }
  rownames(resMat_funct)=c("Baseline","Sensit q - Mean","Sensit q - Median","Sensit q - 1st Decile","Sensit q - 9th Decile","Sensit killing_penalty - Mean","Sensit killing_penalty - Median","Sensit killing_penalty - 1st Decile","Sensit killing_penalty - 9th Decile","Sensit phi - Mean","Sensit phi - Median","Sensit phi - 1st Decile","Sensit phi - 9th Decile", "Sensit total - Mean","Sensit total - Median","Sensit total - 1st Decile","Sensit total - 9th Decile")
  colnames(resMat_funct)=c("Utilitarian Uniform moderate", "Utilitarian Uniform severe", "Utilitarian premium meat moderate", "Utilitarian premium meat severe", "Utilitarian low-cost moderate","Utilitarian low-cost severe", "Killing-adjusted Uniform moderate", "Killing-adjusted Uniform severe", "Killing-adjusted premium meat moderate", "Killing-adjusted premium meat severe", "Killing-adjusted low-cost moderate","Killing-adjusted low-cost severe")
  return(resMat_funct)
}



