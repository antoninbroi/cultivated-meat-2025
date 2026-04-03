simulation_estimates=function(animalsData_funct, W0_funct=10, delta_funct=-1, scenario_name, killing_penalty=FALSE, psi_funct=1){
  tmp_res = computeAQALYs(animalsData_funct=animalsData_funct, 
                                     W0_funct=W0_funct, 
                                     delta_funct=delta_funct,
                                    killing_penalty=killing_penalty,
                                    psi_funct=psi_funct) 
  tmp_res$scenario=scenario_name
  tmp_res=tmp_res %>% relocate(scenario)
}

sim_all_estimates = function(i_funct,jointDraw_funct=NULL, psi_funct=NULL, kp_funct=NULL){
   case_funct= ""
   if(!is.null(jointDraw_funct)) case_funct="q_sensit"
   if(!is.null(psi_funct)) case_funct="psi_sensit"
   if(!is.null(kp_funct)) case_funct="kp_sensit"
  
   W0_funct=10
   delta_funct=-1
   if(is.null(psi_funct)) psi=1
   if(!is.null(psi_funct)) psi=psi_funct
   
   animalsData_scenario3_funct=animalsData_scenario3
   animalsData_scenario4_funct=animalsData_scenario4
   animalsData_scenario5_funct=animalsData_scenario5

   if(case_funct=="q_sensit"){
     W0_funct=jointDraw_funct$W0
     delta_funct=jointDraw_funct$delta
   }else if(case_funct=="psi_sensit"){
     
     animalsData_scenario3_funct=data_with_killing_penalty(animalsData, psi_funct=psi)
     animalsData_scenario4_funct=data_with_differentiated_animals(animalsData, percentage_integration = 0.1, percentage_bovine = 0.8)
     animalsData_scenario4_funct=data_with_killing_penalty(animalsData_scenario4, psi_funct=psi)
     
     animalsData_scenario5_funct=data_with_differentiated_animals(animalsData, percentage_integration = 0.1, percentage_bovine = 0.2)
     animalsData_scenario5_funct=data_with_killing_penalty(animalsData_scenario5_funct, psi_funct=psi)
   }else if(case_funct=="kp_sensit"){
     animalsData_scenario3_funct=data_with_killing_penalty(animalsData, killing_penalty_in_days=kp_funct)
     
     animalsData_scenario4_funct=data_with_differentiated_animals(animalsData, percentage_integration = 0.1, percentage_bovine = 0.8)
     animalsData_scenario4_funct=data_with_killing_penalty(animalsData_scenario4, killing_penalty_in_days=kp_funct)
     
     animalsData_scenario5_funct=data_with_differentiated_animals(animalsData, percentage_integration = 0.1, percentage_bovine = 0.2)
     animalsData_scenario5_funct=data_with_killing_penalty(animalsData_scenario5_funct, killing_penalty_in_days=kp_funct)
   }
   
    res_baseline_funct = simulation_estimates(animalsData, W0_funct=W0_funct, delta_funct=delta_funct, scenario_name="Uniform integration / Utilitarian", psi_funct=psi)
    res_scenario1_funct = simulation_estimates(animalsData_scenario1, W0_funct=W0_funct, delta_funct=delta_funct,scenario_name="High-value meat integration / Utilitarian", psi_funct=psi)
    res_scenario2_funct = simulation_estimates(animalsData_scenario2, W0_funct=W0_funct, delta_funct=delta_funct,scenario_name="Commodity meat integration / Utilitarian", psi_funct=psi)
    res_scenario3_funct = simulation_estimates(animalsData_scenario3_funct, W0_funct=W0_funct, delta_funct=delta_funct,scenario_name="Uniform integration / Killing-adjusted", killing_penalty=TRUE, psi_funct=psi)
    res_scenario4_funct = simulation_estimates(animalsData_scenario4_funct, W0_funct=W0_funct, delta_funct=delta_funct,scenario_name="High-value meat integration / Killing-adjusted", killing_penalty=TRUE, psi_funct=psi)
    res_scenario5_funct = simulation_estimates(animalsData_scenario5_funct, W0_funct=W0_funct, delta_funct=delta_funct,scenario_name="Commodity meat integration / Killing-adjusted", killing_penalty=TRUE, psi_funct=psi)
    list_res = list(res_baseline_funct, res_scenario1_funct,res_scenario2_funct,res_scenario3_funct,res_scenario4_funct,res_scenario5_funct)
    res_all_funct=do.call(rbind.data.frame, list_res)
    res_all_funct$simulation_number=i_funct
    res_all_funct=res_all_funct %>% relocate(simulation_number)
    res_all_funct$sensit=case_funct
    
    if(case_funct=="q_sensit"){
      res_all_funct$W0=W0_funct
      res_all_funct$delta=delta_funct
    }else if(case_funct=="psi_sensit"){
      res_all_funct$psi=psi
    }else if(case_funct=="kp_sensit"){
      res_all_funct$kp=kp_funct
    }
    
    
    return(res_all_funct)
  
}