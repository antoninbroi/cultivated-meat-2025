

computeAQALYs = function(animalsData_funct, 
                         killing_penalty=FALSE,
                         W0_funct=10,
                         delta_funct=-1,
                         psi_funct=1){
  #animalsData_funct: dataset to use
  #killing_penalty: equal to TRUE if we already set up a killing penalty in animalsData_funct

  if(killing_penalty==FALSE) animalsData_funct=data_with_killing_penalty(animalsData_funct, psi_funct=psi_funct, killing_penalty_in_days=0)
  tmp_funct=animalsData_funct %>%
    imap_dfr(~ tibble(
      category = .y,
      moderate_substituted_nbr = .x$nbr_moderate*percentage_integration,
      AQALY_moderate = as.numeric(moderate_substituted_nbr*(compute_monetizedAW(W0_funct=W0_funct,
                                                                    delta_funct=delta_funct,
                                                                    animal_funct=.y,
                                                                    m_funct=1, # 402 for monetary equivalent
                                                                    psi_funct=psi_funct,
                                                                    SWF_funct="Total")- .x$killing_penalty)),
      monetized_moderate = as.numeric(moderate_substituted_nbr*(compute_monetizedAW(W0_funct=W0_funct,
                                                                        delta_funct=delta_funct,
                                                                        animal_funct=.y,
                                                                        m_funct=402,
                                                                        psi_funct=psi_funct,
                                                                        SWF_funct="Total")- .x$killing_penalty*402)),
      
      severe_substituted_nbr = .x$nbr_severe*percentage_integration,
      AQALY_severe = as.numeric(severe_substituted_nbr*(compute_monetizedAW(W0_funct=W0_funct,
                                                                delta_funct=delta_funct,
                                                                animal_funct=.y,
                                                                m_funct=1, # 402 for monetary equivalent
                                                                psi_funct=psi_funct,
                                                                SWF_funct="Total")- .x$killing_penalty)),
      monetized_severe = as.numeric(severe_substituted_nbr*(compute_monetizedAW(W0_funct=W0_funct,
                                                                    delta_funct=delta_funct,
                                                                    animal_funct=.y,
                                                                    m_funct=402,
                                                                    psi_funct=psi_funct,
                                                                    SWF_funct="Total")- .x$killing_penalty*402))
    ))
  names(tmp_funct) <- c("category", "moderate_substituted_nbr", "AQALY_moderate", "monetized_moderate", "severe_substituted_nbr", "AQALY_severe", "monetized_severe")
  
  tmp2_funct <- c(sum(tmp_funct$moderate_substituted_nbr, na.rm = TRUE), 
                  sum(tmp_funct$AQALY_moderate, na.rm = TRUE), 
                  sum(tmp_funct$monetized_moderate, na.rm = TRUE), 
                  sum(tmp_funct$severe_substituted_nbr, na.rm = TRUE), 
                  sum(tmp_funct$AQALY_severe, na.rm = TRUE), 
                  sum(tmp_funct$monetized_severe, na.rm = TRUE))
  names(tmp2_funct) <- c("moderate_substituted_nbr", "AQALY_moderate", "monetized_moderate", "severe_substituted_nbr", "AQALY_severe", "monetized_severe")
  tmp2_funct <- tibble::tibble(
    category = "Total",
    !!!as.list(tmp2_funct)
  )
  
  
  tmp_funct = dplyr::bind_rows(tmp_funct, tmp2_funct)
  
  return(tmp_funct)
}





progress_printer = local({
  last = 0
  function(i, S) {
    pct = floor(i / S * 100)
    if (pct > last) {
      message(paste0(pct, "%"))
      last <<- pct
    }
  }
})
