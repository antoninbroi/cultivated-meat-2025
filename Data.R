#Set elements
animalsData=list()

#Global parameters
humanNeurons=24526423164

# Distribution of meat tonnage and animal numbers (derived from Appendix B table 1)

chicken_meat_tonnes=c(181000000)
pork_meat_tonnes=c(143000000)
bovine_meat_tonnes=c(106000000)
total_meat_tonnes=c(chicken_meat_tonnes + pork_meat_tonnes + bovine_meat_tonnes)

chicken_nbr=c(97779377404)
pork_nbr=c(1740465142)
bovine_nbr=c(429062803)

dairy_bovine_nbr= c(95681005)
beef_bovine_nbr= c(333381798)

# Pork/chicken distribution in total non-bovine meat production
chicken_share = chicken_meat_tonnes / (pork_meat_tonnes + chicken_meat_tonnes)
pork_share = pork_meat_tonnes / (pork_meat_tonnes + chicken_meat_tonnes)


### Populations (derived from Appendix B with calculations described in Methods 4)
# Moderate Intensification

animalsData$chickenIntensive$nbr_moderate=c(97779377404*0.859)
animalsData$chickenExtensive$nbr_moderate=c(97779377404*(1-0.859))
animalsData$porkIntensive$nbr_moderate=c(1740465142*0.595)
animalsData$porkExtensive$nbr_moderate=c(1740465142*(1-0.595))
animalsData$dairyHerdIntensive$nbr_moderate=c(0)
animalsData$dairyHerdExtensive$nbr_moderate=c(dairy_bovine_nbr)
animalsData$beefHerdIntensive$nbr_moderate=c(0)
animalsData$beefHerdExtensive$nbr_moderate=c(beef_bovine_nbr)

# Severe intensification
animalsData$chickenIntensive$nbr_severe=c(97779377404*0.999)
animalsData$chickenExtensive$nbr_severe=c(97779377404*(1-0.999))
animalsData$porkIntensive$nbr_severe=c(1740465142*0.986)
animalsData$porkExtensive$nbr_severe=c(1740465142*(1-0.986))
animalsData$dairyHerdIntensive$nbr_severe=c(dairy_bovine_nbr*0.749)
animalsData$dairyHerdExtensive$nbr_severe=c(dairy_bovine_nbr*(1-0.749))
animalsData$beefHerdIntensive$nbr_severe=c(beef_bovine_nbr*0.749)
animalsData$beefHerdExtensive$nbr_severe=c(beef_bovine_nbr*(1-0.749))

#Indicator function for distinguishing premium animal products (i.e. cows) in the differentiated scenario
animalsData$chickenIntensive$premium=c(0)
animalsData$chickenExtensive$premium=c(0)
animalsData$porkIntensive$premium=c(0)
animalsData$porkExtensive$premium=c(0)
animalsData$dairyHerdIntensive$premium=c(1)
animalsData$dairyHerdExtensive$premium=c(1)
animalsData$beefHerdIntensive$premium=c(1)
animalsData$beefHerdExtensive$premium=c(1)     


### Characteristics

animalsData$chickenIntensive$time=c(40)
animalsData$chickenIntensive$violationPoints=c(16)
animalsData$chickenIntensive$neurons=85614522
animalsData$chickenIntensive$weight=2.5
animalsData$chickenIntensive$moralWeight=c(0.002, 0.641, 0.512, 0.152, 0.165, 0.554, 0.451, 0.257, 0.154, 0.901, 0.825, 0.953, 0.421, 0.295)
animalsData$chickenIntensive$lifeExpectancy=7.5

animalsData$chickenExtensive$time=c(81)
animalsData$chickenExtensive$violationPoints=c(8)
animalsData$chickenExtensive$neurons=85614522
animalsData$chickenExtensive$weight=2.5
animalsData$chickenExtensive$moralWeight=c(0.002, 0.641, 0.512, 0.152, 0.165, 0.554, 0.451, 0.257, 0.154, 0.901, 0.825, 0.953, 0.421, 0.295)
animalsData$chickenExtensive$lifeExpectancy=7.5

animalsData$porkIntensive$time=c(180)
animalsData$porkIntensive$violationPoints=c(16)
animalsData$porkIntensive$neurons=554054029
animalsData$porkIntensive$weight=75
animalsData$porkIntensive$moralWeight=c(0.005, 1.184, 0.594, 0.188, 0.266, 0.630, 0.564, 0.301, 0.173, 1.019, 0.951, 1.070, 0.611, 0.477)
animalsData$porkIntensive$lifeExpectancy=15

animalsData$porkExtensive$time=c(290)
animalsData$porkExtensive$violationPoints=c(7)
animalsData$porkExtensive$neurons=554054029
animalsData$porkExtensive$weight=75
animalsData$porkExtensive$moralWeight=c(0.005, 1.184, 0.594, 0.188, 0.266, 0.630, 0.564, 0.301, 0.173, 1.019, 0.951, 1.070, 0.611, 0.477)
animalsData$porkExtensive$lifeExpectancy=15

animalsData$dairyHerdIntensive$time=c(2920)
animalsData$dairyHerdIntensive$violationPoints=c(15)
animalsData$dairyHerdIntensive$neurons=790446591
animalsData$dairyHerdIntensive$weight=300
animalsData$dairyHerdIntensive$lifeExpectancy=20

animalsData$dairyHerdExtensive$time=c(2920)
animalsData$dairyHerdExtensive$violationPoints=c(8)
animalsData$dairyHerdExtensive$neurons=790446591
animalsData$dairyHerdExtensive$weight=300
animalsData$dairyHerdExtensive$lifeExpectancy=20

animalsData$beefHerdIntensive$time=c(730)
animalsData$beefHerdIntensive$violationPoints=c(13)
animalsData$beefHerdIntensive$neurons=790446591
animalsData$beefHerdIntensive$weight=300
animalsData$beefHerdIntensive$lifeExpectancy=20

animalsData$beefHerdExtensive$time=c(1095)
animalsData$beefHerdExtensive$violationPoints=c(6)
animalsData$beefHerdExtensive$neurons=790446591
animalsData$beefHerdExtensive$weight=300
animalsData$beefHerdExtensive$lifeExpectancy=20

