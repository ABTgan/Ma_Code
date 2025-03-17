#Libraries
library(data.table)
library(dplyr)
library(ggplot2)

# Define phenotype names for GBD and HR datasets
gbd_phenos <- c("Total cancers", "Appendicitis", "Asthma", "Atrial fibrillation and flutter", "Breast cancer", "Ischemic heart disease", "Colon and rectum cancer", "Idiopathic epilepsy", "Gout", "Osteoarthritis hip", "Osteoarthritis knee", "Major depressive disorder", "Malignant skin melanoma", "Prostate cancer", "Rheumatoid arthritis", "Diabetes mellitus type 1", "Diabetes mellitus type 2", "Interstitial lung disease and pulmonary sarcoidosis", "Tracheal, bronchus, and lung cancer")
hr_phenos <- c("C3_CANCER", "K11_APPENDACUT", "J10_ASTHMA", "I9_AF", "C3_BREAST","I9_CHD", "C3_COLORECTAL", "G6_EPLEPSY", "GOUT", "COX_ARTHROSIS", "KNEE_ARTHROSIS", "F5_DEPRESSIO", "C3_MELANOMA_SKIN", "C3_PROSTATE", "RHEUMA_SEROPOS_OTH", "T1D", "T2D", "ILD", "C3_BRONCHUS_LUNG")

# Read the GBD 2021 data
gbd2021 <- fread("/genesandhealth/red/ABT/Intervene/Inputs/GBD_2021_data.csv")
gbd2021$val <- as.numeric(gbd2021$val)

#Model pathways
# Create a list of models with file pathways
models <- list(
  model1 = c(name = "Full_Sample", input = "/genesandhealth/red/ABT/Intervene/Phase_2_HR/Outputs/HR_FullSample_TOPMED-r3.csv", output = "/genesandhealth/red/ABT/Intervene/Phase_3_LRE/Outputs/Full_Sample/", s= gbd2021$sex[3]),
  model2 = c(name = "Age_Stratified", input = "/genesandhealth/red/ABT/Intervene/Phase_2_HR/Outputs/HR_AgeStratified_TOPMED-r3.csv", output = "/genesandhealth/red/ABT/Intervene/Phase_3_LRE/Outputs/Age_Stratified/", s= gbd2021$sex[3]),
  model3 = c(name = "AgeSex_Stratified", input = "/genesandhealth/red/ABT/Intervene/Phase_2_HR/Outputs/HR_AgeandSexStratified_TOPMED-r3.csv", output = "/genesandhealth/red/ABT/Intervene/Phase_3_LRE/Outputs/AgeSex_Stratified/", s= gbd2021$sex[3]),
  model4 = c(name = "Female_Stratified", input = "/genesandhealth/red/ABT/Intervene/Phase_2_HR/Outputs/HR_FemaleSample_TOPMED-r3.csv", output = "/genesandhealth/red/ABT/Intervene/Phase_3_LRE/Outputs/Sex_Stratified/Female/", s= gbd2021$sex[2]),
  model5 = c(name = "Male_Stratified", input = "/genesandhealth/red/ABT/Intervene/Phase_2_HR/Outputs/HR_MaleSample_TOPMED-r3.csv", output = "/genesandhealth/red/ABT/Intervene/Phase_3_LRE/Outputs/Sex_Stratified/Male/", s= gbd2021$sex[1])
)

# Loops through each model
for (model in 1:length(models)){
  # Loop through each phenotype to process incidence, prevalence, and mortality data
  for(j in 1:length(gbd_phenos)){
    # Skipping sex specific conditions for specific models
    if(model == 4 & gbd_phenos[j] == "Prostate cancer" ){ # If the model is for female stratification skip prostate cancer
      j = j + 1 # skips by going to next item on the gbd_phenos list. Can cause errors if item is last on the list.
    } else if( model == 5 & gbd_phenos[j] == "Breast cancer"){ #If the model is for male stratification skip breast cancer
      j = j + 1 # skips by going to next item on the gbd_phenos list. Can cause errors if item is last on the list.
    }
    
    #Outputs message to console on current progress
    print(paste(j, "out of", length(gbd_phenos), gbd_phenos[j], "for Model: ", (models[[model]]["name"]), "for: ", (models[[model]]["s"]) ))
    
    ### Incidence
    # Filter data for incidence measure, specific cause, and sex
    incidence <- gbd2021[measure == "Incidence" & cause == gbd_phenos[j] & sex == (models[[model]]["s"])]
    # Calculate incidence per 10,000
    incidence$incidence <- incidence$val / 10000
    
    # Calculate the population for each age group
    population <- c()
    for(i in unique(incidence$age)){
      subby <- subset(incidence, age== i)
      # Calculate the population for each age group
      poppy <- subby$val[1] / (subby$val[2] / 10)
      population <- c(population, poppy)
    }
    
    # Filter data for rate metric
    incidence <- subset(incidence, metric == 'Rate')
    # Handle any NA values in population
    population[is.na(population)] <- 0
    # Add population data to the incidence data
    incidence$population <- population
    #Subset data to prepare for merging 
    incidence <- incidence[, c('location', 'age', 'cause', 'metric', 'population', 'incidence')]
    
    
    ### Prevalence
    # Filter data for prevalence measure, specific cause, both sexes, and rate metric
    prevalence <- gbd2021[measure == "Prevalence" & cause == gbd_phenos[j] & sex == (models[[model]]["s"]) & metric == 'Rate']
    # Divide prevalence rates by 100,000 to get the prevalence as a probability
    prevalence$prevalence <- prevalence$val / 100000
    #Subset data to prepare for merging 
    prevalence <- prevalence[, c('location', 'age', 'cause', 'metric', 'prevalence')]
    
    # Merge incidence and prevalence data
    data_merged <- left_join(incidence, prevalence, by = c("location", "age", "cause", "metric"))
    
    
    ### Mortality 
    # Filter data for deaths measure and rate metric
    mortality <- gbd2021[measure == "Deaths" & metric == 'Rate']
    mortality <- subset(mortality, cause==gbd_phenos[j] | cause=="All causes")
    
    ## All cause mortality
    # Handle special cases for breast cancer and prostate cancer
    if(gbd_phenos[j] == "Breast cancer"){
      # Filter for all-cause mortality for females
      mortality<- subset(mortality, sex == "Female")
      all_cause_mortality <- subset(mortality, cause == "All causes" & sex == "Female")
    } else if(gbd_phenos[j] == "Prostate cancer"){
      # Filter for all-cause mortality for males
      mortality<- subset(mortality, sex == "Male")
      all_cause_mortality <- subset(mortality, cause == "All causes" & sex == "Male")
    } else {
      # Filter for all-cause mortality for both sexes
      mortality<- subset(mortality, sex == "Both")
      all_cause_mortality <- subset(mortality, cause == "All causes" & sex == "Both")
    }
    
    #Subset data to prepare for merging 
    all_cause_mortality <- all_cause_mortality[,c("location","sex","age","val")]
    colnames(all_cause_mortality)[4] <- c("all_cause_rate")
    
    ##Cause Specific mortality
    cause_specific_mortality <- subset(mortality, cause!="All causes" & metric=="Rate")
    #Subset data to prepare for merging 
    cause_specific_mortality <- cause_specific_mortality[,c("location","sex","age","cause","val")]
    colnames(cause_specific_mortality)[5] <- c("cause_specific_rate")
    
    #Merge all /cause/ specific mortality
    mortality <- left_join(cause_specific_mortality, all_cause_mortality)
    #Calculating mortality rate
    mortality$mortality_rate <- (mortality$all_cause_rate - mortality$cause_specific_rate)/100000
    #Subset data to prepare for merging
    mortality <- mortality[,c("location","age","cause","mortality_rate")]
    
    # Merge mortality with previously merged incidence and prevalence data
    data_merged <- left_join(data_merged, mortality)
    data_merged$mortality_rate[is.na(data_merged$mortality_rate)] <- 0
    
    data_merged <- subset(data_merged, age!="All ages" & age!="Birth" & age!="<5 years" & age!="5-9 years" & age!="10-14 years"& age!="80+ years")
    
    ##############################################################################
    
    #Hazard Ratios
    print("Hazard Ratios")  
    #Read in the hazard ratios and allocate to variables...
    hazrats <- fread((models[[model]]["input"]), data.table = FALSE)
    hazrats <- hazrats[,-1]
    hazrats = rename(hazrats, HR = OR)
    hazrats <- subset(hazrats, phenotype==hr_phenos[j])
    
    #Hazard Ratios
    hr01 <- hazrats[1,"HR"]
    hr02 <- hazrats[2,"HR"]
    hr03 <- hazrats[3,"HR"]
    hr04 <- hazrats[4,"HR"]
    hr05 <- hazrats[5,"HR"]
    hr07 <- hazrats[6,"HR"]
    hr08 <- hazrats[7,"HR"]
    hr09 <- hazrats[8,"HR"]
    hr10 <- hazrats[9,"HR"]
    hr11 <- hazrats[10,"HR"]
    
    #Proportions - 0.2 by definition of PRS group
    props01 <- 0.01
    props02 <- 0.04
    props03 <- 0.05
    props04 <- 0.1
    props05 <- 0.2
    props06 <- 0.2
    props07 <- 0.2
    props08 <- 0.1
    props09 <- 0.05
    props10 <- 0.04
    props11 <- 0.01
    
    #Estimate incidence attributable to different distributions of PRS 
    data_merged$i6 <- (data_merged$incidence*data_merged$population) / ((props06 * data_merged$population) + (hr01 * (props01 * data_merged$population)) + (hr02 * (props02 * data_merged$population)) + (hr03 * (props03 * data_merged$population)) + (hr04 * (props04 * data_merged$population)) + (hr05 * (props05 * data_merged$population)) + (hr07 * (props07 * data_merged$population)) + (hr08 * (props08 * data_merged$population)) + (hr09 * (props09 * data_merged$population)) + (hr10 * (props10 * data_merged$population)) + (hr11 * (props11 * data_merged$population))) 
    data_merged$i6[is.na(data_merged$i6)] <- 0
    data_merged$i1 <- data_merged$i6 * hr01
    data_merged$i2 <- data_merged$i6 * hr02
    data_merged$i3 <- data_merged$i6 * hr03
    data_merged$i4 <- data_merged$i6 * hr04
    data_merged$i5 <- data_merged$i6 * hr05
    data_merged$i7 <- data_merged$i6 * hr07
    data_merged$i8 <- data_merged$i6 * hr08
    data_merged$i9 <- data_merged$i6 * hr09
    data_merged$i10 <- data_merged$i6 * hr10
    data_merged$i11 <- data_merged$i6 * hr11
    
    ###################################################
    
    #LRE
    lifetimerisk <- data.frame(NULL)
    for(i in 1:11){
      #Calculate hazard
      data_merged[[paste0("hazard",i)]] <- data_merged[[paste0("i",i)]] / (1 - data_merged$prevalence)
      
      #Calculate probability of experiencing the endpoint within the age interval. hazard multiplied by 5 as that is the age interval and current probabilities are per year. 
      data_merged[[paste0("risk",i)]] <- 1 - exp(-5*data_merged[[paste0("hazard",i)]])
      
      #Mortality and risk
      data_merged[[paste0("mortandrisk",i)]] <- cumsum(data_merged[[paste0("hazard",i)]] + data_merged$mortality_rate)
      
      #Survival
      data_merged[[paste0("survival",i)]] <- exp(-5*data_merged[[paste0("mortandrisk",i)]])
      
      #Calculate lifetime risk as the cumulative sum of the product of survival and risk.
      data_merged[[paste0("lifetimerisk",i)]] <- cumsum(data_merged[[paste0("survival",i)]]*data_merged[[paste0("risk",i)]])*100
      
      result <- data.frame(data_merged$age, paste0("Group", i), data_merged[[paste0("lifetimerisk",i)]])
      lifetimerisk <- rbind(lifetimerisk, result)
    }
    
    colnames(lifetimerisk) <- c("Age","Group","LifetimeRisk")
    
    #Plot all as well as overall lifetime risk
    lifetimerisk$Age <- factor(lifetimerisk$Age, levels=c("15-19 years","20-24 years","25-29 years","30-34 years","35-39 years","40-44 years","45-49 years","50-54 years","55-59 years","60-64 years","65-69 years","70-74 years","75-79 years" ))
    lifetimerisk$Group <- factor(lifetimerisk$Group, levels=c("Group1","Group2","Group3","Group4","Group5","Group6","Group7","Group8","Group9","Group10","Group11"))

    #Writing and saving
    write.csv(lifetimerisk, file = paste0((models[[model]]["output"]), (models[[model]]["name"]), "_" , hr_phenos[j], "_LifetimeRisk_TOPMED-r3.csv"))
    
    #Not considering confidence intervals
    test = ggplot(lifetimerisk, aes(Age, LifetimeRisk, color=Group, group=Group)) +
      stat_smooth(method = "lm", formula = y ~ poly(x, 13), se = FALSE) +
      geom_point() +
      geom_line() +
      ggtitle(gbd_phenos[j]) +
      xlab("Age Range") + 
      ylab("Cumulative Risk (%)") + 
      theme_bw() +
      labs(color='PRS Group') +
      scale_color_hue(labels = c("0-1%", "1-5%", "5-10%", "10-20%", "20-40%", "40-60%", "60-80%", "80-90%", "90-95%", "95-99%", "99-100%")) +
      theme(title = element_text(size = 15),
            plot.title = element_text(hjust = 0.5),
            axis.title.x = element_text(size = 12),
            axis.text.x = element_text(size = 10, angle=-90, hjust=0),
            axis.title.y = element_text(size = 12),
            axis.text.y = element_text(size = 10))
    
    plot(test)
    
   ggsave(paste0((models[[model]]["output"]), (models[[model]]["name"]), "_",hr_phenos[j],"_LifetimeRisk_TOPMEDr-3.png"), height=10 , width=10)
  }
}  




rm(list = ls())

