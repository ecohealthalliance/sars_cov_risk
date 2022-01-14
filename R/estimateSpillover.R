# function to estimate spillover of SARSr-CoVs from bats to humans in SE Asia
# based on bat-human overlap, bat-human contact, seroprevalence data

estimateSpillover <- function(upars, seed = 26, samples, popMean, contactDistr,
                              detectDistr, pastyear_min, pastyear_max){

  #' @param upars # number of unknown parameters to vary
  # name, habitat names, suitability, and importance
  #' @param seed random seed
  #' @param samples set number of reps
  #' has to be high so that sobol indices are all > 1
  #' 
  #' @return returns a dataframe of species habitat suitability in long format
  
  set.seed(seed)

  # construct random Latin hypercube design
  lhssample <- randomLHS(samples, upars)

  colnames(lhssample) <- c("people", "contact", "detection", "pastyear")

  # transform lhs samples on uniform distribution into desired distributions
  
  # assume number of people in consensus bat distribution
  # is normally distributed with mean calculated from WorldPop
  # and SD chosen so that the tails of the distribution (3SD)
  # would extend 10% beyond the mean, to reflect potential WP uncertainty
  peopleSample <- qnorm(lhssample[, "people"], mean = popMean, 
                        sd = popMean*0.1 / 3)
  contactSample <- qbeta(lhssample[, "contact"], 
                         shape1 = contactDistr$estimate[1], 
                         shape2 = contactDistr$estimate[2])
  detectSample <- qbeta(lhssample[, "detection"], 
                        shape1 = detectDistr$estimate[1], 
                        shape2 = detectDistr$estimate[2])

  # last param is also uniform, just needs to be on different scale
  pastyearSample <- (pastyear_max - pastyear_min)*lhssample[, "pastyear"] + 
    pastyear_min
  
  # make data frame of inputs
  lhsdata <- data.frame(peopleSample, contactSample, detectSample, pastyearSample)
  
  # calculate total infected by multiplying
  lhsdata %<>% mutate(infected = ceiling(peopleSample*contactSample*detectSample*pastyearSample)) 
  
  # rename columns
  names(lhsdata) <- c("nPeople", "pContact", "pDetect", "pPastyear", "totInf")
  
  return(lhsdata)
  
}