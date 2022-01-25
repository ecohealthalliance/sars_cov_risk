# set distributions to be used for parameters in sensitivity analysis
# some plots are commented out so that code runs properly when sourced

# determine distribution for Pcontact-------------------------------------------

# vector with contact probabilities from literature
pContactVals <- c(201/1585, 9/1585, 
                  16/150, 69/150, 20/150, 20/150, 25/150, 5/150,
                  128/626, 96/626, 78/626, 65/626, 46/626, 45/626, 41/626, 
                    25/626, 16/626, 
                  61/106, 29/106, 11/106, 6/106, 2/106, 
                  142/305, 
                  3/245)

# skewness-kurtosis plot to examine possible distributions
descdist(pContactVals, boot = 1000)

# fit beta distribution
fitBetaContact <- fitdist(pContactVals, "beta")

#plot(fitBetaContact)

# determine distribution for Pcontact, with highest 3 estimates removed---------

pContactVals_B <- sort(pContactVals)[-c(22:24)]

# skewness-kurtosis plot to examine possible distributions
descdist(pContactVals_B, boot = 1000)

# fit beta distribution
fitBetaContact_B <- fitdist(pContactVals_B, "beta")

# not as nice a fit as when all estimates are included
#plot(fitBetaContact_B)

# determine distribution for Pdetect--------------------------------------------

pDetectVals <- c(0/128,
                 0/4, 0/15,
                 3/99, 7/171, 7/227, 
                 1/199, 1/199, 0/199, 0/199,
                 3/12,
                 2/32,
                 0/15, 0/29,
                 7/50, 0/23)

# skewness-kurtosis plot to examine possible distributions
descdist(pDetectVals, boot = 1000)

# fit beta distribution
fitBetaDetect <- fitdist(pDetectVals, "beta", method = "mme")

#plot(fitBetaDetect)

# determine distribution for Pdetect, with highest 2 estimates removed----------

pDetectVals_B <- sort(pDetectVals)[-c(15:16)]

# skewness-kurtosis plot to examine possible distributions
descdist(pDetectVals_B, boot = 1000)

# fit beta distribution
fitBetaDetect_B <- fitdist(pDetectVals_B, "beta", method = "mme")

#plot(fitBetaDetect_B)

# determine distribution for P(detection due to infection in past year)---------
library(scales)

# import data
IgGs <- read.csv(here("data-raw/IgGtimeseries.csv"))

# plot points
# plot(IgGs$monthsPI, IgGs$percIgGpos, pch = 19, ylim = c(0, 1), 
#      col = alpha("black", 0.4), xlab = "Months post-infection",
#      ylab = "Proportion IgG positive")

# fit 2nd degree polynomial equation
fit2 <- lm(percIgGpos ~ poly(monthsPI, 2, raw = TRUE), data = IgGs)

# generate range of values to predict on
newDat <- seq(0, 72, by = 0.25)

# lines(newDat, predict(fit2, data.frame(monthsPI = newDat)))

# get the coefficients for the polynomial
sopCoefs <- coef(fit2)

# create function for the 2nd order polynomial
# that can be used with integrate function
sop <- function(newVals) {
  #y = c + bx + ax^2
  res <- sopCoefs[1] + (sopCoefs[2] * newVals) + (sopCoefs[3] * newVals^2)
  return(res)
}

# integrate to determine 
# number of people with detectable IgG due to an infection in past ~6 years
# versus those with detectable IgG due to an infection in the past year
allPos <- integrate(sop, lower = 0, upper = 71.5)
yr1Pos <- integrate(sop, lower = 0, upper = 12)

# divide to get proportion that IgG detection is due to infection in past year
pIgG1 <- yr1Pos$value/allPos$value




# re-fit the curve, this time excluding the outlier at 72 months
IgGs2 <- IgGs %>% 
  filter(!monthsPI == 72)

# plot points
# plot(IgGs2$monthsPI, IgGs2$percIgGpos, pch = 19, ylim = c(0, 1), 
#      col = alpha("black", 0.4), xlab = "Months post-infection",
#      ylab = "Proportion IgG positive")

# fit 2nd degree polynomial equation
fit2b <- lm(percIgGpos ~ poly(monthsPI, 2, raw = TRUE), data = IgGs2)

# lines(newDat, predict(fit2b, data.frame(monthsPI = newDat)))

sopCoefsB <- coef(fit2b)

# create function for the 2nd order polynomial
# that can be used with integrate function
sop2 <- function(newVals) {
  #y = c + bx + ax^2
  res <- sopCoefsB[1] + (sopCoefsB[2] * newVals) + (sopCoefsB[3] * newVals^2)
  return(res)
}

allPos2 <- integrate(sop2, lower = 0, upper = 48.5)
yr1Pos2 <- integrate(sop2, lower = 0, upper = 12)

pIgG2 <- yr1Pos2$value/allPos2$value
