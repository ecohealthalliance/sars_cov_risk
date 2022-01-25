# estimate spillover and perform sensitivity analyses

source(here("R/setDistributions.R"))
source(here("R/estimateSpillover.R"))

samples <- 400000
load(here("data/countsOverall.rda"))
popMean <- countsOverall$people_WPCount

# estimate spillover------------------------------------------------------------

lhsdata <- estimateSpillover(upars = 4, samples = samples, popMean = popMean,
                             contactDistr = fitBetaContact, 
                             detectDistr = fitBetaDetect, pastyear_min = pIgG1, 
                             pastyear_max = pIgG2)

# pull out summary stats on total infected
ceiling(summary(lhsdata$totInf))

# calculate 95% CI around median
CI1 <- MedianCI(lhsdata$totInf, conf.level = 0.95, method = "boot", R = 1000)

# Save to data folder
usethis::use_data(lhsdata, overwrite = TRUE)

# re-run analysis, removing 3 highest estimates of human-bat contact prev.------
lhs_adjContact <- estimateSpillover(upars = 4, samples = samples, 
                                    popMean = popMean,
                                    contactDistr = fitBetaContact_B, 
                                    detectDistr = fitBetaDetect, 
                                    pastyear_min = pIgG1, 
                                    pastyear_max = pIgG2)

# pull out summary stats on total infected
ceiling(summary(lhs_adjContact$totInf))

# calculate 95% CI around median
CI2 <- MedianCI(lhs_adjContact$totInf, conf.level = 0.95, method = "boot", 
                R = 1000)

# Save to data folder
usethis::use_data(lhs_adjContact, overwrite = TRUE)

# re-run analysis, removing highest estimate of seropositivity------------------
lhs_adjDetect <- estimateSpillover(upars = 4, samples = samples, 
                                   popMean = popMean, 
                                   contactDistr = fitBetaContact, 
                                   detectDistr = fitBetaDetect_B, 
                                   pastyear_min = pIgG1, 
                                   pastyear_max = pIgG2)

# pull out summary stats on total infected
ceiling(summary(lhs_adjDetect$totInf))

# calculate 95% CI around median
CI3 <- MedianCI(lhs_adjDetect$totInf, conf.level = 0.95, method = "boot", 
                R = 1000)

# Save to data folder
usethis::use_data(lhs_adjDetect, overwrite = TRUE)

# re-run analysis, removing highest estimates of contact AND seropositivity-----
lhs_adjContactDetect <- estimateSpillover(upars = 4, samples = samples, 
                                          popMean = popMean,
                                          contactDistr = fitBetaContact_B, 
                                          detectDistr = fitBetaDetect_B, 
                                          pastyear_min = pIgG1, 
                                          pastyear_max = pIgG2)

# pull out summary stats on total infected
ceiling(summary(lhs_adjContactDetect$totInf))

# calculate 95% CI around median
CI4 <- MedianCI(lhs_adjContactDetect$totInf, conf.level = 0.95, method = "boot", 
         R = 1000)

# Save to data folder
usethis::use_data(lhs_adjContactDetect, overwrite = TRUE)

# calculate Sobol indices using original input params---------------------------

# create a df without the output 
inputs <- lhsdata[, -ncol(lhsdata)]

# needs two random samples
X1 <- inputs[1:(samples/2), ]
X2 <- inputs[(samples/2 + 1):samples, ]

# function to feed into sobolEff
myfunction <- function(df){
  y <- ceiling(df[, 1] * df[, 2] * df[, 3] * df[, 4])
}

# first order Sobol index expresses the amount of variance of y (output)
  # explained by explanatory variable x
# 2nd order expresses the amount of variance of Y explained by the interaction
  # of factors x1 and x2 
# etc for higher level orders
# total sensitivity index accounts for all contributions to output due to x
  # ie first order index plus all interactions

firstOrder <- sobolEff(model = myfunction, X1 = X1, X2 = X2, order = 1, 
                       nboot = 100)
print(firstOrder)

foTable <- firstOrder$S

total <- sobolEff(model = myfunction, X1 = X1, X2 = X2, order = 0, 
                  nboot = 100)
print(total)

toTable <- total$S

# combine into one tibble
sobolIndices <- bind_rows(
  foTable %>%
    mutate(var = c("X1", "X2", "X3", "X4"),
           order = "First-order index"),
  toTable %>%
    mutate(var = c("X1", "X2", "X3", "X4"),
           order = "Total-order index")
)

# Save to data folder
usethis::use_data(sobolIndices, overwrite = TRUE)
