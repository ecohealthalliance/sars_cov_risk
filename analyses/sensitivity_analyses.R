# sensitivity analyses

source(here("R/modifyFacets.R"))
source(here("R/setDistributions.R"))

# latin hypercube sampling------------------------------------------------------

# number of unknown parameters to vary
upars <- 4

set.seed(26)

# set number of reps
# has to be high so that sobol indices are all > 1
samples <- 400000

# construct random Latin hypercube design
lhssample <- randomLHS(samples, upars)

colnames(lhssample) <- c("people", "contact", "detection", "pastyear")

# transform lhs samples on uniform distribution into desired distributions
peopleSample <- qnorm(lhssample[, "people"], mean = 477589486, sd = 15919650)
contactSample <- qbeta(lhssample[, "contact"], shape1 = contactShape1, 
                       shape2 = contactShape2)
detectSample <- qbeta(lhssample[, "detection"], shape1 = detectShape1, 
                       shape2 = detectShape2)
# last param is also uniform, just needs to be on different scale
pastyear_min <- pIgG1
pastyear_max <- pIgG2
pastyearSample <- (pastyear_max - pastyear_min)*lhssample[, "pastyear"] + 
  pastyear_min

# vector for output
infected <- rep(NA, samples)

# loop that calculates output (total infected) for each set of inputs
for(i in 1:samples){
  
  # values for lhs parameters
  nPeople <- peopleSample[i]
  pContact <- contactSample[i]
  pDetect <- detectSample[i]
  pPastyear <- pastyearSample[i]
  
  # calculate total infected
  infected[i] <- ceiling(nPeople*pContact*pDetect*pPastyear)
}

# make data frame of inputs and output
lhsdata <- data.frame(peopleSample, contactSample, detectSample, pastyearSample, 
                      infected)
names(lhsdata) <- c("nPeople", "pContact", "pDetect", "pPastyear", "totInf")

# also create a df without the output 
inputs <- lhsdata[, -ncol(lhsdata)]


# plot all histograms
# tiff("figures/FigS3.tif", height = 4, width = 8, units = "in", res = 300)
par(mfrow = c(1, 5))
hist(peopleSample, breaks = 20, main = "Npeople")
hist(contactSample, breaks = 20, main = "Pcontact")
hist(detectSample, breaks = 20, main = "Pdetect")
hist(pastyearSample, breaks = 20, main = "Ppastyear")
hist(infected, breaks = 20, main = "Total infected")
# dev.off()

# calculate Sobol indices-------------------------------------------------------

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

# Fig. S4-----------------------------------------------------------------------

# plot 1: LHS inputs versus output
lhsLong <- lhsdata %>% 
  pivot_longer(nPeople:pPastyear, names_to = "inputVar", values_to = "value")

# facet labels
inputLabs <- c("Npeople (millions)", "Pcontact", "Pdetect", "Ppastyear")
names(inputLabs) <- c("nPeople", "pContact", "pDetect", "pPastyear")

ggplot(data = lhsLong) +
  geom_point(aes(x = value, y = totInf), 
             color = "blue", alpha = 0.1) +
  facet_wrap_custom(~inputVar, nrow = 1, scales = "free_x", 
                    labeller = labeller(inputVar = inputLabs),
                    scale_overrides = list(
                      scale_override(1, scale_x_continuous(
                        breaks = seq(4.2e8, 5.4e8, length.out = 7), 
                        labels = seq(4.2e8, 5.4e8, length.out = 7)/1e6)))) +
  scale_y_continuous(name = "Total people infected in 1 year (millions)",
                     breaks = seq(0, 3.6e7, length.out = 10),
                     labels = seq(0, 3.6e7, length.out = 10)/1e6) +
  theme_bw() +
  theme(axis.text = element_text(color = "black"),
        axis.title.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) -> pS4A


# plot 2: sobol indices and CIs
sobolIndices <- bind_rows(
  foTable %>%
    mutate(var = c("X1", "X2", "X3", "X4"),
           order = "First-order index"),
  toTable %>%
    mutate(var = c("X1", "X2", "X3", "X4"),
           order = "Total-order index")
)

ggplot(data = sobolIndices) +
  geom_point(aes(x = var, y = original), size = 2) +
  geom_errorbar(aes(x = var, ymin = `min. c.i.`, ymax = `max. c.i.`), 
                width = 0.2, size = 0.8) +
  scale_x_discrete(name = "", labels = c("Npeople", "Pcontact", "Pdetect",
                                         "Ppastyear")) +
  scale_y_continuous(name = "Sensitivity index value", limits = c(0, 1)) +
  facet_wrap(~order, nrow = 1) +
  theme_bw() +
  theme(axis.text = element_text(color = "black")) -> pS4B

# plot together. slow because of number of points
#tiff("figures/FigS4.tif", width = 8.5, height = 8, units = "in", res = 300)
grid.arrange(pS4A, pS4B, nrow = 2)
# dev.off()

# pull out summary stats
range(infected)
mean(infected)
median(infected)
