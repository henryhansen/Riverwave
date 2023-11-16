
# load data and libraries -------------------------------------------------
source("helper_functions.R") #access functions for working with SMHI Data
source("bayes_flood_plots")
library(rstan) # Load bayesian analysis library - no stan coding necessary
library(brms) # Load bayesian analysis library - no stan coding necessary
library(cowplot) # Load library for combining plots
library(tidybayes) # Extract results from model fits


###########################################################################
# 3. Start Bayesian Workflow ----------------------------------------------
###########################################################################

# specify priors ----------------------------------------------------------
# look at reference image in references/Sundborg-plot.png
# Throughout the year, the highest maximum value was around 1300 m3/sec with
# average flooding events occurring around 650  m3/sec
# let's create a prior that matches this (informative) and a weaker version

# the goal here is to solve for mu and sigma given our plots characteristics
# the mean is easy, based on the plot it is right around 650
# if we assume the max value that Sundberg reported is 3 SD away from the mean
# we should expect the following: 650 = 3*SD; 216.66 = SD; 46941.56 = variance

mean <- 650
variance <- 46941.56

# for the math part, we know the mean is E(x) = exp(mu + 0.5*sigma^2), therefore if
# we rearrange the equation, we get: log(E(x)) = mu + 0.5*sigma^2 = log(650)
# now we plug this into the variance equation
# which is: var(x) = exp(2*mu + sigma^2)*(exp(sigma^2)-1) but to simplify things
# let's take the log form
# log(var(x)) = 2*mu + sigma^2*(exp(sigma^2)-1) = log(46941.56)
# notice the first part is almost the same as our first equation, so we multiply
# by 2 so we can substitute them -> 2*mu +sigma^2 = 2*log(650)
# the eqn is now: log(exp(sigma^2)-1)+2*log(650)=log(46941.56)
# solving for sigma we get

sigma <- sqrt(log(variance / mean ^ 2 + 1))
sigma

# and plugging in sigma into our first equation, mu becomes
mu <- log(mean) - sigma ^ 2 / 2
mu

# now plot the resulting distribution
curve(dlnorm(x, meanlog=mu, sdlog=sigma), from=0, to=1500)

# this matches our lognormal perception of Sundberg's plot
# average around 650 with a relatively low probability of 1300
# in a bayesian context we now want to specify a prior distribution for each of
# these values, typically the distribution choice should be a conjugate
# see here if unfamiliar with this- https://en.wikipedia.org/wiki/Conjugate_prior
# in our case a normal distribution for each value is appropriate
# here the strong priors have very small standard deviations on the log scale
strong.mu.prior <- prior(normal(6.42429, 0.01), class = "Intercept")
strong.sigma.prior <- prior(normal(0.32458, 0.01), class = "sigma")

# here the weak priors have 10-fold higher standard deviations on the log scale
weak.mu.prior <- prior(normal(6.42429, 0.1), class = "Intercept")
weak.sigma.prior <- prior(normal(0.32458, 0.1), class = "sigma")

# prior predictive checks -------------------------------------------------

strong.prior.only <- brm(Discharge ~1, data = edbk.f, family = lognormal(),
                         prior = c(strong.mu.prior, strong.sigma.prior),
                         sample_prior = "only",
                         seed = 1234,
)


# fit models --------------------------------------------------------------
# # fit lognormal distribution using a bayesian approach to full dataset

strong.fit.f <- brm(Discharge ~1, data = edbk.f, family = lognormal(),
                    prior = c(strong.mu.prior, strong.sigma.prior))


weak.fit.f <- brm(Discharge ~1, data = edbk.f, family = lognormal(),
                  prior = c(weak.mu.prior, weak.sigma.prior))

def.fit.f <- brm(Discharge ~1, data = edbk.f, family = lognormal())


# # fit lognormal distribution using a bayesian approach to 10 year dataset
strong.fit.10 <- brm(Discharge ~1, data = edbk10.f, family = lognormal(),
                     prior = c(strong.mu.prior, strong.sigma.prior))


weak.fit.10 <- brm(Discharge ~1, data = edbk10.f, family = lognormal(),
                   prior = c(weak.mu.prior, weak.sigma.prior))

def.fit.10 <- brm(Discharge ~1, data = edbk10.f, family = lognormal())

# # fit lognormal distribution using a bayesian approach to 5 year dataset
strong.fit.5 <- brm(Discharge ~1, data = edbk5.f, family = lognormal(),
                    prior = c(strong.mu.prior, strong.sigma.prior))


weak.fit.5 <- brm(Discharge ~1, data = edbk5.f, family = lognormal(),
                  prior = c(weak.mu.prior, weak.sigma.prior))

def.fit.5 <- brm(Discharge ~1, data = edbk5.f, family = lognormal())

# model ouputs
model.outputs <- list(
    strong.fit.f,
    weak.fit.f,
    def.fit.f,
    strong.fit.10,
    weak.fit.10,
    def.fit.10,
    strong.fit.5,
    weak.fit.5,
    def.fit.5
)

# show fit for each model
lapply(model.outputs, plot)

# show summary for each model
lapply(model.outputs, summary)

# save model estimates
model.ests <-  brms.fit2post(model.outputs)

# export plot
example_mcmc_fit <- plot(model.outputs[[1]])

# Open tiff device
png("plots/example_mcmc_fit.png",
    width = 500,
    height = 500,
    units = "px")

# Make  plot
example_mcmc_fit

# Close device
dev.off()


# # # Sensitivity Analysis  ---------------------------------------------------

# let's setup a range of prior values for sigma and apply it to the weak 10 model
# for reference here was the original prior - prior(normal(0.32458, 0.1), class = "sigma")

# we specified 10 new sigmas from our very strong prior to our weak prior
sensi_sigmas <- seq(0.01,0.1,length.out=10)
sensi_fits <- list()

# a brute force way to do this is to rerun the model with each prior
for (i in 1:length(sensi_sigmas)) {
    s <- sensi_sigmas[i]
    sensi_prior <- do.call("prior",
                           list(prior = call("normal", 0.32458, sensi_sigmas[i]), class = "sigma"))
    print(sensi_prior)
    sensi_fits[[i]] <- brm(Discharge ~1, data = edbk10.f, family = lognormal(),
                           prior = c(weak.mu.prior, sensi_prior))
}

# we can explore results
# show fit for each model
lapply(sensi_fits, plot)

# show summary for each model
lapply(sensi_fits, summary)

# extract sigma posterior information
out <- data.frame()
for (i in 1:length(sensi_fits)) {
    sensi_fits[[i]] %>%
        gather_rvars(b_Intercept, sigma) %>%
        median_qi(.value) -> model
    print(model)
    out <- rbind(out,model)
}

out$prior_value <- rep(sensi_sigmas,each = 2)

sensi_plot <- ggplot(data = out, aes(x = .value, y = prior_value, xmin = .lower, xmax = .upper, color = .variable)) +
    geom_pointinterval() +
    scale_y_continuous(n.breaks = 9, limits = c(0.01,0.1)) +
    theme_minimal() +
    labs(x = "Posterior Median", y = "Prior value on sigma", color = "Parameters") +
    facet_wrap(~.variable, scales = "free")

# Open tiff device
png("plots/sensi_plot.png",
    width = 500,
    height = 500,
    units = "px")

# Make  plot
sensi_plot

# Close device
dev.off()

# # visualize fit -----------------------------------------------------------
# # # example plot resulting distribution and fitted distribution
hist(edbk.f$Discharge, freq = F, breaks = 10)
curve(dlnorm(x, meanlog=as.double(out[1,2]), sdlog=as.double(out[2,2])), from = 0, to = max(edbk.f$Discharge), add = T)

###########################################################################
# 4. Flood Freq. Calculations ----------------------------------------------
###########################################################################
ggc <- 0.5
# test exceedence function
test <- excd(edbk.f, "Discharge", ggc)
# See exceedence plot
plot(test$TpEst,test$Discharge)

# test exceedence function
test10 <- excd(edbk10.f, "Discharge", ggc)
# See exceedence plot
plot(test10$TpEst,test10$Discharge)

# test exceedence function
test5 <- excd(edbk5.f, "Discharge", ggc)
# See exceedence plot
plot(test5$TpEst,test5$Discharge)

# create exceedance dataframe with each test
smhi <- data.frame(facet = c(rep("Full Dataset", nrow(test)),rep("10 year", nrow(test10)), rep("5 Year", nrow(test5))),
                   Tpest = c(test$TpEst,test10$TpEst,test5$TpEst),
                   Discharge = c(test$Discharge, test10$Discharge, test5$Discharge))

smhi$facet <- factor(smhi$facet, levels = c("Full Dataset", "10 year", "5 Year"))

# specify return intervals
ReturnInterval <- c(1.0101,2,5,10,15,25,30,35,40,45,50,60,70,80,90,100,150,200)

# calculate exceedence probabilities
excProb <- 1/ReturnInterval

# use frequency calculation and fitted model estimates to retrieve calculated discharges
all_models <- data.frame(
    ReturnInterval = ReturnInterval,
    ExceedenceProbability = excProb,
    #fitted models
    full_data.strong_prior = exp(unlist(sapply(excProb, FF_LogNormal, m = model.ests[1,1], s = model.ests[1,2]))),
    full_data.weak_prior = exp(unlist(sapply(excProb, FF_LogNormal, m = model.ests[2,1], s = model.ests[2,2]))),
    full_data.no_prior = exp(unlist(sapply(excProb, FF_LogNormal, m = model.ests[3,1], s = model.ests[3,2]))),
    teny_data.strong_prior = exp(unlist(sapply(excProb, FF_LogNormal, m = model.ests[4,1], s = model.ests[4,2]))),
    teny_data.weak_prior = exp(unlist(sapply(excProb, FF_LogNormal, m = model.ests[5,1], s = model.ests[5,2]))),
    teny_data.no_prior = exp(unlist(sapply(excProb, FF_LogNormal, m = model.ests[6,1], s = model.ests[6,2]))),
    fivey_data.strong_prior = exp(unlist(sapply(excProb, FF_LogNormal, m = model.ests[7,1], s = model.ests[7,2]))),
    fivey_data.weak_prior = exp(unlist(sapply(excProb, FF_LogNormal, m = model.ests[8,1], s = model.ests[8,2]))),
    fivey_data.no_prior = exp(unlist(sapply(excProb, FF_LogNormal, m = model.ests[9,1], s = model.ests[9,2])))
)

#make long format
all_models_long <- gather(all_models, key = "model", value = "Discharge", -c(ReturnInterval, ExceedenceProbability))
all_models_long$Prior.Strength <- rep(rep(c("Strong", "Weak", "Uninformative"), each = 18),3)
all_models_long$Data.set <- rep(c("Full Dataset", "10 year", "5 Year"), each = 54)
all_models_long$facet <- factor(all_models_long$Data.set, levels = c("Full Dataset", "10 year", "5 Year"))
# plot all results

ff_plot <- ggplot(data = all_models_long, aes(x = ReturnInterval, y = Discharge)) +
    geom_point(aes(color = Prior.Strength)) +
    geom_point(data = smhi, aes(x = Tpest, y = Discharge, alpha = "Original Data"), shape = 4) +
    theme_minimal() +
    labs(x = "Return Interval (y)", y = bquote("Discharge"(m^3/s)), color = "Prior Strength", alpha ="") +
    scale_alpha_manual(name = "", values = c(0.95), label = c("Original Data")) +
    facet_wrap(~facet,ncol=1)

ff_plot

# Open tiff device
png("plots/ff_plot.png",
    width = 400,
    height = 800,
    units = "px")

# Make  plot
ff_plot

# Close device
dev.off()
