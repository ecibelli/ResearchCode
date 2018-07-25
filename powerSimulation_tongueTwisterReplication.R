### ............................................................ ###
###                                                              ###
### Power simulation code for tongue twister replication project ###
###           Gia Eapen, Emily Cibelli, Matt Goldrick            ###
###                                                              ###
### ............................................................ ###

## Description: This script simulates a new data set based on the estimates of a model reported in McMillan and Corley (2010). The simulation re-runs the model on the simulated data set (and resamples, and re-fits many times) in order to estimate the power needed to replicate the original finding.

## Specific goal: how many subjects are necessary to replicate the place*voice interaction in experiment 1 (the VOT analysis) of that paper?

## McMillan, C. T., & Corley, M. (2010). Cascading influences on the production of speech: 
## Evidence from articulation. Cognition, 117(3), 243-260.

## ....

## Written by Emily Cibelli (emily.cibelli@northwestern.edu)
## Last updated: July 25, 2018 (expanded explanations, cleaned up ordering of elements)

## Method/code for simulation adapted from Greg Snow: 
## https://stat.ethz.ch/pipermail/r-sig-mixed-models/2009q1/001790.html

## Section 0: Description of experiment and background -----------------------------------

# McMillan and Corley (2010) ran a tongue-twister study to estimate how much variance in VOT (voice onset time, a duration parameter that distinguishes consonants like "k" and "g") is observed when people produce tongue twisters with alternating onset consonants, e.g. 

# "kef gef gef kef"

# Of interest was whether dissimilar stops become more similar to one another in VOT in these twister conditions. Specifically, was there was a difference in the magnitude of VOT deviance for changes in place of articulation (the location in the mouth where a consonant is articulated e.g. "k" to "t" or "g" to "d") as opposed to voicing (the major dimension measured by VOT, e.g. "k" vs. "g"), and whether these two interact (voicing and place change, e.g. "k" vs. "d").

# They recorded seven speakers producing 60 of these twisters, and also recorded four control trials (e.g. "kef kef kef kef"), with no change in initial consonant.

# Our goal was to assess how many speakers would be needed to replicate this effect, as part of a larger project designed to assess what the appropriate baseline/control condition for tongue twisters is. More detail on the motivation for the project, as well as details on planned analyses, can be found here:

# https://osf.io/zcr9p/

# Note that the power simulation code here is changed from the one reported at the OSF repository to add more detail and clarification; the basic structure remains the same.

## ........................................................................................

##### Section 1: Set starting seed values for simulation variables ------------------

# Structure of original model:
# VOTdeviance ~ place * voice + (1|subject)

nSubj = 10 		  # Number of subjects
nTwister = 60	  # Number of tongue twister trials
nControl = 4	  # Number of non-twister control trials
b0 = 10.84 		  # Intercept (these and all betas/t-stats are reported in McM & C 2010)
bPlace= 0.01	  # Main effect (beta) of place 
tPlace = 0.02   # Place t-stat
bVoice = 1.16   # Main effect (beta) of voice
tVoice= 2.18    # Voice t-stat
bPV= -2.64 		  # Interaction beta 
tPV= 2.02       # Interaction t-stat
Vsubj=4			    # By-subject random error. **
Vtwister=1		  # By-item random error **
Verror=90		    # Residual error *

# * See section 2 for notes on selecting random effects error and residual error.
# ** Within-subject variance is reported in Snow's code (as well as similar code from Ben Bolker) as being 4x item variance.

##### Section 2: A note on seelcting the random effects/residual error --------------------

# Note: The full random effect/residual error values were not reported in McM+C 2010. To get a rough approximation of them, I generated data sets with varying by-subject, by-item, and residual error values (Vsubj, Vtwister, and Verror above).

# This is an imperfect procedure, but provides one option when attempting to estimate power from an existing study with incomplete information. (This situation is not uncommon, as the details of the random effects variance are - rightly or wrongly -  often not considered to have much interpretive interest for understanding a model's findings.)

# The goal was to replicate the values reported in McM+C Table 1, by introducing enough error (by-item, by-subject, and residual) to get the grand means and standard deviations of VOT deviance within range of the reported magnitudes. That original table:

#					Place of articulation
# Voicing			No change				Change
#				    	M     (SD)		  M		  (SD)
# ...................................................
# No change		9.09	(8.78)		10.56	(9.77)
# Change			12.21	(12.00)		11.06	(10.87)

# The values selected for Vsubj, Vtwister, Verror are those that got as close as possible to this table *in magnitude*, if not in differences by condition. 

# To test this procedure, generate the "data" object internal to "sim1" in section 3 below, with different values for Vsubj, Vtwister, and Verror. (Note that Vsubj is typically reported as 4 times Vtwister.) Uncomment and run the following two lines to assess the effects of changing these variables on the magnitude of VOT deviance.

# with(data, aggregate(vot, list(place, voice), mean)) # Use this to test the magnitude of Vsubj/Vtwister
# with(data, aggregate(vot, list(place, voice), sd)) # Playing around with Verror/eps seems to matter most here

##### Section 3: Function to run simulation -----------------------------------

# The function sim1 creates a simulated data set based on a set of variables (the number of subjects, number of twister and control trials, the estimates/betas for place of articulation, voicing, and place*voice interaction, the intercept, and the random effects and residual error).

# For each iteration/simulation, it creates a data frame to hold one data point for each trial for each speaker. It samples from the estimates of place, voicing, and interaction distributions, as well as the random effects and residual error, to create vectors of estimates of each effect for each data point. These are added together to generate a vector of the dependent variable (VOT deviance) for each data point. Finally, the model is fit to this newly-simulated data twice - once without the critical effect of interest (the place * voicing interaction), and once with it. The two models are compared to see if the inclusion of the critical effect significantly improves model fit for that particular simulation run.

# The crucial concept here is the idea of sampling from uncertain estimates. As with any model, we do not know the true effect of place or voicing, nor do we have a perfect estimate of by-subject or by-item variance. Each simulation, we are sampling new values from these distributions to capture this uncertainty. Of interest is whether the effects we think are true really hold, even with some noise in the estimates, and if they do, how many speakers we need to reliably observe them again.

sim1 <- function(nSubj = 0, nTwister = 0, nControl = 0, 
                 bPlace=0, bVoice=0, bPV=0, b0=0, 
                 Vsubj=0, Vtwister=0, Verror=0) {

# Generate a data frame for simulated data for each trial and subject
numReps = 1:4					  # 4 repetitions of the tongue twister per trial
wordCount = 1:4					# 4 words per tongue twister
subj = 1:nSubj
trials = 1:(nTwister+nControl)
data = expand.grid(trials, numReps, wordCount, subj)
colnames(data) = c("trialNum", "numReps", "wordCount", "subj")

# Add information about place of articulation and voicing for each trial
data$place = ""
data$voice = ""
# 1/3 of trials contrast in voicing ("t" vs. "d", "k" vs. "g")
data[data$trialNum %in% c(1:20),]$place = "noChange" 
data[data$trialNum %in% c(1:20),]$voice = "change"
# 1/3 of trials contrast in place of articulation ("d" vs. "g", "t", vs. "k")
data[data$trialNum %in% c(21:40),]$place = "change"
data[data$trialNum %in% c(21:40),]$voice = "noChange"
# 1/3 of trials contrast in both place and voicing ("t" vs. "g", "k" vs. "d")
data[data$trialNum %in% c(41:60),]$place = "change"
data[data$trialNum %in% c(41:60),]$voice = "change"
# 4 control trials have no consonant change (e.g. "kef kef kef kef")
data[data$trialNum %in% c(61:64),]$voice = "noChange"
data[data$trialNum %in% c(61:64),]$place = "noChange"
data$place = as.factor(as.character(data$place))
data$voice = as.factor(as.character(data$voice))

# Contrast coding for predictors
data$cPlace = ifelse(data$place == "change", 0.5, -0.5)
data$cVoice = ifelse(data$voice == "change", 0.5, -0.5)

# Generate random effects distributions
S.re <- rnorm(nSubj, 0, sqrt(Vsubj))
T.re <- rnorm(nTwister+nControl, 0, sqrt(Vtwister))
eps <- rnorm(nrow(data), 0, sqrt(Verror)) # Residual error

# Calculate standard errors for effects based on values reported in McM & C (2010)
# se = beta / t
sePlace = bPlace / tPlace
seVoice = bVoice / tVoice
sePV = bPV/tPV

# Generate distribution of values for each fixed effect estimate, by sampling from a distribution centered around the estimate, with the standard errors just calculated. In each simulated run, we sample 1 point from 95% CI range of values for each data point.
# (Note: at N > 300, the t-distribution approximates the normal distribution.)

bPlaceSamp = runif(1, (bPlace - qnorm(p = 0.95) * sePlace), 
                   (bPlace + qnorm(p = 0.95) * sePlace))
bVoiceSamp = runif(1, (bVoice - qnorm(p = 0.95) * seVoice), 
                   (bVoice + qnorm(p = 0.95) * seVoice))
# Interaction: negative beta, so order reversed
bPVSamp = runif(1, (bPV + qnorm(p = 0.95) * sePV), 
                (bPV - qnorm(p = 0.95) * sePV)) 

# Create separate vectors for each predictor to give as input to the equation to calculate VOT (dependent variable)
cVoice = data$cVoice
cPlace = data$cPlace
subject = data$subj
twister = data$trialNum

# Generate VOT deviance vector (dependent variable) as a linear combination of the intercept, estimate, random effects, and residual error
data$vot = b0 +                   # Intercept
  bPlaceSamp*cPlace +             # Main effect of place
  bVoiceSamp*cVoice +             # Main effect of voice
	bPVSamp*cPlace*cVoice +         # Interaction of place and voice
	S.re[subject] +                 # Subject random intercept 
	T.re[twister] + 	              # Item random intercept 
	eps                             # Residual error


# Check for convergence errors
# Unlikely in this model (only 2 random intercepts), but worth noting if they do occur
catchWarning1 = tryCatch(lmer(vot ~ cPlace + cVoice + (1|subject) + (1 |twister), 
                              data=data, REML = F),warning = function(w)w)
catchWarning2 = tryCatch(lmer(vot ~ cPlace * cVoice + (1|subject) + (1|twister), 
                              data=data, REML = F),warning = function(w)w)
	
# Print convergence warnings, if any
if (is(catchWarning1,"warning")){  
  repNo = getWinProgressBar(pb)
  print(sprintf("Convergence warning in fit 1 rep %s", repNo))
  flush.console()
  
  } else if (is(catchWarning2,"warning")){ 
    repNo = getWinProgressBar(pb)
    print(sprintf("Convergence warning in fit 2 rep %s", repNo))
    flush.console()

  # If no convergence warnings, run model fits  
  } else { 
  # Fit model 1: no interaction between place and voice	
  fit1 <- lmer(vot ~ cPlace + cVoice + (1|subject) + (1|twister), data=data, REML = F)
  
  # Fit model 2: interaction between place and voice
  fit2 <- lmer(vot ~ cPlace * cVoice + (1|subject) + (1|twister), data=data, REML = F)
  
  # Compare the two models, pull out p-value of chi-sq test
  # Does model 2 significantly improve model fit (suggesting a significant interaction)?
  anova(fit2,fit1)[2,8]	
  }
} # End sim1 function

##### Section 4: Run simulations  --------------------------------------------------

library(lme4)

# Set number of subjects and simulations
# Change nSubj to assess power with different sample sizes
nSubj = 5
nSamp = 1000 # Number of simulations to run (10-100 to test, 1000-10000 to run)

# For Windows only, progress bar to keep track of how many loops/simulations have run
pb <- winProgressBar(max=nSamp)
setWinProgressBar(pb, 0)

# Run simulations nSamp number of times
# Give sim1 the values specified in Section 1
out1 <- replicate(nSamp, {setWinProgressBar(pb, getWinProgressBar(pb)+1);
				sim1(nSubj = nSubj, nTwister = nTwister, nControl = nControl,
				     b0 = b0, bPlace=bPlace,bVoice=bVoice, bPV=bPV,
				     Vsubj=Vsubj, Vtwister=Vtwister, Verror=Verror)}) # see recommended values above

close(pb)				

# If there were convergence errors, remove those reps from out1 before taking mean or plotting the distribution (the relevant reps will be printed on screen)

# Plot the results - what was the distribution of p-values for the chisq test comparing the fits with and without the interaction for each simulation?
hist(out1)

# Power calculation:
# In how many simulations did the interaction significantly improve model fit?
mean( out1 < 0.05 )

# ------------------------------------------

#### Power estimates, 1/25/2017:

## 1000 simulations
# power, N = 5: 0.691 (no convergence errors)
# power, N = 10: 0.741 (no convergence errors)
# power, N = 15: 0.807 (no convergence errors)
# power, N = 20: 0.765 (no convergence errors)
# power, N = 25: 0.809 (no convergence errors)
# power, N = 30: 0.786 (no convergence errors)
