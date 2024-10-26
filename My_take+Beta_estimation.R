library(dplyr)
library(CircMLE)
library(circular)

wdir <- '/home/thed/Desktop/AnalysisGleb'
setwd(wdir)
birds <- read.csv('data.csv') %>% filter(!is.na(Mean))

before <- birds %>%
  filter(Treatment == 'Before') %>%
  dplyr::select(Mean) %>%
  circular(units='degrees')

after <- birds %>%
  filter(Treatment == 'After') %>%
  dplyr::select(Mean) %>%
  circular(units='degrees')

# First step: checking how the distributions looks like with CircMLE
# It doesn't tell anything specific actually
# It just tells which distributions samples are most likely to came from

mle_before <- circ_mle(before)
mle_after <- circ_mle(after)

# just asking the machine to show my results
View(mle_before$results)
View(mle_after$results)

# checking the rayleygh test vals for both samples
# (just because why not)
rayleigh.test(before)
rayleigh.test(after)

# Apparently both samples look very cool: as close to VM as possible
# Check MWW, Watson-Williams and MANOVA

watson.wheeler.test(append(before, after), birds$Treatment)
watson.williams.test(append(before, after), birds$Treatment) # I've tried but it returns warning so probably not the best idea

# The manova idea is questionable by itself. For details re why it works see: Landler et al., 2022
# https://doi.org/10.1186/s40462-022-00323-8
# Technically it is not supposed to work, therefore the idea is debatable
# Yet you cannot argue with numbers, and Landler et al show very convincing ones

manova(cbind(cos(birds$Mean), sin(birds$Mean)) ~ birds$Treatment) %>%
  summary(intercept=T) 

# everything looks cool so far. Now try the thing Nikita proposed
# or at least I would say so if I understood what he wants

combined <- append(before, after)
p_critical <- watson.wheeler.test(combined, birds$Treatment)$p.value

vm_estimates <- mle.vonmises(combined)
mu <- vm_estimates$mu
kappa <- vm_estimates$kappa

effects_mu <- c(15, 30, 45, 60, 75, 90, 105, 120, 135, 150, 165, 180)
errors = c()
n_iters <- 10000

for (effect in effects_mu) {
  err <- 0
  for (. in 1:n_iters) {
    sample1 <- rvonmises(10, mu=mu, kappa=kappa)
    sample2 <- rvonmises(12, mu=mu+effect, kappa=kappa)
    p_val <- watson.wheeler.test(append(sample1, sample2),
                                 append(rep(1, 10), rep(2, 12)))$p.value
    if (p_val >= p_critical) {
      err = err + 1
    }
  }
  errors = append(errors, err/n_iters)
}

plot(effects_mu, errors)
errors
