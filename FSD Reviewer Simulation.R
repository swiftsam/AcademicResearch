## FSD Reviewer Simulation

# Let us suppose that a student's performance is determined by a latent variable (like IQ), which I'll call q. 
# Let's randomly generate 20,000 students with a reasonable IQ distribution for the students in the authors' sample:
  
q <- rnorm(20000, mean = 120, sd = 10)

# Let's also suppose that there are 100 different undergraduate institutions that these students might attend, 
# and that these institutions have a selectivity characteristic which I'll call s and put on the same scale as q:
  
s <- rnorm(100, mean = 120, sd = 10)

# Let's say that students are assigned to schools as follows: a student's latent intelligence is perturbed 
# by random noise, and then the student is randomly assigned to school with a selectivity characteristic +/- 20 
# points from there. (This models that we can't measure the latent q variable perfectly, and that admissions 
# decisions also include other factors.) Let i be the institution for each student
i <- sapply(q + rnorm(length(q), sd = 15), function(x) { 
  y <- which(abs(s - x) < 20)
  if( length(y) > 0 ) i <- y[sample(length(y), 1)]
  else i <- sample(length(s), 1) # if no close matches, pick at random
  i
})  
  
# Now let's turn to GPA, and let's assume for sake of argument that GPA is in fact a noisy indicator 
# of the latent intelligence variable, and that while grade inflation might exist, it does not differ 
# across institutions. Let g be a student's GPA, rescaled to the 0-4 range and truncated at 4

g <- scale(q + rnorm(length(q), sd = 15))
g <- g/2 + 3.25
g[g > 4] <- 4

# We'll now compute the average GPA per institution and each student's relative GPA:

g.by.i <- aggregate(g ~ i, cbind(g,i), mean)[,2]
g.rel <- g - g.by.i[i]

# Finally, to model admissions decisions, let's say that the students are subject to a separate evaluation, 
# and that it is in fact a function of the latent intelligence dimension, again perturbed by random error. Call this e:
e <- q + rnorm(length(q), sd = 15)

# Finally, let's see what happens if we predict this judgment from each student's relative GPA, 
# the average GPA of their institution, and the institution's selectivity. This is similar to the authors'
# model that includes relative and average GPA as well as undergraduate institution quality. 

summary(lm(scale(e) ~ scale(g.rel) + scale(g.by.i[i]) + scale(s[i]) - 1))

# I replicated the above simulation 1000 times, and here the median and 95% confidence intervals for the standardized regression coefficients:

# relative GPA .266 [.252, .280]
# average GPA .065 [.023, .107]
# selectivity .147 [.102, .193]