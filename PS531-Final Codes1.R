##title: "PS531-Final"
##author: "Ekin Alpay"

library(DeclareDesign)
library(tidyverse)
library(fabricatr)


intol_exp <- 
  declare_model(N = 300, U = rnorm(N),
                potential_outcomes(Y ~ 0.25 * Z + U)) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(Z = complete_ra(N, prob = 0.5)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, model = difference_in_means, inquiry = "ATE")+
  declare_test(Y~Z, model = lm_robust, model_summary = glance)

##Simulating the design multip times: 
draw_data(intol_exp)  
draw_estimands(intol_exp)
draw_estimates(intol_exp)
run_design(intol_exp)
simulation_exp <- simulate_design(intol_exp)
head(simulation_exp)


study_diagnosands <-
  declare_diagnosands(
    bias = mean(estimate - estimand),
    rmse = sqrt(mean((estimate - estimand) ^ 2)),
    power = mean(p.value <= 0.05),
    false_positive_rate = mean(p.value <= 0.05),
    #fwer = pairwise.t.test(data= simulation_exp$estimate, simulation_exp$estimator ,p.adjust.method="bonferroni"
    #I tried to add the FWER, I think it should have looked like this, but I cannot figure out what should be the "g" argument, I     tried all but R does not accept. 
    ttest = t.test(simulation_exp)
  )


##I am not sure what this codes here does but most of the examples included this. What I did was I added the false positive rate to the diagnosands function in general but this code has it as well. 

if (FALSE) {
  diagnosis <- diagnose_design(
    design = simulation_exp,
    diagnosands = declare_diagnosands(
      false_positive_rate = mean(p.value <= 0.05))
  )
}


diagnose_design(simulation_exp, diagnosands = study_diagnosands) 

designs <- redesign(intol_exp, N = c(100, 200, 300, 400, 500))
diagnose_design(designs)



##Graph for Question 14: 
ggplot(simulation_exp) +
  geom_histogram(aes(estimate), bins = 40, fill = "#72B4F3")+
  geom_vline(data = simulation_exp,
             aes(xintercept = estimand),
             lty = "dashed", color = "#C6227F", label= "Estimand")+  
  annotate("text", y = 80, x = 0.24, label = "Estimand",
           color = "#C6227F", hjust = 1)+
  labs(x = "Estimate", y = "Count of simulations")