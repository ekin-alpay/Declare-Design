##title: "PS531-Final: Declare Design"
##author: "Ekin Alpay"


##Pulling the libraries: 
library(DeclareDesign)
library(tidyverse)
library(fabricatr)

##Defining the model and research design, which is an experimental design here:
intol_exp <- 
  declare_model(N = 300, U = rnorm(N),
                potential_outcomes(Y ~ 0.25 * Z + U)) + #Defines a model with 300 sample size, where U is a normally distributed random variable and potential outcomes Y are dependent on the treatment Z and U.
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +  #Specifies the average treatment effect (ATE). 
  declare_assignment(Z = complete_ra(N, prob = 0.5)) + #Randomly assigns the treatment Z with a probability of 0.5. 
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +  #Measures the outcome Y based on the treatment assignment.
  declare_estimator(Y ~ Z, model = difference_in_means, inquiry = "ATE")+ #Estimates the treatment effect using the difference-in-means method and links it to the inquiry ATE.
  declare_test(Y~Z, model = lm_robust, model_summary = glance) #Tests the relationship using a robust linear model.



##Simulating the design multip times: 
draw_data(intol_exp)  
draw_estimands(intol_exp)
draw_estimates(intol_exp)
run_design(intol_exp)
simulation_exp <- simulate_design(intol_exp) #Runs the simulation and stores the results in simulation_exp.
head(simulation_exp)

##draw_data, draw_estimands, draw_estimates, run_design: Simulates the design multiple times and extracts data, estimands, and estimates.

##Diagnosands: Specifies metrics to evaluate the design: bias, root mean square error (RMSE), power, false positive rate, and performs a t-test.
study_diagnosands <-
  declare_diagnosands(
    bias = mean(estimate - estimand),
    rmse = sqrt(mean((estimate - estimand) ^ 2)),
    power = mean(p.value <= 0.05),
    false_positive_rate = mean(p.value <= 0.05),
    ttest = t.test(simulation_exp)
  )


##Design Diagnosis: Diagnoses the design using the specified diagnosands. Provides an option to diagnose false positive rate separately.

if (FALSE) {
  diagnosis <- diagnose_design(
    design = simulation_exp,
    diagnosands = declare_diagnosands(
      false_positive_rate = mean(p.value <= 0.05))
  )
}

diagnose_design(simulation_exp, diagnosands = study_diagnosands) 

##Redesign and Diagnose: Redesigns the experiment with varying sample sizes and diagnoses each design.
 
designs <- redesign(intol_exp, N = c(100, 200, 300, 400, 500))
diagnose_design(designs)

##script redesigns the experiment with varying sample sizes (100, 200, 300, 400, 500) to evaluate how sample size impacts the design's 
##performance. This step ensures the findings are robust across different scales, identifies the optimal sample size for balancing 
##precision and resource constraints, and diagnoses potential biases or errors specific to certain sample sizes. This helps in optimizing 
##the experimental design for practical implementation and ensuring reliable, consistent results.


##Plotting: Generates a histogram of the estimates with a vertical line indicating the estimand. 

ggplot(simulation_exp) +
  geom_histogram(aes(estimate), bins = 40, fill = "#72B4F3")+
  geom_vline(data = simulation_exp,
             aes(xintercept = estimand),
             lty = "dashed", color = "#C6227F", label= "Estimand")+  
  annotate("text", y = 80, x = 0.24, label = "Estimand",
           color = "#C6227F", hjust = 1)+
  labs(x = "Estimate", y = "Count of simulations")
