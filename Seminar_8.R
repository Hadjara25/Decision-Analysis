library(decisionSupport)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(DiagrammeR)
library(plyr)



## Step 1####

# Generate model as a function

example_decision_function <- function(x, varnames){

  # calculate ex-ante risks: impact of implementation of the interventions ####
  intervention_NonPopInvolvEvent <- chance_event(intervention_NonPopInvolv, 1, 0, n = 1)

  # pre-calculate common random draws for all intervention model runs ####

  # profits from Tropical Livestock Units (TLU)
  TLU <- vv(TLU_no_intervention, var_CV, n_years)
  TLU_profit <- vv(profit_per_TLU, var_CV, n_years)

  # benefits of fruit
  precalc_intervention_fruit_benefits <-
    vv(intervention_fruit_area_ha, var_CV, n_years) *
    vv(intervention_fruit_yield_t_ha, var_CV, n_years) *
    vv(intervention_fruit_profit_USD_t, var_CV, n_years)

  # benefits of vegetables
  precalc_intervention_vegetable_benefits <-
    vv(intervention_vegetable_area_ha, var_CV, n_years) *
    vv(intervention_vegetable_yield_t_ha, var_CV, n_years) *
    vv(intervention_vegetable_profit_USD_t, var_CV, n_years)

  # benefits of rain-fed crops
  precalc_intervention_rainfed_crop_benefits <-
    vv(intervention_rainfed_crop_area_ha, var_CV, n_years) *
    vv(intervention_rainfed_crop_yield_t_ha, var_CV, n_years) *
    vv(intervention_rainfed_crop_profit_USD_t, var_CV, n_years)

  #  Intervention ####

  for (decision_intervention_strips in c(FALSE,TRUE))
  {

    if (decision_intervention_strips)
    {
      intervention_strips <- TRUE
      intervention_strips_PlanningCost <- TRUE
      intervention_strips_cost <- TRUE
    } else
    {
      intervention_strips <- FALSE
      intervention_strips_PlanningCost <- FALSE
      intervention_strips_cost <- FALSE
    }

    if (intervention_NonPopInvolvEvent) {
      intervention_strips <- FALSE
      intervention_strips_cost <- FALSE
    }

    # Costs ####
    if (intervention_strips_cost) {
      cost_intervention_strips <-
        intervention_adaptation_cost +
        intervention_tech_devices_cost +
        intervention_nursery_cost +
        intervention_wells_cost +
        intervention_training_cost +
        intervention_mngmt_oprt_cost +
        intervention_mngmt_follow_cost +
        intervention_mngmt_audit_cost
    } else
      cost_intervention_strips <- 0

    if (intervention_strips_PlanningCost) {
      plan_cost_intervention_strips <-
        intervention_communication_cost + intervention_zoning_cost
    } else
      plan_cost_intervention_strips <- 0

    maintenance_cost <- rep(0, n_years)

    if (intervention_strips)
      maintenance_cost <-
      maintenance_cost + vv(maintenance_intervention_strips,
                            var_CV, n_years)

    intervention_cost <- maintenance_cost
    intervention_cost[1] <-
      intervention_cost[1] +
      cost_intervention_strips +
      plan_cost_intervention_strips


    # Benefits from  cultivation in the intervention strips ####

    intervention_fruit_benefits <-
      as.numeric(intervention_strips) * precalc_intervention_fruit_benefits
    intervention_vegetable_benefits <-
      as.numeric(intervention_strips) * precalc_intervention_vegetable_benefits
    intervention_rainfed_crop_benefits <-
      as.numeric(intervention_strips) * precalc_intervention_rainfed_crop_benefits

    # Total benefits from crop production (agricultural development and riparian zone) ####
    crop_production <-
      intervention_fruit_benefits +
      intervention_vegetable_benefits +
      intervention_rainfed_crop_benefits

    # Benefits from livestock ####
    # The following allows considering that intervention strips may
    # restrict access to the reservoir for livestock.

    if (intervention_strips)
      TLU_intervention <-
      TLU * (1 + change_TLU_intervention_perc / 100)
    else
      TLU_intervention <- TLU

    if (decision_intervention_strips){
      livestock_benefits <- TLU_intervention * TLU_profit
      total_benefits <- crop_production + livestock_benefits
      net_benefits <- total_benefits - intervention_cost
      result_interv <- net_benefits}


    if (!decision_intervention_strips){
      livestock_benefits <- TLU_no_intervention * TLU_profit
      total_benefits <- livestock_benefits
      net_benefits <- total_benefits - intervention_cost
      result_n_interv <- net_benefits}

  } #close intervention loop bracket

  NPV_interv <-
    discount(result_interv, discount_rate, calculate_NPV = TRUE)

  NPV_n_interv <-
    discount(result_n_interv, discount_rate, calculate_NPV = TRUE)

  # Beware, if you do not name your outputs
  # (left-hand side of the equal sign) in the return section,
  # the variables will be called output_1, _2, etc.

  return(list(Interv_NPV = NPV_interv,
              NO_Interv_NPV = NPV_n_interv,
              NPV_decision_do = NPV_interv - NPV_n_interv,
              Cashflow_decision_do = result_interv - result_n_interv))

}

## Step 2####

# input table describing uncertaintiy in our variables

library(readr)

example_input_table = "https://raw.githubusercontent.com/CWWhitney/Decision_Analysis_Course/main/data/example_input_table.csv"

input_table <- read_csv(url(example_input_table))

## Load data

input_table <- read.csv("example_input_table.csv")
input_table

### names
names(input_table)


## Perform Monte Carlo simulations####

mcSimulation_results <- decisionSupport::mcSimulation(
  estimate = decisionSupport::estimate_read_csv("example_input_table.csv"),
  model_function = example_decision_function,
  numberOfModelRuns = 200,
  functionSyntax = "plainNames"
  )


# Model assessment####
# Plot Net Present Value (NPV) distributions
# use plot_distributions function to produce distribution outputs
# Shows overlay of full results of Monte Carlo of decision options

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results,
                                     vars = c ("Interv_NPV" ,  "NO_Interv_NPV"),
                                     method = 'smooth_simple_overlay',
                                     base_size = 7)

input_table

## Use of boxplot to show the Interv_NPV and NO_Interv_NPV
# Useful when comparing multiple outputs
#Boxplots show the median (central line), the 25th and 75th percentiles (sides of boxes) and any outliers

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results,
                                    vars = c ("Interv_NPV" , "NO_Interv_NPV"),
                                    method = 'boxplot')

#Difference in NPV between do and do not do

decisionSupport::plot_distributions (mcSimulation_object = mcSimulation_results,
                                      vars = "NPV_decision_do" ,
                                      method = 'boxplot_density')


##Cashflow analysis####
# distribution of annual cashflow over the simulated period for the intervention(years)
# Use plot_cashflow

plot_cashflow (mcSimulation_object = mcSimulation_results, cashflow_var_name = "Cashflow_decision_do")


##Projection to Latent Structures (PLS) analysis####
#we apply a post-hoc analysis to the mcSimulation outputs with plsr.mcSimulation to determine the variable importance in the project(VIP)

pls_result <- plsr.mcSimulation(object = mcSimulation_results,
              resultName = names(mcSimulation_results$y) [3], ncomp = 1)

# We run the plot_pls on the results from plsr.mcSimulation
#The length of the bars is equal to VIP with a vertical line at '1' on the x-axis indicating a standard cut-off for VIP used for variable selection.
#The overall plot only shows those variables with a VIP > 0.8, which is the common threshold for variable selection.
#The colors of the bars represent the positive or negative coefficient of the given input variable with the output variable.

plot_pls(pls_result, input_table = input_table, threshold = 0)


## Value of Information (VOI) analysis
# We calculate Value of information(VOI) analysis with the expected Value of Perfect Information (EVPI).
# EVPI measures expected opportunity loss that is incurred when decision maker does not have perfect information about a forcast.

mcSimulation_table <- data.frame(mcSimulation_results$x, mcSimulation_results$y [1:3])

evpi <- multi_EVPI(mc = mcSimulation_table,first_out_var = "Interv_NPV")

## we use the function plot_evpi on the results from multi_EVPI to plot the expected Value of Perfect Information (EVPI)

plot_evpi(evpi, decision_vars = "NPV_decision_do")


## Finally, we can use the compound_figure() function to provide a single figure for a quick assessment.
#The can be used to run the full decision assessment for a simple binary decision ('do' or 'do not do')

compound_figure(mcSimulation_object = mcSimulation_results,
                input_table = input_table, plsrResults = pls_result,
                EVPIresults = evpi,decision_var_name = "NPV_decision_do",
                cashflow_var_name = "Cashflow_decision_do",
                base_size = 7)
