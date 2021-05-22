library(decisionSupport)

install.packages("DiagrammeR")

library(DiagrammeR)

### Impact pathway

mermaid("graph LR
        Y(Yield)-->I(Income); linkStyle 0 stroke:green, stroke-width:1.5px
        M(Market price)-->I; linkStyle 1 stroke: green, stroke-width:1.5px
        I-->F(Final result); linkStyle 2 stroke: green, stroke-width:1.5px
        CL(Labor cost)-->F; linkStyle 3 stroke: red, stroke-width:1.5px")

mermaid("graph LR
        Y(Yield)-->I(Income); linkStyle 0 stroke:green, stroke-width:1.5px
        M(Market price)-->I; linkStyle 1 stroke: green, stroke-width:1.5px
        I-->F(Final result); linkStyle 2 stroke: green, stroke-width:1.5px
        CL(Labor cost)-->F; linkStyle 3 stroke: red, stroke-width:1.5px
         MC(Management Cost)--> F(Final result); linkStyle 4 stroke: yellow, stroke-width:1.5px")


##TD####

mermaid("graph TD
        Y(Yield)-->I(Income); linkStyle 0 stroke:green, stroke-width:1.5px
        M(Market price)-->I; linkStyle 1 stroke: green, stroke-width:1.5px
        I-->F(Final result); linkStyle 2 stroke: green, stroke-width:1.5px
        CL(Labor cost)-->F; linkStyle 3 stroke: red, stroke-width:1.5px
         MC(Management Cost)--> F(Final result); linkStyle 4 stroke: yellow, stroke-width:1.5px")

### Change node colors

mermaid("graph TD
        Y(Yield)-->I(Income); linkStyle 0 stroke:green, stroke-width:2px,
        M(Market price)-->I; linkStyle 1 stroke: green, stroke-width:2px
        I-->F(Final result); linkStyle 2 stroke: green, stroke-width:2px
        CL(Labor cost)-->F; linkStyle 3 stroke: red, stroke-width:2px
         MC(Management Cost)--> F(Final result); linkStyle 4 stroke: yellow, stroke-width:2px")

mermaid("graph TB
        Y(Yield)-->I(Income); style I fill:green
        linkStyle 0 stroke:green, stroke-width:2px
        M(Market price)-->I; linkStyle 1 stroke: green, stroke-width:2px
        I-->F(Final result); linkStyle 2 stroke: green, stroke-width:2px
        CL(Labor cost)-->F; style CL fill:red
        linkStyle 3 stroke: red, stroke-width:2px
        CM(Management cost)-->F; style CM fill:red
        linkStyle 4 stroke: red, stroke-width:2px")

## management cost####


input_estimates <- data.frame(variable = c("Yield", "Market_price", "Labor_cost"),
                              lower = c(6000, 3, 500),
                              median = NA,
                              upper = c(14000, 8, 1000),
                              distribution = c("posnorm", "posnorm", "posnorm"),
                              label = c("Yield (kg/ha)", "Price (USD/kg)", "Labor cost (USD/ha)"),
                              Description = c("Yield in a sweet cherry farm under normal conditions",
                                              "Price of sweet cherry in a normal season",
                                              "Labor costs in a normal season"))





input_estimates <- data.frame(variable = MC("management_cost"),
                              lower = c(100),
                              median = NA,
                              upper = c(2000),
                              distribution = c("posnorm", "posnorm", "posnorm"),
                              label = MC("Management_cost(USD/ha"),
                              Description = MC("Management _cost in a normal season"))






input_estimates <- data.frame(variable = c("Yield", "Market_price", "Labor_cost", "Management_cost"),
                    lower = c(6000, 3, 500, 100),
                    median = NA,
                    upper = c(14000, 8, 1000, 2000),
                    distribution = c("posnorm", "posnorm", "posnorm", "posnorm"),
                    label = c("Yield (kg/ha)", "Price (USD/kg)", "Labor cost (USD/ha)", "Management cost (USD/ha)"),
                    Description = c("Yield in a sweet cherry farm under normal conditions",
                                    "Price of sweet cherry in a normal season",
                                    "Labor costs in a normal season", 
                                    "Management costs in a normal season"))

#input_estimates



### Estimate the income a normal season
# Simplified model function

                                         
 model_function <- function()
               {
               income <- Yield * Market_price
               overall_costs <- Labor_cost + Management_cost
               final_result <- income - overall_cost
               return(list(final_result = final_result))
  
 }
 
 
 # A more explained model function of income
 
 model_function <- function(){
   
   # Estimate the income in a normal season
   income <- Yield * Market_price
   
   # estimate of overall_ cost
   Overall_cost <- Labor_cost + Management_cost
   
   # Estimate the final results from the model
   final_result <- income - Overall_cost
   
   # Generate the list of outputs from the Monte Carlo simulation
   return(list(final_result = final_result))
 }
 


# Run the Monte Carlo simulation using the model function

chile_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                    model_function = model_function,
                                    numberOfModelRuns = 800,
                                    functionSyntax = "plainNames")

# new renamed input_estimates

 chile_mc_simulation                                      
                                         
                                         
                                         
                                         
                                         
 ##  histogram ####

plot_distributions(mcSimulation_object = chile_mc_simulation,
                   vars = "final_result",
                   method = "boxplot_density",
                   old_names = "final_result",
                   new_names = "Outcome distribution for profits")                                        
                                         
                                         
                                         
                                         
