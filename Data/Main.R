library(mice)
library(magrittr)
library(ggplot2)
library(effsize)

# Paths to data Set
humanDataDir <- "HumanResults/"
genDataDir <- "SampleResults/"

# Number of Imputations
imputations <- 5
number_of_problems <- 4

# Problem to be Examined
problem = 0

## Functions
insert_sorted_value <- function(value, existing_list) {
  # Insert the value into the list while maintaining sorted order
  existing_list <- c(existing_list, value)
  existing_list <- sort(existing_list, decreasing = TRUE)
  return(existing_list)
}

# Cohens Effect size using pooled standard deviation
calc_effect_size <- function(list1, list2) {
  
  SD1 <- sd(list1)
  SD2 <- sd(list2)
  pooledSD <- sqrt(( (length(list1-1))*SD1^2 + (length(list2-1))*SD2^2) / 2)
  effectR <- (mean(list1) - mean(list2)) / pooledSD
  return (effectR)   
}


# Holds final values of all problems
final_results = data.frame(matrix( 
  vector(), 0, 4, dimnames=list(c(), c("ProblemNum", "Effect Size", "z-Value", "p-Values"))), 
  stringsAsFactors=F) 



## Data Collection using Multiple Imputation ##

for (problem in 0:number_of_problems) { 
  genSampleResults_P <- read.csv(gsub(" ", "", paste(genDataDir,'P',problem,".csv")))
  humanSampleResults_P <- read.csv(gsub(" ", "", paste(humanDataDir,'p',problem,".csv")))
  
  sampleSize = nrow(genSampleResults_P) 
  
  ## STAGE 1 - Create Imputation ##
  
  # Create Imputated data
  humanSampleResults_P.imp <- mice(humanSampleResults_P, m = imputations, method = "pmm")
  genSampleResults_P.imp <- mice(genSampleResults_P, m = imputations, method = "pmm")
  
  
  #Plots Graph of Imputated Values
  # Visualize Shit
  # Extract the "tall" matrix which stacks the imputations
  humanSampleResults_P.comp <- complete(humanSampleResults_P.imp, "long", include = TRUE)
  
  # cci returns logical whether its input is complete at each observation.
  humanSampleResults_P.comp$Score.NA <- cci(humanSampleResults_P$Score)

  # Plot imputed data set relitive to pre-imputed dataset
  ggplot(humanSampleResults_P.comp,
         aes(x= .imp, y = Score, color = Score.NA)) +
    geom_jitter(show.legend = FALSE,
                width = .1)
  
  # Gets complete set of all imuputations
  humanSampleResults_P.comp <- complete(humanSampleResults_P.imp, "long", include = FALSE)
  genSampleResults_P.comp <- complete(genSampleResults_P.imp, "long", include = FALSE)
  
  
  # Creates data frame with both groups scores
  imp = humanSampleResults_P.comp$.imp
  humResults = humanSampleResults_P.comp$Score
  genResults = genSampleResults_P.comp$Score
  samples <- data.frame(imp, humResults, genResults)
  
  ## STAGE 2 - Analysis of Data ##
  
  # Holds values for imputed sets
  pValueResults <- numeric(0)
  zValueResults <- numeric(0)
  zeffectSizes <- numeric(0)
  for (curImp in 1:imputations) {
    # Gets individual imputed sets
    hum <- samples$humResults[samples$imp == curImp]
    gen <- samples$genResults[samples$imp == curImp]
    print(shapiro.test(hum))
    print(shapiro.test(gen))
    # Perform Test
    results = wilcox.test(hum, gen,
                          alternative = "greater", paired = TRUE, exact = FALSE)
    # Append lists
    zValueResults <- append(results$statistic, zValueResults)
    pValueResults <- append(results$p.value, pValueResults)
    zeffectSizes <- append(abs(qnorm(results$p.value/2))/sqrt(13), zeffectSizes)
  }
  
  
  ## STAGE 3 - Collect Data ##
  final_results[nrow(final_results) + 1, ] <- c(problem, median(zeffectSizes), median(zValueResults), median(pValueResults))
}

## Type 1 Error Correction: Benjamini-Hochberg Procedure ## 

# Sort List of pValues
unordered_pValues <- final_results$p.Values
ordered_pValues <- numeric(0)
for (value in unordered_pValues) {
  ordered_pValues <- insert_sorted_value(value, ordered_pValues)
} 

# Benjamini-Hochberg Procedure 
# Goes through values and seperates the significant values of the insignificant according to procedure, [https://www.wikiwand.com/en/False_discovery_rate]
significance = 0.05
remaining_Values = numeric(0)
for (index in 1:length(ordered_pValues)) {
  hochSig = (significance/index)
  
  if (ordered_pValues[index] < hochSig) {
    remaining_Values <- ordered_pValues[(index + 1):length(ordered_pValues)]
    break
  }
}


# Filter the data frame
significant_results <- final_results[final_results$p.Values %in% remaining_Values, ]
indicative_results <- final_results[!final_results$p.Values %in% remaining_Values, ]

# Print Final Results
print(significant_results)
print(indicative_results)
