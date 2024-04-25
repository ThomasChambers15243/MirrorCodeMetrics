library(ggplot2)
library(mice)
library(tidyverse)

#problem = 2

humanDataDir <- "HumanResults/"
genDataDir <- "SampleResults/"


for (problem in 0:4) {
  problem <- 3
  hd <- read.csv(gsub(" ", "", paste(humanDataDir,'p', problem,".csv")))
  gd <- read.csv(gsub(" ", "", paste(genDataDir,'p', problem,".csv")))

  # Create Imputated data
  hd.imp <- mice(hd, m = 1, method = "pmm")
  gd.imp <- mice(gd, m = 1, method = "pmm")

  hd.comp <- complete(hd.imp, "long", include = FALSE)
  gd.comp <- complete(gd.imp, "long", include = FALSE)

  # ggplot(hd.comp, aes(x=Attempt, y=Score)) +
  #  geom_point()

  df <- data.frame(
    Attempt = hd.comp$Attempt,
    hScore = hd.comp$Score,
    gScore = gd.comp$Score
  )

  # # create scatterplot of hscore vs. attempt
  # plot(df$attempt, df$hscore, col = 'red', pch = 19, xlab = 'attempts', ylab = 'human & generated scores', main = paste('plot of human and generatead scores p:',problem))
  # 
  # 
  # 
  # # overlay scatterplot of gscore vs. attempt
  # points(df$attempt, df$gscore, col = 'blue', pch = 19)
  # 
  # # add a legend
  # legend('topright', legend = c('hscore', 'gscore'), pch = 19, col = c('red', 'blue'))

  # Reshape the data frame to long format for ggplot
  df_long <- reshape2::melt(df, id.vars = "Attempt")
  
  # Plot the bar graph
  ggplot(df_long, aes(x=Attempt, y=value, fill=variable)) +
    geom_bar(stat="identity", position="dodge") +
    theme_minimal() +
    labs(x="Attempt", y="Score", fill="Score Type", title=paste('Bar Graph of human and generatead scores Problem:',problem)) +
    theme(plot.title = element_text(hjust = 0.5))
  
}

