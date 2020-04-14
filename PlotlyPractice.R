#library(dplyr)
#library(magrittr)
library(plotly)

roll <- function(x){ # function to simulate a six-faced die rolled 'x' number of times
  die <- 1:6
  result <- sample(die, size = x, replace = T); return(result)
}

# Create scatterplot demonstrating LNN
set.seed(04272000)
n <- 10000; r <- 1 ; num_roll <- 1:n # 'n' number of rolls; 'r' desired roll; 'numroll' tracker of number of rolls

v1 <- roll(n); v1_prop <- cumsum(v1 == r)/num_roll

df1 <- cbind.data.frame(num_roll, v1_prop)
g1 <- ggplot(df1, aes(x=num_roll, y=v1_prop)) + geom_point(color = 'dodgerblue4') + scale_y_continuous(limits = c(0,0.25)) + 
  geom_hline(yintercept=0.16667, color = 'goldenrod4', linetype = 'dashed') + xlab('Number of Rolls') + ylab('Proportion of 1s')

ggplotly(g1)