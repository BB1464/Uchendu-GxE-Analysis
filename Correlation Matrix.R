
# Correlation matrix ------------------------------------------------------

library(corrplot)
library(tidyverse)
library(readxl)

dat <- read_excel('Copy of Sensory Data for Bayesian Analysis.xlsx')
M=cor(dat[,4:9])
ord=corrMatOrder(M,order='AOE')
M2=M[ord,ord]


corrplot.mixed(M,upper = 'ellipse',lower = 'number')





corrplot(M,upper = 'ellipse',lower = 'number')
