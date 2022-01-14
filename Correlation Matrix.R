
# Correlation matrix ------------------------------------------------------

library(corrplot)
library(tidyverse)
library(readxl)

dat <- read_excel('Copy of Sensory Data for Bayesian Analysis.xlsx')







corrplot.mixed(corr = M,lower = 'number',
               upper = 'ellipse',
               tl.pos = 'lt',
               diag = 'u',
               lower.col = 'black')



