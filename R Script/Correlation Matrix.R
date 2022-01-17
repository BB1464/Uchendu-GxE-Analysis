
# Correlation matrix ------------------------------------------------------
# Uncomment this line and run it to install the packages

# install.packages("corrplot")
# install.packages("tidyverse")
# install.packages("readxl")
# install.packages('here')

library(corrplot)
library(tidyverse)
library(readxl)
library(here)

dat <- read_excel(here::here('Data/Copy of Sensory Data for Bayesian Analysis.xlsx'))


M1 <- cor.mtest(dat[,4:9])

M <- cor(dat[,4:9])

M2 <- cor.mtest(dat[,4:9],conf.level=.95)

corrplot(cor(dat[,4:9]),
         method='square',
         type = 'lower',
         insig = 'label_sig',
         sig.level = c(.001,.01,.05),
         pch.cex = 0.8,
         pch.col = 'yellow',
         tl.cex = 1,
         addCoef.col = 'black',
         tl.pos = 'n',
         outline = TRUE)


corrplot.mixed(corr = M,lower = 'number',
               upper = 'ellipse',
               tl.pos = 'lt',
               diag = 'u',
               lower.col = 'black')


# Mark the significant values with star based on the significant l --------

corrplot.mixed(corr = M,lower = 'number',
               upper = 'ellipse',
               tl.pos = 'lt',
               diag = 'u',
               lower.col = 'black', 
               sig.level = c(0.001, 0.01, 0.05)
               ,p.mat=M1$p,pch.cex=0.9,pch.col='gray20')



# New line based on github reply 

corrplot.mixed(corr = M,lower = 'number',
               upper = 'ellipse',
               tl.pos = 'lt',
               diag = 'u',
               lower.col = 'black', 
               sig.level = c(0.001, 0.01,0.05)
               ,insig = 'label_sig',p.mat=M1$p,pch.cex=0.9,pch.col='gray20')







