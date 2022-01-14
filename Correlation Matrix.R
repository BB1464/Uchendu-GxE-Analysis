
# Correlation matrix ------------------------------------------------------

library(corrplot)
library(tidyverse)
library(readxl)

dat <- read_excel('Copy of Sensory Data for Bayesian Analysis.xlsx')


M1 <- cor.mtest(dat[,4:9])

M <- cor(dat[,4:9])


M2 <- cor.mtest(dat[,4:9],conf.level=.95)
corrplot(cor(dat[,4:9]),
         method='square',
         type = 'lower',
         p.mat = res1$p,
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



