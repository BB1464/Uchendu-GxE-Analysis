## -------------------------------------------------------------------------------
#intstall.packages('statgenSTA')
#install.packages('statgenGxE')
#install.packages('here')

library(readxl)
library(statgenGxE)
library(statgenSTA)
library(tidyverse)


## -------------------------------------------------------------------------------
data <- read_excel(here::here('Data/Copy of Sensory Data for Bayesian Analysis.xlsx'))


## -------------------------------------------------------------------------------
dataTD <- createTD(data=data,genotype = 'Code',trial = 'Env')


## -------------------------------------------------------------------------------
Finlay1 <- gxeFw(TD=dataTD,trait='Mealiness')


## -------------------------------------------------------------------------------
plot(Finlay1,plotType = 'scatter')


## -------------------------------------------------------------------------------
Mixed_VarComp <- gxeVarComp(TD = dataTD,trait = 'Mealiness')
summary(Mixed_VarComp)


## -------------------------------------------------------------------------------
vc(Mixed_VarComp)


## -------------------------------------------------------------------------------
herit(Mixed_VarComp)


## -------------------------------------------------------------------------------
# Boxplot showing the distribution of the dataset

plot(dataTD, plotType = "box", traits = "Mealiness",orderBy = "descending")


GGE_Mealiness <- gxeGGE(TD=dataTD,trait='Mealiness')
summary(GGE_Mealiness)

# gxeAmmi() for AMMI ANALYSIS


## -------------------------------------------------------------------------------
plot(GGE_Mealiness,plotType = 'GGE2',scale = 0.5,sizeGeno = 3)+theme_classic()
plot(GGE_Mealiness,plotType = 'AMMI1',scale = 0.5,sizeGeno = 3)+theme_classic()+theme_classic()
plot(GGE_Mealiness,plotType = 'AMMI2',scale = 0.5,sizeGeno = 3)+theme_classic()


## -------------------------------------------------------------------------------
Mega_Mealiness <- gxeMegaEnv(TD = dataTD,trait = 'Mealiness')
summary(Mega_Mealiness)

predict(Mega_Mealiness) # Shows best linear unbiased predictors alongside with their standard errors


## -------------------------------------------------------------------------------
plot(Mega_Mealiness)


## -------------------------------------------------------------------------------
Stability_Mealiness <- gxeStability(TD = dataTD,trait = 'Mealiness')
summary(Stability_Mealiness) # This shows top 10 performing genotype
#summary(Stability_Mealiness,pctGeno=2) # This shows top 2 performing genotype


## -------------------------------------------------------------------------------
plot(Stability_Mealiness)


## -------------------------------------------------------------------------------
Finlay2 <- gxeFw(TD=dataTD,trait='Fibre')


## -------------------------------------------------------------------------------
plot(Finlay2,plotType = 'scatter')


## -------------------------------------------------------------------------------

# Boxplot showing the distribution of the dataset

plot(dataTD, plotType = "box", traits = "Fibre",orderBy = "descending")


Mixed_VarComp2 <- gxeVarComp(TD = dataTD,trait = 'Fibre')
summary(Mixed_VarComp2)


## -------------------------------------------------------------------------------
vc(Mixed_VarComp2)


## -------------------------------------------------------------------------------
herit(Mixed_VarComp2)


## -------------------------------------------------------------------------------
GGE_Fibre2 <- gxeGGE(TD=dataTD,trait='Fibre')
summary(GGE_Fibre2)


## -------------------------------------------------------------------------------
plot(GGE_Fibre2,plotType = 'GGE2',scale = 0.5,sizeGeno = 3)+theme_classic()
plot(GGE_Fibre2,plotType = 'AMMI1',scale = 0.5,sizeGeno=3)+theme_classic()
plot(GGE_Fibre2,plotType = 'AMMI2',scale = 0.5,sizeGeno = 3)+theme_classic()


## -------------------------------------------------------------------------------
Mega_Fibre2 <- gxeMegaEnv(TD = dataTD,trait = 'Fibre')
summary(Mega_Fibre2)


## -------------------------------------------------------------------------------
plot(Mega_Fibre2)


## -------------------------------------------------------------------------------
Stability_Fibre2 <- gxeStability(TD = dataTD,trait = 'Fibre')
summary(Stability_Fibre2) # This shows top 10 performing genotype
#summary(Stability_Mealiness,pctGeno=2) # This shows top 2 performing genotype


## -------------------------------------------------------------------------------
plot(Stability_Fibre2)


## -------------------------------------------------------------------------------
Finlay3 <- gxeFw(TD=dataTD,trait='ADH')


## -------------------------------------------------------------------------------
plot(Finlay3,plotType = 'scatter')


## -------------------------------------------------------------------------------
# Boxplot showing the distribution of the dataset

plot(dataTD, plotType = "box", traits = "ADH",orderBy = "descending")


Mixed_VarComp3 <- gxeVarComp(TD = dataTD,trait = 'ADH')
summary(Mixed_VarComp3)


## -------------------------------------------------------------------------------
vc(Mixed_VarComp3)


## -------------------------------------------------------------------------------
herit(Mixed_VarComp3)


## -------------------------------------------------------------------------------
GGE_ADH3 <- gxeGGE(TD=dataTD,trait='ADH')
summary(GGE_ADH3)


## -------------------------------------------------------------------------------
plot(GGE_ADH3,plotType = 'GGE2',scale = 0.5,sizeGeno = 3)+theme_classic()
plot(GGE_ADH3,plotType = 'AMMI1',scale = 0.5,sizeGeno=3)+theme_classic()
plot(GGE_ADH3,plotType = 'AMMI2',scale = 0.5,sizeGeno=3)+theme_classic()


## -------------------------------------------------------------------------------
Mega_ADH3 <- gxeMegaEnv(TD = dataTD,trait = 'ADH')
summary(Mega_ADH3)


## -------------------------------------------------------------------------------
plot(Mega_ADH3)


## -------------------------------------------------------------------------------
Stability_ADH3 <- gxeStability(TD = dataTD,trait = 'ADH')
summary(Stability_ADH3) # This shows top 10 performing genotype
#summary(Stability_Mealiness,pctGeno=2) # This shows top 2 performing genotype


## -------------------------------------------------------------------------------
plot(Stability_ADH3)


## -------------------------------------------------------------------------------
Finlay4 <- gxeFw(TD=dataTD,trait='Softness')


## -------------------------------------------------------------------------------
plot(Finlay4,plotType = 'scatter')


## -------------------------------------------------------------------------------
# Boxplot showing the distribution of the dataset

plot(dataTD, plotType = "box", traits = "Softness",orderBy = "descending")


Mixed_VarComp4 <- gxeVarComp(TD = dataTD,trait = 'Softness')
summary(Mixed_VarComp4)


## -------------------------------------------------------------------------------
vc(Mixed_VarComp4)


## -------------------------------------------------------------------------------
herit(Mixed_VarComp4)


## -------------------------------------------------------------------------------
GGE_Softness4 <- gxeGGE(TD=dataTD,trait='Softness')
summary(GGE_Softness4)


## -------------------------------------------------------------------------------
plot(GGE_Softness4,plotType = 'GGE2',scale = 0.5,sizeGeno = 3)+theme_classic()
plot(GGE_Softness4,plotType = 'AMMI1',scale = 0.5,sizeGeno=3)+theme_classic()
plot(GGE_Softness4,plotType = 'AMMI2',scale = 0.5,sizeGeno=3)+theme_classic()




## -------------------------------------------------------------------------------
Mega_Softness4 <- gxeMegaEnv(TD = dataTD,trait = 'Softness')
summary(Mega_Softness4)

predict(Softness) # Shows best linear unbiased predictors alongside with their standard errors

## -------------------------------------------------------------------------------
plot(Mega_Softness4)


## -------------------------------------------------------------------------------
Stability_Softness4 <- gxeStability(TD = dataTD,trait = 'Softness')
summary(Stability_Softness4) # This shows top 10 performing genotype
#summary(Stability_Mealiness,pctGeno=2) # This shows top 2 performing genotype


## -------------------------------------------------------------------------------
plot(Stability_Softness4)


## -------------------------------------------------------------------------------
Finlay5 <- gxeFw(TD=dataTD,trait='Taste')


## -------------------------------------------------------------------------------
plot(Finlay5,plotType = 'scatter')


## -------------------------------------------------------------------------------
# Boxplot showing the distribution of the dataset

plot(dataTD, plotType = "box", traits = "Taste",orderBy = "descending")


Mixed_VarComp5 <- gxeVarComp(TD = dataTD,trait = 'Taste')
summary(Mixed_VarComp5)


## -------------------------------------------------------------------------------
vc(Mixed_VarComp5)


## -------------------------------------------------------------------------------
herit(Mixed_VarComp5)


## -------------------------------------------------------------------------------
GGE_Taste5 <- gxeGGE(TD=dataTD,trait='Taste')
summary(GGE_Taste5)


## -------------------------------------------------------------------------------
plot(GGE_Taste5,plotType = 'GGE2',scale = 0.5,sizeGeno = 3)+theme_classic()
plot(GGE_Taste5,plotType = 'AMMI1',scale = 0.5,sizeGeno=3)+theme_classic()
plot(GGE_Taste5,plotType = 'AMMI2',scale = 0.5,sizeGeno=3)+theme_classic()


## -------------------------------------------------------------------------------
Mega_Taste5 <- gxeMegaEnv(TD = dataTD,trait = 'Taste')
summary(Mega_Taste5)

predict(Taste) # Shows best linear unbiased predictors alongside with their standard errors

## -------------------------------------------------------------------------------
plot(Mega_Taste5)


## -------------------------------------------------------------------------------
Stability_Taste5 <- gxeStability(TD = dataTD,trait = 'Taste')
summary(Stability_Taste5) # This shows top 10 performing genotype
#summary(Stability_Mealiness,pctGeno=2) # This shows top 2 performing genotype


## -------------------------------------------------------------------------------
plot(Stability_Taste5)


## -------------------------------------------------------------------------------
Finlay6 <- gxeFw(TD=dataTD,trait='Colour')


## -------------------------------------------------------------------------------
plot(Finlay6,plotType = 'scatter')


## -------------------------------------------------------------------------------
Mixed_VarComp6 <- gxeVarComp(TD = dataTD,trait = 'Colour')
summary(Mixed_VarComp6)


## -------------------------------------------------------------------------------
vc(Mixed_VarComp6)


## -------------------------------------------------------------------------------
herit(Mixed_VarComp6)


## -------------------------------------------------------------------------------
# Boxplot showing the distribution of the dataset

plot(dataTD, plotType = "box", traits = "Colour",orderBy = "descending")


GGE_Colour6 <- gxeGGE(TD=dataTD,trait='Colour')
summary(GGE_Colour6)


## -------------------------------------------------------------------------------
plot(GGE_Colour6,plotType = 'GGE2',scale = 0.5,sizeGeno = 3)+theme_classic()
plot(GGE_Colour6,plotType = 'AMMI1',scale = 0.5,sizeGeno=3)+theme_classic()
plot(GGE_Colour6,plotType = 'AMMI2',scale = 0.5,sizeGeno=3)+theme_classic()


## -------------------------------------------------------------------------------
Mega_Colour6 <- gxeMegaEnv(TD = dataTD,trait = 'Colour')
summary(Mega_Colour6)

predict(Colour) # Shows best linear unbiased predictors alongside with their standard errors

## -------------------------------------------------------------------------
plot(Mega_Colour6)


## -------------------------------------------------------------------------------
Stability_Colour6 <- gxeStability(TD = dataTD,trait = 'Colour')
summary(Stability_Colour6) # This shows top 10 performing genotype
#summary(Stability_Mealiness,pctGeno=2) # This shows top 2 performing genotype


## ------------------------------------------------------------------------
plot(Stability_Colour6)

