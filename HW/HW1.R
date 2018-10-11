rm(list=ls())
library(tidyverse)
library(haven)
library(corrr)
library(lmtest)

laborDF <- read_dta("./MROZ.dta") %>%
# 1) Remove Columns that dont meet our criteria
    select(-hours, -wage, -nwifeinc, -lwage) %>%
# 2) Transform variables that are on limited scale and normalize
    mutate_at(vars(age, hushrs), log) %>%
    mutate_at(vars(-inlf), function(x) (x - mean(x))/sd(x))

# check out the top variables it looks like both expr and expr^2 have hi corr
# so we can inlcude them both
laborDF %>%
    correlate %>%
    focus(inlf) %>%
    mutate(abs.corr=abs(inlf)) %>%
    arrange(-abs.corr)

# Take everything with more than 15% corr
fbase <- inlf ~ repwage + exper + expersq + kidslt6 + educ
baseModel <- glm(fbase, family=binomial(link=logit), data=laborDF)

baseModelDF <- laborDF %>%
    mutate(fittedBase=baseModel$fitted.values, inlf=as.logical(inlf)) %>%
    mutate(residNorm=(inlf-fittedBase)/sqrt(fittedBase*(1-fittedBase))) %>%
    arrange(residNorm) %>%
    mutate(Outlier=abs(residNorm) > 2, id=1:n())

baseModelDF %>%
    ggplot(aes(fittedBase, group=inlf, fill=inlf)) +
    geom_density(alpha=.3) +
    theme_classic() +
    labs(x="Fitted Probabilities", y="Density of Fits", 
         fill="Labor Force\nParticipant")

baseModelDF %>%
    ggplot(aes(x=id, y=residNorm, color=Outlier)) +
    theme_classic() +
    geom_point() +
    scale_colour_manual(values=c("TRUE"="red", "FALSE"="black")) +
    labs(x="", y="Standardized Residuals")

nullModel <- glm(inlf~1, family=binomial(link=logit), data=laborDF)
M <- step(
    nullModel, trace=TRUE, direction="both",
    scope=list(upper=baseModel, lower=nullModel))

singVars <- attr(M$terms, "term.labels")
names(singVars) <- singVars

modelDropList <- lapply(singVars, function(v){
    newf <- drop.terms(
        M$terms, 
        grep(v, attr(M$terms, "term.labels")), 
        keep.response=TRUE)
    update(M, newf)
})

pValueCompare <- sapply(modelDropList, function(m){
    round(lrtest(m,M)$`Pr(>Chisq)`[2], 4)
})

bicCompare <- sapply(modelDropList, function(m){
    BIC(m) > BIC(M)
})

brierCompare <- sapply(modelDropList, function(m){
        mean((laborDF$inlf-m$fitted.values)^2)})

# select the model 
finalM <- modelDropList[[which(!bicCompare)]]
