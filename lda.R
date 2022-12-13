install.packages("MASS")
library(MASS)
# https://little-book-of-r-for-multivariate-analysis.readthedocs.io/en/latest/src/multivariateanalysis.html
setwd('/Users/ander.barrio/Desktop/MVA-Project')
df <- read.csv("US_Accidents_DT.csv", sep=",",dec=".",stringsAsFactors = TRUE)
names(df)
dim(df)
summary(df)
df <- df[-c(1)]
summary(df)



df.lda <- lda(df$Severity ~ df$Visibility.mi. + df$Precipitation.in. + df$Humidity... + 
                df$Temperature.F. + df$Distance.mi. + df$Pressure.in.)

df.lda

#coeficients de la funcio discriminant
df.lda$scaling[,1]
df.lda$scaling[,2]
df.lda$scaling[,3]


df.lda$scaling[,1:3]

#valors de cada cas per la primera funcio discriminant
df.lda.values <- predict(df.lda, df[,c(6:11)])

df[,20]<- df.lda.values$x[,1]
df[,21]<- df.lda.values$x[,2]
df[,22]<- df.lda.values$x[,3]
names(df)[20]<-"LDA1"
names(df)[21]<-"LDA2"
names(df)[22]<-"LDA3"

calcWithinGroupsVariance <- function(variable,groupvariable)
{
  # find out how many values the group variable can take
  groupvariable2 <- as.factor(groupvariable[[1]])
  levels <- levels(groupvariable2)
  numlevels <- length(levels)
  # get the mean and standard deviation for each group:
  numtotal <- 0
  denomtotal <- 0
  for (i in 1:numlevels)
  {
    leveli <- levels[i]
    levelidata <- variable[groupvariable==leveli,]
    levelilength <- length(levelidata)
    # get the standard deviation for group i:
    sdi <- sd(levelidata)
    numi <- (levelilength - 1)*(sdi * sdi)
    denomi <- levelilength
    numtotal <- numtotal + numi
    denomtotal <- denomtotal + denomi
  }
  # calculate the within-groups variance
  Vw <- numtotal / (denomtotal - numlevels)
  return(Vw)
}


groupStandardise <- function(variables, groupvariable)
{
  # find out how many variables we have
  variables <- as.data.frame(variables)
  numvariables <- length(variables)
  # find the variable names
  variablenames <- colnames(variables)
  # calculate the group-standardised version of each variable
  for (i in 1:numvariables)
  {
    variablei <- variables[i]
    variablei_name <- variablenames[i]
    variablei_Vw <- calcWithinGroupsVariance(variablei, groupvariable)
    variablei_mean <- mean(as.matrix(variablei))  
    variablei_new <- (variablei - variablei_mean)/(sqrt(variablei_Vw))
    data_length <- nrow(variablei)
    if (i == 1) { variables_new <- data.frame(row.names=seq(1,data_length)) }
    variables_new[`variablei_name`] <- variablei_new
  }
  return(variables_new)
}
#si s'estandarditzen les variables es solen obtenir valors mes interpretables
dfStandard <- groupStandardise(df[,c(6:11)], df[13])

dfStandard.lda <- lda(df$Severity ~ dfStandard$Visibility.mi. + dfStandard$Precipitation.in. + dfStandard$Humidity... + 
                        dfStandard$Temperature.F. + dfStandard$Distance.mi. + dfStandard$Pressure.in.)

dfStandard.lda

df.lda.values2 <- predict(dfStandard.lda, dfStandard)
df.lda.values
df.lda.values2

calcBetweenGroupsVariance <- function(variable,groupvariable)
{
  # find out how many values the group variable can take
  groupvariable2 <- as.factor(groupvariable[[1]])
  levels <- levels(groupvariable2)
  numlevels <- length(levels)
  # calculate the overall grand mean:
  grandmean <- mean(as.matrix(variable) )         
  # get the mean and standard deviation for each group:
  numtotal <- 0
  denomtotal <- 0
  for (i in 1:numlevels)
  {
    leveli <- levels[i]
    levelidata <- variable[groupvariable==leveli,]
    levelilength <- length(levelidata)
    # get the mean and standard deviation for group i:
    meani <- mean( as.matrix(levelidata) )
    sdi <- sd(levelidata)
    numi <- levelilength * ((meani - grandmean)^2)
    denomi <- levelilength
    numtotal <- numtotal + numi
    denomtotal <- denomtotal + denomi
  }
  # calculate the between-groups variance
  Vb <- numtotal / (numlevels - 1)
  Vb <- Vb[[1]]
  return(Vb)
}


calcSeparations <- function(variables,groupvariable)
{
  # find out how many variables we have
  variables <- as.data.frame(variables)
  numvariables <- length(variables)
  # find the variable names
  variablenames <- colnames(variables)
  # calculate the separation for each variable
  for (i in 1:numvariables)
  {
    variablei <- variables[i]
    variablename <- variablenames[i]
    Vw <- calcWithinGroupsVariance(variablei, groupvariable)
    Vb <- calcBetweenGroupsVariance(variablei, groupvariable)
    sep <- Vb/Vw
    print(paste("variable",variablename,"Vw=",Vw,"Vb=",Vb,"separation=",sep))
  }
}

#separacio que donen les dues funcions discriminants (ratio de variancia
#entre respecte v intra)
calcSeparations(df.lda.values$x,df[13])
calcSeparations(df.lda.values2$x,df[13])
(df.lda$svd)^2

#total separation (la suma de les dues)
#percentatge que separa cada una, coincideix amb la proportion of trace del model discriminant

df.lda

hist(df.lda.values$x[,1])
hist(df.lda.values$x[,2])
hist(df.lda.values$x[,3])
#histograma multiple entre la funcio discriminant i la resposta
par("mar")
par(mar=c(1,1,1,1))
par(mar=c(5.1,4.1,4.1,2.1))
par(mar=c(3,2.5,1.5,1))


ldahist(data = df.lda.values$x[,1], g=df$Severity, ymax=1)

ldahist(data = df.lda.values$x[,2], g=df$Severity, ymax = 1)

ldahist(data = df.lda.values$x[,3], g=df$Severity, ymax = 1)

#plot de les dues components discriminants (etiquetem els grups)
plot(df.lda.values$x[,1],df.lda.values$x[,2]) # make a scatterplot
text(df.lda.values$x[,1],df.lda.values$x[,2],df$Severity,cex=0.7,pos=4,col="red") # add labels


df.lda$scaling[,1]
df.lda$scaling[,2]
df.lda$scaling[,3]


n<-dim(df)[1]
for(i in 1:n){
  if(wine[i,15]<0){wine[i,17]<-2
  }else{if (wine[i,16]<0){wine[i,17]<-1 
  }else{
    wine[i,17]<-3
  }
  }
}


#matriu de confusio
table(wine[,1])
MC<-table(wine[,1], wine[,17])
MC


#accuracy
accuracy<-sum(diag(MC))/dim(wine)[1]
accuracy

#compute missclassification rate
MR<-1-accuracy
MR

#buscar punts intermedis de les mitjanes i utilitzarlos per definir les regles de classificacio


printMeanAndSdByGroup <- function(variables,groupvariable)
{
  # find the names of the variables
  variablenames <- c(names(groupvariable),names(as.data.frame(variables)))
  # within each group, find the mean of each variable
  groupvariable <- groupvariable[,1] # ensures groupvariable is not a list
  means <- aggregate(as.matrix(variables) ~ groupvariable, FUN = mean)
  names(means) <- variablenames
  print(paste("Means:"))
  print(means)
  # within each group, find the standard deviation of each variable:
  sds <- aggregate(as.matrix(variables) ~ groupvariable, FUN = sd)
  names(sds) <- variablenames
  print(paste("Standard deviations:"))
  print(sds)
  # within each group, find the number of samples:
  samplesizes <- aggregate(as.matrix(variables) ~ groupvariable, FUN = length)
  names(samplesizes) <- variablenames
  print(paste("Sample sizes:"))
  print(samplesizes)
}

#mitjanes de les funcions discriminants per grups
printMeanAndSdByGroup(df.lda.values$x,df[13])
c1 <- (0.2165391 - 0.3092213) / 2
c2 <- (- 0.3092213 + 0.5002339) / 2
c3 <- (0.5002339 + 0.1311213) / 2

calcAllocationRuleAccuracy <- function(ldavalue, groupvariable, cutoffpoints)
{
  # find out how many values the group variable can take
  groupvariable2 <- as.factor(groupvariable[[1]])
  levels <- levels(groupvariable2)
  numlevels <- length(levels)
  # calculate the number of true positives and false negatives for each group
  numlevels <- length(levels)
  for (i in 1:numlevels)
  {
    leveli <- levels[i]
    levelidata <- ldavalue[groupvariable==leveli]
    # see how many of the samples from this group are classified in each group
    for (j in 1:numlevels)
    {
      levelj <- levels[j]
      if (j == 1)
      {
        cutoff1 <- cutoffpoints[1]
        cutoff2 <- "NA"
        results <- summary(levelidata <= cutoff1)
      }
      else if (j == numlevels)
      {
        cutoff1 <- cutoffpoints[(numlevels-1)]
        cutoff2 <- "NA"
        results <- summary(levelidata > cutoff1)
      }
      else
      {
        cutoff1 <- cutoffpoints[(j-1)]
        cutoff2 <- cutoffpoints[(j)]
        results <- summary(levelidata > cutoff1 & levelidata <= cutoff2)
      }
      trues <- results["TRUE"]
      trues <- trues[[1]]
      print(paste("Number of samples of group",leveli,"classified as group",levelj," : ",
                  trues,"(cutoffs:",cutoff1,",",cutoff2,")"))
    }
  }
}

calcAllocationRuleAccuracy(df.lda.values$x[,1], df[13], c(c1, c2, c3))
