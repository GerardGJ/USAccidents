library(plyr)
library(ggplot2)
library(ggridges)
library(dplyr)

setwd("~/Documents/MDS/1st_Semester/MVA/Project/D4/")
dataClustered <- read.csv("clustered_data_CA_2020.csv", sep=",",dec=".",stringsAsFactors = TRUE)
nrow(dataClustered[dataClustered$cluster=="1"])
names(dataClustered)
str(dataClustered)

levels(dataClustered$cluster)

table(dataClustered$cluster)
dataClustered$cluster<- revalue(dataClustered$cluster, c("cluster-1" = "1", "cluster-2" = "2", "cluster-3" = "3", "cluster-4" = "4"))
table(dataClustered$cluster)

P<-dataClustered$cluster; P
nc<-length(levels(P)); nc # 4 clusters

dd <- dataClustered[,c(2:13,15,18:22)]

dd$Start_Time<-as.POSIXct(dd$Start_Time, tz = "GMT", format="%Y-%m-%d %H:%M:%OS")
dd$End_Time<-as.POSIXct(dd$End_Time, tz = "GMT", format="%Y-%m-%d %H:%M:%OS")

K <-dim(dd)[2] # 18 features
n<-dim(dd)[1]

nameP="cluster"
pvalk <- matrix(data=0,nrow=nc,ncol=K, dimnames=list(levels(P),names(dd)))

ValorTestXnum <- function(Xnum,P){
  nk <- as.vector(table(P)); 
  n <- sum(nk); 
  xk <- tapply(Xnum,P,mean);
  #valors test
  txk <- (xk-mean(Xnum))/(sd(Xnum)*sqrt((n-nk)/(n*nk))); 
  #p-values
  pxk <- pt(txk,n-1,lower.tail=F);
  for(c in 1:length(levels(as.factor(P)))){if (pxk[c]>0.5){pxk[c]<-1-pxk[c]}}
  return (pxk)
}

ValorTestXquali <- function(P,Xquali){
  taula <- table(P,Xquali);
  n <- sum(taula); 
  pk <- apply(taula,1,sum)/n;
  pj <- apply(taula,2,sum)/n;
  pf <- taula/(n*pk);
  pjm <- matrix(data=pj,nrow=dim(pf)[1],ncol=dim(pf)[2], byrow=TRUE);      
  dpf <- pf - pjm; 
  dvt <- sqrt(((1-pk)/(n*pk))%*%t(pj*(1-pj))); 
  #i hi ha divisions iguals a 0 dona NA i no funciona
  zkj <- dpf
  zkj[dpf!=0]<-dpf[dpf!=0]/dvt[dpf!=0]; 
  pzkj <- pnorm(zkj,lower.tail=F);
  for(c in 1:length(levels(as.factor(P)))){for (s in 1:length(levels(Xquali))){if (pzkj[c,s]> 0.5){pzkj[c,s]<-1- pzkj[c,s]}}}
  return (list(rowpf=pf,vtest=zkj,pval=pzkj))
}

devAskNewPage(ask = TRUE)
for(k in 1:K){
  if (is.numeric(dd[,k])){ 
    plt<- ggplot(dd, aes_string(x=P, y=names(dd)[k], fill=P)) + 
      geom_boxplot(alpha=0.3) +
      theme(legend.position="none") +
      labs(x="cluster")
    print(plt)
    barplot(tapply(dd[[k]], P, mean),main=paste("Means of", names(dd)[k], "per", nameP ), density=c(5,10,20,60) , angle=c(0,45,90,11) , col=rainbow(5))
    abline(h=mean(dd[[k]]), col="blue")
    legend(0,mean(dd[[k]]),"global mean",bty="n")

    # STATISTICAL TESTS
    print("Statistics per cluster:")
    for(s in levels(as.factor(P))) {print(summary(dd[P==s,k]))}
    o<-oneway.test(dd[,k]~P)
    print(paste("p-valueANOVA:", o$p.value))
    kw<-kruskal.test(dd[,k]~P)
    print(paste("p-value Kruskal-Wallis:", kw$p.value))
    pvalk[,k]<-ValorTestXnum(dd[,k], P)
    print("p-values ValorsTest: ")
    print(ValorTestXnum(dd[,k], P))
  
  }else{
    if(class(dd[,k])[1]=="POSIXct"){
      print(summary(dd[,k]))
      print(sd(dd[,k]))

      plt<-ggplot(dd) +
        geom_histogram(aes_string(x=names(dd)[k], fill=P),) +
        labs(fill="cluster")

     print(plt)
    }else{
      table<-table(P,dd[,k])
      
      # proportion of all levels within same cluster
      rowperc<-prop.table(table,1)

      # proportion of one factor level within all clusters
      colperc<-prop.table(table,2)

      dd[,k]<-as.factor(dd[,k])

      paleta<-rainbow(length(levels(dd[,k])))
      
      
      # proportion of observations in all clusters
      marg <- table(as.factor(P))/n
      print(marg)
    
      
      plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of all levels of ",names(dd)[k], "within each cluster"))
      for(c in 1:length(levels(dd[,k]))){lines(rowperc[,c],col=paleta[c]) }
      legend("topright", levels(dd[,k]), col=paleta, lty=2, cex=0.6)
      
      
      plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of each level of",names(dd)[k], "within all clusters"))
      for(c in 1:length(levels(dd[,k]))){lines(colperc[,c],col=paleta[c]) }
      legend("topright", levels(dd[,k]), col=paleta, lty=2, cex=0.6)
      
      # proportion of observations in all factor levels
      marg <-table(dd[,k])/n
      print(marg)
      
      paleta<-rainbow(length(levels(as.factor(P))))
      
      
      plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of all levels of ",names(dd)[k], "within each cluster"), las=3)
      for(c in 1:length(levels(as.factor(P)))){lines(rowperc[c,],col=paleta[c])}
      legend("topright", levels(as.factor(P)), col=paleta, lty=2, cex=0.6)
      
      
      plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of each level of",names(dd)[k], "within all clusters"), las=3)
      for(c in 1:length(levels(as.factor(P)))){lines(colperc[c,],col=paleta[c])}
      legend("topright", levels(as.factor(P)), col=paleta, lty=2, cex=0.6)
      
      
      
      paleta<-rainbow(length(levels(dd[,k])))
      
      table<-table(dd[,k],P)
      print("Cross Table:")
      print(table)

       # stacked bar plot
     
     
      barplot(table(dd[,k], as.factor(P)), beside=FALSE,col=paleta, main=paste("Variable",names(dd)[k]) )
      legend("topright",levels(as.factor(dd[,k])),pch=1,cex=0.5, col=paleta)
      
      # grouped bar plot

      barplot(table(dd[,k], as.factor(P)), beside=TRUE,col=paleta, main=paste("Variable",names(dd)[k]))
      legend("topright",levels(as.factor(dd[,k])),pch=1,cex=0.5, col=paleta)
      
      p<-chisq.test(dd[,k], as.factor(P))
      print("Test Chi quadrat: ")
      print(p)

      q<-ValorTestXquali(dd[,k],P)
      print("p-values ValorsTest: ")
      print(q)
      
    }
  }
}#endfor

dd$duration<-difftime(dd$End_Time, dd$Start_Time, units="mins")

dd$duration<- as.numeric(dd$duration)

summary(dd$duration)

K<-K+1
barplot(tapply(dd$duration, P, mean),main=paste("Means of", names(dd)[19], "per", nameP ), density=c(5,10,20,60) , angle=c(0,45,90,11) , col=rainbow(5))
abline(h=mean(dd$duration), col="blue")
legend(0,mean(dd$duration),"global mean",bty="n")

ggplot(dd, aes(x = log(duration), y = P, fill = P)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")+
  labs(fill="cluster")

ggplot(dd, aes(x=P, y=log(duration), fill=P)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none") +
  labs(x="cluster")

#Most significant descriptors for each cluster
for (c in 1:length(levels(as.factor(P)))) {
  if(!is.na(levels(as.factor(P))[c])){
    print(paste("P.values per class:",levels(as.factor(P))[c]));
    print(sort(pvalk[c,]), digits=3) 
  }
}


DESCRIPTION = c(0, 0, 0,0)
summaryTable <- matrix(data=0,nrow=nc,ncol=K, dimnames=list(levels(P),names(dd)))
summaryTable <- cbind(summaryTable,DESCRIPTION) 

