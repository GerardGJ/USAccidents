library(naniar)
library(impute)
library(stringi)
library(class)
library("FactoMineR")
library("factoextra")

setwd("/Users/anderbarriocampos/Desktop")

#USAccidents
USAccidents <- read.csv("USAccidents_preprocessed.csv")
summary(USAccidents)
USAccidents <- USAccidents[,2:23]
USAccidents$State <- as.factor(USAccidents$State)
USAccidents$City <- as.factor(USAccidents$City)
USAccidents$Weather_Condition <- as.factor(USAccidents$Weather_Condition)
USAccidents$Year <- as.factor(USAccidents$Year)
USAccidents$County <- as.factor(USAccidents$County)
USAccidents$Severity <- as.factor(USAccidents$Severity)
USAccidents$Humidity... <- as.numeric(USAccidents$Humidity...)
USAccidents$Month <- as.factor(USAccidents$Month)

USAccidents$Season[USAccidents$Season == "12" | USAccidents$Season == "1" | USAccidents$Season == "2"] = "Winter"
USAccidents$Season[USAccidents$Season == "6" | USAccidents$Season == "7" | USAccidents$Season == "8"] = "Summer"
USAccidents$Season[USAccidents$Season == "9" | USAccidents$Season == "10" | USAccidents$Season == "11"] = "Autumn"
USAccidents$Season[USAccidents$Season == "3" | USAccidents$Season == "4" | USAccidents$Season == "5"] = "Spring"
USAccidents$Season <- as.factor(USAccidents$Season)
USAccidents$Season
USAccidents_noNAs <- USAccidents[rowSums(is.na(USAccidents)) == 0,]


sapply(USAccidents,class)
numeriques <- which(sapply(USAccidents,is.numeric))
numeriques #7 numerical variables for performing PCA

df_numeric <- USAccidents[, numeriques]
sapply(df_numeric, class)

dcon_withNAs <- df_numeric[rowSums(is.na(df_numeric)) > 0,]
dcon <- df_numeric[rowSums(is.na(df_numeric)) == 0,]
summary(dcon_withNAs)
summary(dcon) #dataset with no NA for performing PCA


res.pca <- PCA(dcon, scale.unit = TRUE, ncp = 7, graph = FALSE)
print(res.pca)

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50)) #screeplot

eig.val <-get_eigenvalue(res.pca)
eig.val
kaixo <- eig.val[,3]
bp<-barplot(kaixo, xlab = "Principal Components", ylab = "Cumulative Sum of Inertia")
text(bp, 0, round(kaixo, 1),cex=1,pos=3) 


fviz_pca_ind(res.pca,select.ind = list(contrib = 2000), label="none", axes = c(1,2))
fviz_pca_ind(res.pca,select.ind = list(contrib = 2000), label="none", axes = c(1,3))
fviz_pca_ind(res.pca,select.ind = list(contrib = 2000), label="none", axes = c(1,4))
#fviz_pca_ind(res.pca,select.ind = list(contrib = 2000), label="none", axes = c(2,1)) SAME AS 1,2

fviz_pca_ind(res.pca,select.ind = list(contrib = 2000), label="none", axes = c(2,3))
fviz_pca_ind(res.pca,select.ind = list(contrib = 2000), label="none", axes = c(2,4))

fviz_pca_ind(res.pca,select.ind = list(contrib = 2000), label="none", axes = c(3,4))




fviz_pca_ind(res.pca, col.ind = "cos2", select.ind = list(contrib = 2000), label="none", axes = c(1,2),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)
fviz_pca_ind(res.pca, col.ind = "cos2", select.ind = list(contrib = 2000), label="none", axes = c(1,3),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)
fviz_pca_ind(res.pca, col.ind = "cos2", select.ind = list(contrib = 2000), label="none", axes = c(1,4),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)
fviz_pca_ind(res.pca, col.ind = "cos2", select.ind = list(contrib = 2000), label="none", axes = c(2,3),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)
fviz_pca_ind(res.pca, col.ind = "cos2", select.ind = list(contrib = 2000), label="none", axes = c(2,4),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)
fviz_pca_ind(res.pca, col.ind = "cos2", select.ind = list(contrib = 2000), label="none", axes = c(3,4),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)





fviz_pca_var(res.pca, col.var = "contrib", repel = TRUE, axes=c(1,2),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
fviz_pca_var(res.pca, col.var = "contrib", repel = TRUE, axes=c(1,3),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
fviz_pca_var(res.pca, col.var = "contrib", repel = TRUE, axes=c(1,4),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
fviz_pca_var(res.pca, col.var = "contrib", repel = TRUE, axes=c(2,3),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
fviz_pca_var(res.pca, col.var = "contrib", repel = TRUE, axes=c(2,4),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
fviz_pca_var(res.pca, col.var = "contrib", repel = TRUE, axes=c(3,4),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)






fviz_pca_biplot(res.pca, select.ind = list(contrib = 2000), axes=c(1,2), repel = TRUE,
                select.var = list(contrib = 7),
                habillage = USAccidents_noNAs$Severity, addEllipses = TRUE, elipse.level = 0.2,
                col.var = "red", alpha.var ="cos2",
                label = "var") +
  scale_color_brewer(palette="Dark2")+
  theme_minimal()
fviz_pca_biplot(res.pca, select.ind = list(contrib = 2000), axes=c(1,3), repel = TRUE,
                select.var = list(contrib = 7),
                habillage = USAccidents_noNAs$Severity, addEllipses = TRUE, elipse.level = 0.2,
                col.var = "red", alpha.var ="cos2",
                label = "var") +
  scale_color_brewer(palette="Dark2")+
  theme_minimal()
fviz_pca_biplot(res.pca, select.ind = list(contrib = 2000), axes=c(1,4), repel = TRUE,
                select.var = list(contrib = 7),
                habillage = USAccidents_noNAs$Severity, addEllipses = TRUE, elipse.level = 0.2,
                col.var = "red", alpha.var ="cos2",
                label = "var") +
  scale_color_brewer(palette="Dark2")+
  theme_minimal()
fviz_pca_biplot(res.pca, select.ind = list(contrib = 2000), axes=c(2,3), repel = TRUE,
                select.var = list(contrib = 7),
                habillage = USAccidents_noNAs$Severity, addEllipses = TRUE, elipse.level = 0.2,
                col.var = "red", alpha.var ="cos2",
                label = "var") +
  scale_color_brewer(palette="Dark2")+
  theme_minimal()
fviz_pca_biplot(res.pca, select.ind = list(contrib = 2000), axes=c(2,4), repel = TRUE,
                select.var = list(contrib = 7),
                habillage = USAccidents_noNAs$Severity, addEllipses = TRUE, elipse.level = 0.2,
                col.var = "red", alpha.var ="cos2",
                label = "var") +
  scale_color_brewer(palette="Dark2")+
  theme_minimal()
fviz_pca_biplot(res.pca, select.ind = list(contrib = 2000), axes=c(3,4), repel = TRUE,
                select.var = list(contrib = 7),
                habillage = USAccidents_noNAs$Severity, addEllipses = TRUE, elipse.level = 0.2,
                col.var = "red", alpha.var ="cos2",
                label = "var") +
  scale_color_brewer(palette="Dark2")+
  theme_minimal()



