library(FactoMineR)
library(dplyr)
library(factoextra)

setwd("/Users/Gerard/Desktop/MVA/Project/")
USAccidents_imputed <- read.csv("USAccidents_imputed.csv")[,2:23]

USAccidents_MFA <- USAccidents_imputed[which(USAccidents_imputed$Weather_Condition != "Unknown"),]

USAccidents_MFA$Weather_Condition <- as.factor(USAccidents_MFA$Weather_Condition)
levels(USAccidents_MFA$Weather_Condition)

colnames(USAccidents_MFA)

geographic <- USAccidents_MFA[,2]
time_df <- USAccidents_MFA[,22]
weather_condition <- USAccidents_MFA[,4]
atmospheric_condition <- USAccidents_MFA[,c(12:16,20)]
distance <- USAccidents_MFA[,19]
scenario <- USAccidents_MFA[,7:10]

DFtoMFA <- cbind(geographic, time_df, weather_condition, atmospheric_condition, distance, scenario)

set.seed(123)
stratified <- as.data.frame(DFtoMFA %>% group_by(geographic) %>% slice_sample(prop = 0.5))
stratified$geographic <- as.factor(stratified$geographic)
stratified$time_df <- as.factor(stratified$time_df)
stratified$weather_condition <- as.factor(stratified$weather_condition)

res.mfa <- MFA(stratified, 
               group = c(1, 1, 1, 6, 1, 4), 
               type = c("n", "n", "n", "s", "s", "n"),
               name.group = c("geographic","time_df","weather_condition",
                              "atmospheric_condition", "distance","scenario"),
               graph = FALSE)


eig.val <- get_eigenvalue(res.mfa)
head(eig.val)
#We can see that getting the three first dimension we can explain 82 percent of the data

fviz_screeplot(res.mfa)
fviz_mfa_var(res.mfa, "group")
fviz_mfa_var(res.mfa, "quanti.var", palette = "jco", 
             col.var.sup = "violet", repel = TRUE) 

plot <- fviz_pca_ind(res.mfa, col.ind = "cos2", select.ind = list(contrib = 2000), label="none", axes = c(1,2),
                     gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                     repel = TRUE # Avoid text overlapping (slow if many points)
)


summary(DFtoMFA)