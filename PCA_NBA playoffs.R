setwd("~/Library/Mobile Documents/com~apple~CloudDocs/AnalyticsPackages/MSAPython/NBA Project")

library(data.table)
library(dplyr)
library(stringr)
library(tidyr)
#install.packages('ggfortify')
library(ggplot2)
library(ggfortify)

#read file
nba = read.csv('teams_updated.csv')

#look at initial cols
colnames(nba)

#remove unneeded cols
nba = nba[ , !names(nba) %in% c('X', 'Unnamed..0', 'Unnamed..0.1', 'SEASON', 'L', 'GP', 'MIN' )]

#rename +/- to PM
nba = rename(nba, PM = X...)

# PF - personal fouls, PFD - personal fouls drawn

#creating covariance matrix
covM = cor(nba[2:24])
eig = eigen(covM, symmetric = TRUE, only.values = FALSE)
c=colnames(nba[2:24])
eig$values

rownames(eig$vectors) = c(colnames(nba[2:24]))
eig$vectors

#Shows 45% of variance is explained by first two PC's
sum(eig$values[1:2])/sum(eig$values)


#Create scores for principal components
X = scale(nba[2:24], center = TRUE, scale = FALSE)
scores = data.frame(X %*% eig$vectors)
colnames(scores) = c('Prin1', 'Prin2', 'Prin3', 'Prin4')
scores[1:10, ]

#plot components
plot(scores$Prin1, scores$Prin2, main = 'Data Projection',
     xlab = 'First', ylab = 'Second',
     col = c('red', 'blue', 'green', 'purple', 'orange', 'black')[nba$Playoff.Outcome])


#Creating PCA
pca = prcomp(nba[4:24], rank = 6, scale = T)
summary(pca)

#create scree plot
std_dev = pca$sdev
pr_var = std_dev^2
prop_var = pr_var/sum(pr_var)
plot(prop_var, xlab = "Principal Component",
     ylab = "Proportion Explained",
     type = "b")
#4 PC's described 


#View Loadings
pca$rotation[, 1:4]

#plots basic 2 components
plot(pca$x[,1], pca$x[,2],
       col = nba$Playoff.Outcome,
       xlab = "Principal Component 1",
       ylab = "Principal Component 2",
       main = 'NBA PCA')
text(x = pca$x[,1], 
     y = pca$x[,2],
     labels = nba$Playoff.Outcome, adj = 0)

#More complex plot
autoplot(pca, data = nba,
           colour = alpha(c('red', 'blue', 'green', 'purple', 'orange', 'black')[as.factor(nba$Playoff.Outcome)],0.4),
           loadings = TRUE, loadings.colour = 'black',
           loadings.label = TRUE, loadings.label.size = 3.5, loadings.label.alpha = 1,
           loadings.label.fontface='bold',
           loadings.label.colour = 'black',
           loadings.label.repel=T,
         label = TRUE,
         label.label = nba$Playoff.Outcome)
text(x = pca$x[,1], 
     y = pca$x[,2],
     labels = nba$Playoff.Outcome, adj = 0)

#too many teams, going to run same analysis on playoff teams
nba_playoff = nba %>%
        filter(Playoff.Outcome %in% c("Missed", "FR"))

#Creating PCA
pca_play = prcomp(nba_playoff[4:24], rank = 6, scale = T)

#scree plot
par(mfrow=c(1,1))
plot(pca_play$sdev^2)
#4 PC's still explain most variance

#biplot
autoplot(pca_play, data = nba_playoff,
         colour = alpha(c('red', 'blue', 'green', 'purple', 'orange', 'black')[as.factor(nba_playoff$Playoff.Outcome)],0.4),
         loadings = TRUE, loadings.colour = 'black',
         loadings.label = TRUE, loadings.label.size = 3.5, loadings.label.alpha = 1,
         loadings.label.fontface='bold',
         loadings.label.colour = 'black',
         loadings.label.repel=T,
         label = TRUE,
         label.label = paste(nba_playoff$TEAM,"-", nba_playoff$Playoff_Yr))
text(x = pca$x[,1], 
     y = pca$x[,2],
     labels = nba_playoff$Playoff.Outcome, adj = 0)


?alpha()

unique(nba_playoff$Playoff.Outcome)



autoplot(pca_play, data = nba_playoff,
         colour = alpha(c('red', 'blue', 'green', 'purple', 'orange', 'black')[as.factor(nba_playoff$Playoff.Outcome)],0.4),
         loadings = TRUE, loadings.colour = 'black',
         loadings.label = TRUE, loadings.label.size = 3.5, loadings.label.alpha = 1,
         loadings.label.fontface='bold',
         loadings.label.colour = 'black',
         loadings.label.repel=T,
         label = TRUE,
         label.label = paste(nba_playoff$TEAM,"-", nba_playoff$Playoff_Yr))
text(x = pca$x[,3], 
     y = pca$x[,4],
     labels = nba_playoff$Playoff.Outcome, adj = 0)


