#Name: Alejandra Regla-Vargas 
#Course: STAT 571: Modern Data Mining 
#Assignment: PCA, K-means, Regression 

#load packages 
pacman::p_load(data.table, factoextra, knitr, irlba, ISLR, Rfast, tidyverse, car)

# Case Study 1: Self-Esteem

## Data Prep and Cleaning

self <- read_csv("data/NLSY79.csv")
skimr::skim(self) %>%
  select(-c(numeric.p0, numeric.p25, numeric.p50, numeric.p75, numeric.p100, numeric.hist))

#exploratory data analysis 
table(self$Education05) #years of education 
table(self$Income05)

#fix income values -2 n=25
table(self$Income87)
self$Income87[self$Income87==-2] <- NA
self$Income87[self$Income87==-1] <- NA

#fix height values 
table(self$HeightFeet05)
self$HeightFeet05[self$HeightFeet05==-4] <- NA

#assign 0s to the average response 13523 
table(self$Income87)
summary(self$Income87)

self$Income87[self$Income87==0] <- 13523

table(self$Weight05)
table(self$Gender)

table(self$Job05) #address uncodeable value n=1 
self$Job05[self$Job05=='9990: Uncodeable'] <- NA

table(self$HeightFeet05)
table(self$HeightInch05)

table(self$Imagazine)
table(self$Inewspaper)

table(self$MotherEd)
table(self$FatherEd)

table(self$FamilyIncome78)

table(self$Science)
table(self$Arith)
table(self$Word)
table(self$Parag)
table(self$Number)
table(self$Coding)
table(self$Auto)
table(self$Math)
table(self$Mechanic)
table(self$Elec)
table(self$AFQT)

table(self$Esteem81_1)
table(self$Esteem81_2)

#omit missing values 
self <- na.omit(self)

#rename variables for simplicity of interpretation 
self <- rename(self, worth = Esteem87_1)
self <-rename(self, goodqual = Esteem87_2)
self <-rename(self, failure = Esteem87_3)
self <-rename(self, well = Esteem87_4)
self <-rename(self, proud = Esteem87_5)
self <-rename(self, positive = Esteem87_6)
self <-rename(self, satisfied = Esteem87_7)
self <-rename(self, respect = Esteem87_8)
self <-rename(self, useless = Esteem87_9)
self <-rename(self, nogood = Esteem87_10)

#reverse the scale 
self$worth <- car::recode(self$worth, "1=4; 2=3; 3=2; 4=1")
self$goodqual <- car::recode(self$goodqual, "1=4; 2=3; 3=2; 4=1")
self$well <- car::recode(self$well, "1=4; 2=3; 3=2; 4=1")
self$positive <- car::recode(self$positive, "1=4; 2=3; 3=2; 4=1")
self$satisfied <- car::recode(self$satisfied, "1=4; 2=3; 3=2; 4=1")

#double-check to see if the values are reversed 
table(self$worth)
table(self$goodqual)
table(self$well)
table(self$positive)
table(self$satisfied)

#create a subset of the esteem data to simplify analysis 
data.esteem <- self[,37:46]

Summary: After cleaning the variables, the remaining sample size was n = 2345. I began by dropping negative values from both the income87 and height05 variable. For income, I reassigned respondents that answered "0" to the average (13523). I concluded by dropping "uncodeable" values in the job05 variable. The dataset contains two categorical variables and 44 numeric variables. 

## EDA
#1987 plots 
p1 <- data.esteem %>%
  ggplot(aes(worth, fill= cut(worth,100))) +
  geom_histogram(binwidth=1, show.legend=FALSE) +
  theme(plot.title = element_text(size=8)) +
  labs(title = 'I am a person of worth', x = "", y = "") +
  scale_y_continuous(limits=c(0,2000))

p2<- data.esteem %>%
  ggplot(aes(goodqual, fill= cut(goodqual,100))) +
  geom_histogram(binwidth=1, show.legend=FALSE) +
  theme(plot.title = element_text(size=8)) +
  labs(title = 'I have good qualities', x = "", y = "") +
  scale_y_continuous(limits=c(0,2000))

p3 <- data.esteem %>%
  ggplot(aes(failure, fill= cut(failure,100))) +
  geom_histogram(binwidth=1, show.legend=FALSE) +
  theme(plot.title = element_text(size=8)) +
  labs(title = 'I feel like a failure', x = "", y = "") +
  scale_y_continuous(limits=c(0,2000))

p4 <- data.esteem %>%
  ggplot(aes(well, fill= cut(well,100))) +
  geom_histogram(binwidth=1, show.legend=FALSE) +
  theme(plot.title = element_text(size=8)) +
  labs(title = 'I do things as well as others', x = "", y = "") +
  scale_y_continuous(limits=c(0,2000))

p5 <- data.esteem %>%
  ggplot(aes(proud, fill= cut(proud,100))) +
  geom_histogram(binwidth=1, show.legend=FALSE) +
  theme(plot.title = element_text(size=8)) +
  labs(title = 'I do not have much to be proud of', x = "", y = "") +
  scale_y_continuous(limits=c(0,2000))

p6 <- data.esteem %>%
  ggplot(aes(positive, fill= cut(positive,100))) +
  geom_histogram(binwidth=1, show.legend=FALSE) +
  theme(plot.title = element_text(size=8)) +
  labs(title = 'I take a positive attitude towards myself', x = "", y = "") +
  scale_y_continuous(limits=c(0,2000))

p7 <- data.esteem %>%
  ggplot(aes(satisfied, fill= cut(satisfied,100))) +
  geom_histogram(binwidth=1, show.legend=FALSE) +
  theme(plot.title = element_text(size=8)) +
  labs(title = 'I am satisfied with myself', x = "", y = "") +
  scale_y_continuous(limits=c(0,2000))

p8 <- data.esteem %>%
  ggplot(aes(respect, fill= cut(respect,100))) +
  geom_histogram(binwidth=1, show.legend=FALSE) +
  theme(plot.title = element_text(size=8)) +
  labs(title = 'I wish I could have more respect for myself', x = "", y = "") +
  scale_y_continuous(limits=c(0,2000))

p9 <- data.esteem %>%
  ggplot(aes(useless, fill= cut(useless,100))) +
  geom_histogram(binwidth=1, show.legend=FALSE) +
  theme(plot.title = element_text(size=8)) +
  labs(title = 'I feel useless', x = "", y = "") +
  scale_y_continuous(limits=c(0,2000))

p10 <- data.esteem %>%
  ggplot(aes(nogood, fill= cut(nogood,100))) +
  geom_histogram(binwidth=1, show.legend=FALSE) +
  theme(plot.title = element_text(size=8)) +
  labs(title = 'I think I am no good', x = "", y = "") +
  scale_y_continuous(limits=c(0,2000))

#compile plots; 5 on each page

pacman::p_load(grid, gridExtra, gtable)
g1<- grid.arrange(p1, p2, p3,p4,p5, ncol=2, 
                  left = textGrob("Count"),
                  bottom=textGrob("Esteem Score"))
g2<- grid.arrange(p6, p7,p8,p9,p10, ncol=2,
                  left = textGrob("Count"),
                  bottom=textGrob("Esteem Score"))

#paste compiled plots onto one document 
multi.page <- ggpubr::ggarrange(g1, g2,
                                nrow = 1, ncol =1)

Summary: Participants in the sample have mixed self-esteem scores. When asked about their worth, participants reported higher self-esteem scores; however, when asked about if they felt like a failure, participants reported lower self-esteem scores. In terms of distributions, positively worded self-esteem questions have similar distributions-- with most of the responses clustered at 3 and 4. Similarly, negatively worded self-esteem questions follow similar distributions-- with most of the responses clustered at 3 and 4. However, it is important to note that in two of the negatively-worded esteem questions, ~500 respondents reported "2", thus contributing to the spread of the distribution. Overall, the self-esteem questions follow similar distributions. I foresee the formation of two clusters, given the distributions. 

## Intercorrelations between Esteem Measures

res <- cor(data.esteem)
res

#produce correlation plot 
pacman::p_load(corrplot)
corrplot(res)


Summary: As expected, the positively worded self-esteem scores are positively correlated. For example,"I am a person of worth" and "I have a number good qualities" have a positive linear relationship of 0.700. Similarly, "I am satisfied with myself" and "I take a positive attitude towards myself and others" have a positive linear relationship of 0.605. With respect to the negatively-worded esteem questions, I observed similar patterns. "I think I am no good at all" and "I feel useless at times" have a moderate positive relationship of 0.579. Overall, many of the variables are positively correlated. 

## PCA on 10 esteem measurements 

### PC1 and PC2 loadings 
pc.10<- prcomp(data.esteem, scale=FALSE)

loading <- pc.10$rotation
knitr::kable(loading)


PC 1 scores: [0.232, 0.243, 0.278, 0.258, 0.312, 0.313, 0.301, 0.393, 0.402, 0.377]; 

PC 2 scores: [-0.371, -0.3364, -0.156, -0.320, -0.143, -0.211, -0.164, 0.330, 0.578, .0.260]

Summary: Yes, they are unit vectors. Yes, they are uncorrelated. 

### Interpretation of PC1 and PC2 scores 

Summary: PC1 is proportional to the total sum of the ten esteem scores. PC2 is the difference between the esteem scores (orthogonal). 

### Formula for calculating PC1 scores 

Response: PC1 Score_respondent_1 = 0.62(0.23) + -0.40(0.24) + 0.42(0.27) + -0.50(0.26) + -1.52(0.31) + 0.40(0.31) + 0.282(0.29) + -0.097(0.39) + -0.062(0.39) + 0.63(0.37) = -0.057

### Are PC1 and PC2 uncorrelated? 
pccor <- round(cov(pc.10$x), 10)


Response: Yes, PC1 and PC2 are uncorrelated. 

### Proportion of Variance 

#table of variance, sd, and cumulative proportion 
knitr::kable(summary(pc.10)$importance)


#PVE scree plot 
PVE <- plot(summary(pc.10)$importance[2, ], 
            ylab = "Variance %",
            xlab="Number of PC's",
            pch = 16,
            main="Scree Plot of PVE for Self-Esteem Scores | 1987")

Summary: PC1 score accounts for 46% of the total variance in the ten self-esteem scores. PC2 score captures 13% of the total variation in the ten self-esteem scores. Overall, the two PC scores capture 59% of the variance. 

### Cumulative Proportion of Variance 

CPVE <- plot(summary(pc.10)$importance[3, ], pch=16, ylab="Cumulative PVE",
             xlab="Number of PC's",
             main="Scree Plot of Cumulative PVE for Self-Esteem Scores")

Summary: 59% of the total variability is explained by the first two principal components.

### Bi-plot of PC scores 
biplot<- biplot(pc.10, # choices=c(1,2),
                choices = c(1,2),
                xlim=c(-.08, .08),
                ylim=c(-.08, .08),
                main="Biplot of the PC's")
abline(v=0, h=0, col="red", lwd=2)

#load packages 
library(ggfortify)
biplotgg <- autoplot(pc.10, data = self,
                     choices = c(1,2),
                     loadings = TRUE, loadings.colour = 'blue',
                     loadings.label = TRUE, loadings.label.size = 3)

Summary: Negatively worded esteem questions, such as "I wish I could have more respect for myself" and "I think I am no good at all" are correlated. Similarly, positively worded esteem questions, such as "I am a person of worth" and "I have a number of good qualities are highly correlated." PC1 scores seem to be related to positively worded esteem scores, whereas PC2 relates to negatively worded esteem scores. However, it is important to note that PC1 also includes negatively worded esteem scores. In sum, scores 1-7 cluster downwards and PC scores 8-10 cluster upwards. 

## K-means Clustering

### Optimal number of clusters 

self.kmeans <- kmeans(data.esteem, 2, nstart=25)

#total within-cluster sum of square 
wss <- function(data.esteem, k) {
  kmeans(data.esteem, k, nstart = 10)$tot.withinss
}

#plot wss for the following clusters 
k.values <- 2:15

#produce wss for specified clusters 
wss_values <- sapply(k.values,
                     function(k) kmeans(data.esteem[,-1], centers = k)$tot.withinss)

#plot clusters 
plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#optimal number of clusters 
library(factoextra)
fviz_nbclust(data.esteem[,-1], kmeans, method = "wss")

Summary: We can conclude that the the optimal number of clusters is three. 

### Characteristics of each cluster 

#produce means of the clusters 
print(self.kmeans)

Summary: Cluster one captures respondents with lower reported self-esteem scores, whereas cluster two captures respondents with higher self-esteem scores. However, cluster two also captures respondents with lower self-esteem scores. 

### Visualize clusters 
#plot to capture clusters 
fviz_cluster(self.kmeans, data = data.esteem)

Summary: The plot displays two distinct clusters; however, there is some overlap between the two clusters.

## What factors relate to self-esteem?

### Data Prep and Modelling
#logged income variable 
self$Income_log <- log(self$Income87)

#height variable
self$height <- paste(self$HeightFeet05, self$HeightInch05)

#remove spaces 
self$height <-gsub("[[:space:]]", "", self$height)

#convert to numeric 
self$height <- as.numeric(self$height) 

#create BMI variable 
self$bmi <- self$Weight05 / (self$height^2)

#set the following variables as factors 

self$Imagazine<- as.factor(self$Imagazine)
self$Ilibrary<- as.factor(self$Ilibrary)

data.esteem$pc <- (pc.10$x[,1])

#PC SVABS variable 
data.svab <- self[,16:25]
pc.svab<- prcomp(data.svab, scale=FALSE)



#subset data 
data.self <- data.table(pc1 = pc.10$x[,1],
                        pc.svab = pc.svab$x[,1], 
                        magazine = self$Imagazine,
                        library = self$Ilibrary, 
                        newspaper = self$Inewspaper,
                        bmi = self$bmi,
                        income = self$Income_log,
                        motheredu = self$MotherEd,
                        fatheredu = self$FatherEd,
                        familyincome = self$FamilyIncome78,
                        gender= self$Gender,
                        edu = self$Education05,
                        job= self$Job05)

#Model 1: Sociodemographic characteristics 
m1 <- lm(pc1 ~ gender + edu + income, data=data.self) 

#Model 2: Additional sociodemographic characteristics 
m2 <- lm(pc1 ~ gender + edu + income + motheredu + fatheredu + familyincome + job + bmi,  data=data.self) 

#Model 3: remaining variables 
m3 <- lm(pc1 ~ gender + edu + income + motheredu + fatheredu + familyincome + job + bmi + magazine + library + newspaper + pc.svab, data=data.self) 

#Model 4: significant predictors 
m4 <- lm(pc1 ~ gender + edu + income + motheredu + job + bmi+ newspaper + pc.svab, data=data.self) 
```

Summary: The final model is number four. In future analysis, I would collapse the job category given the non-significant results across job types. Model four contains variables that predict self-esteem scores. The variables in model four are significant. 

### Diagnostics

library(broom)
model.diag.metrics <- augment(m4)
residualplot<- plot(m4)

Summary: I used a step-wise approach to narrow non-significant predictors from the model. As previously stated, I would collapse the job category, given some of  the non-significant categories. Overall, the model does not meet the assumptions for a linear model. The residuals in the first plot do not have a horizontal line. Second, the normal Q-Q plot, demonstrates that the residuals are not normally distributed. Third, the scale-location plot does not follow a horizontal line, thus suggesting heteroskedasticity. 

### Summary of Findings

Summary: The models demonstrate that gender, education, income, mother's education, bmi, newspaper, and intelligence predict self-esteem scores. However, it is important to note that the model fit is rather weak, as the model only captured 13 percent of the total proportion of the variance.
