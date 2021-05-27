#Name: Alejandra Regla-Vargas 
#Course: STAT 571: Modern Data Mining 
#Assignment: Exploratory Data Analysis

## Data preparation

#Set working directory 
getwd() #Find working directory 
setwd("~/Desktop/Spring 2021/STAT 571/data") #Get working directory 

#Load data 
surveyresults <- read.csv("data/Survey_results_final.csv", header=T, stringsAsFactors = FALSE)

#Clean variables 
names(surveyresults) #view variable names 
summary(surveyresults) #view missing values 
str(surveyresults) #view data format
surveyresults[1:5,] #view format of data

mturk <- surveyresults %>% #reference advanced R tutorial 
  select(Answer.Age,
         Answer.Gender, 
         Answer.Education,
         Answer.HouseHoldIncome, 
         Answer.Sirius.Radio,
         Answer.Wharton.Radio,
         WorkTimeInSeconds) %>% 
  rename(age = Answer.Age,
         gender =  Answer.Gender,
         education = Answer.Education,
         income = Answer.HouseHoldIncome, 
         sirius = Answer.Sirius.Radio,
         wharton = Answer.Wharton.Radio,
         worktime = WorkTimeInSeconds)

#Address missing values 
#Check for NAs
skimr::skim(mturk)

#1. age variable to be numeric (na = 4)
mturk$age <- as.numeric(mturk$age)
head(mturk$age)

mturk %>%
  count(age)

#2. gender variable, no issues
mturk %>%
  count(gender)

mturk$gender[mturk$gender == ''] <- NA

#3. education variable, address select one respondents (na =19)
mturk %>%
  count(education)

#recode education variable 
mturk$education <- mturk$education %>% #reference advanced R tutorial 
  rename('Less than 12 years; no high school diploma' = 'hs or less',
         'High school graduate (or equivalent)' = 'hs grad or less',
         'Some college, no diploma; or Associate’s degree' = 'some college',
         'Bachelor’s degree or other 4-year degree' = 'college degree or more',
         'Graduate or professional degree' = 'grad or prof degree')

mturk$education[mturk$education == 'select one'] <- NA

#4. household income, address (na = 6) 
mturk %>%
  count(income)

mturk$income[mturk$income == ''] <- NA

#5. sirius variable, (na = 5)
mturk %>%
  count(sirius)

mturk$sirius[mturk$sirius == ''] <- NA

#6. wharton variable, (na = 4)
mturk %>%
  count(wharton)

mturk$wharton[mturk$wharton == ''] <- NA


#7. worktime variable, no issues 
mturk %>%
  count(worktime)

#drop NAs from dataset 
mturk <- na.omit(mturk)


summary(mturk) 

#Summary: Before addressing NAs, I made sure the variables in the dataset had the correct variable types. I converted "age" into a numeric variable, and in the process, addressed incorrectly coded values, such as responses with characters, and quotation marks. For gender, I assigned "na" values to respondents that did not select a gender. Limited options in gender may have resulted in the missing values (e.g., non-gender conforming, transgender).Following gender, I assigned "na" values to respondents that selected "select one" as their level of education. I presume these respondents 1) forget to select a level of education or 2) intentionally omitted. It may have been that those that forgot to select a level of education were not prompted to select a level of education and were allowed to move onto the next portion of survey. For income, I replaced non-responses with "NAs."I presume that respondents did not provide their income information due to the perceived intrusiveness of the question. Respondents that earn too much or too little may not feel comfortable disclosing how much money they earn. Similar to income, I replaced the missing values in the "sirius" and "wharton" variables with NAs. Given the small number of missing values, I presume that similar to education, respondents forgot to select an option and were allowed to move on, as the survey may have not required them to provide a response. I concluded by omitting the NAs from my dataset resulting in a sample size of (n = 1727). 

#Descriptive statistics 
#packages 
install.packages("gridExtra")
install.packages("grid")


#sample descriptives 
skimr::skim(mturk)

genderplot <-ggplot(mturk, aes(x = factor(gender))) +
  geom_bar() + theme_classic() + coord_flip()

eduplot<- ggplot(mturk, aes(x = factor(education))) +
  geom_bar() + theme_classic() + coord_flip()

incomeplot <- ggplot(mturk, aes(x = factor(income))) +
  geom_bar() + theme_classic() + coord_flip()

siriusplot <- ggplot(mturk, aes(x = factor(sirius))) +
  geom_bar() + theme_classic() + coord_flip()

whartonplot<- ggplot(mturk, aes(x = factor(wharton))) +
  geom_bar() + theme_classic() + coord_flip()

#gender, education, and income plot
grid.arrange(genderplot, eduplot, incomeplot, ncol=2)

#sirius and wharton plot 
grid.arrange(siriusplot,whartonplot, ncol=2)


#stratified descriptives

#age x gender
age.gender.plot <- ggplot(mturk, aes(x = gender, y = age)) +
  geom_bar(stat = "identity") +
  coord_flip() 

#gender x income
gender.income.plot <- ggplot(mturk, aes(x = gender, fill = income)) +
  geom_bar(position = position_dodge()) +
  theme_classic() + coord_flip()

#gender x education 
gender.edu.plot <- ggplot(mturk, aes(x = gender, fill = education)) +
  geom_bar(position = position_dodge()) +
  theme_classic() + coord_flip()

#age x gender; gender x income; gender x education
grid.arrange(age.gender.plot, gender.income.plot, gender.edu.plot, ncol=2)


#gender x sirius 
gender.sirius.plot <- ggplot(mturk, aes(x = gender, fill = sirius)) +
  geom_bar(position = position_dodge()) +
  theme_classic() + coord_flip()

#gender x wharton
gender.wharton.plot <- ggplot(mturk, aes(x = gender, fill = wharton)) +
  geom_bar(position = position_dodge()) +
  theme_classic() + coord_flip()

#gender x radio 
grid.arrange(gender.sirius.plot, gender.wharton.plot, ncol=2)


#Summary of descriptives: Beginning with age, the average respondent is 30-years-old. Males are overrepresented in the sample. The majority of the respondents in the sample do not have diploma or associates degree. Following those with no diploma, those with bachelor's degree or higher are the next largest group in the sample. The majority of the respondents in the sample earn between $30,000 - $50,000. Respondents are more likely to have listened to Sirius radio than Wharton radio. 

#Summary of stratified descriptives: Male respondents are older than female respondents. Overall, male respondents earn more than female respondents; however, male respondents are more likely to earn "less than 15,000" than their female counterparts. This may be due to the overrepresentation of males in the sample. Male respondents are more likely to report "some college, no diploma; or associate's degree" than their female counterparts; however, male respondents are also more likely to report having a bachelor's degree or other 4-year degree. Males are more likely to listen sirius radio than females. Females are more likely to listen to wharton radio than males. It is important to note the overrepresentation of males in the sample. 

