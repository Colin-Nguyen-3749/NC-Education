
---
# A Brief Analysis of Public Education in North Carolina


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(GGally)

education <- read.csv("C:/Users/colin/Downloads/NC_Education_By_County_Cleaned.csv")
education <- education %>% replace(education == "NULL", NA)
education <- education %>% 
  mutate_if(is.character, as.numeric)
View(education)
summary(education)

education2 <- read.csv("C:/Users/colin/Downloads/NC Education By County - Copy(Sheet1) (1).csv")
education2 <- education2 %>% replace(education == "NULL", NA)
View(education2)
```

# Abstract

Growing up in North Carolina, I spent the majority of my life in a small town in the Piedmont region. This part of the state is between the mountain region and the coastal plains, and is characterized by its popularity for farming and development due to its fertile soils and rolling hills. The Piedmont weather is known to be milder than its counterparts, as winters in the mountains were extremely cold and summers near the coast were very humid. Historically, this is also the region where North Carolina's Gold Rush was the strongest. Perhaps these are some of the reasons why the Piedmont region is home to the majority of the state's population. Most of North Carolina's biggest and most important cities, like Charlotte, Raleigh, and Winston-Salem, are located in this region. My earliest years of childhood were spent in the outskirts of Charlotte before I moved farther away to a smaller town. However, even though I was young, I still noticed how different the schools felt. As I got older, I only realized more of these differences- the schools looked newer, they seemed to have more money, and test scores were higher. But was this all true?

In my later years of high school I was able to see more of my home state. Every time we'd drive past another school I liked to imagine what it would be like to go there every day. Many of those schools looked old and worn-down compared to the schools in my county. When I traveled more, I started to realize that I failed to think about how different the rest of the state was, and how each one had a story that I wanted it to tell me. How do these schools compare to mine? Are their test scores higher, lower, or similar? Why so? To find out, I gathered data on all 115 school districts in North Carolina to see what patterns, if any, arise. 

The research question that this paper will be exploring is as follows: what causes the differences in test scores of public schools in North Carolina? To answer this, I will be looking to see if there exists a relationship between a school's testing scores and certain factors, like school revenue, location, student-teacher ratio, fund allocation, enrollment size, the amount of counselors, and the amount of economically-disadvantaged students. Testing scores refers to the schools' reading, math, and (if available) science proficiency scores. I will also be looking at each counties' high-school college readiness score. NOTE: for all variable definitions, please look to the README file in the repository. 

# Ethical Considerations

I will be using a dataset that I compiled all of the data for, but it is important to note that all of the data came from US New and World Reports, which I do not own. I did not gather the data myself, and I do not claim any ownership over it. This is merely a personal and independent project to answer some questions that I've had for a while .I also need to mention the timeframe when this data was gathered- much of this data was collected as recently as 2023 or as far back as 2018. It is important for us to realize that during 2019-2020 many schools were affected by COVID-19, and some may have not updated their information since. So we must know that our findings here may not tell the most complete story, and there will certainly be details missing from the final picture. 

Potential stakeholders in this research include the students and families of these school districts in North Carolina, as well as educators, counselors, school board representatives and workers, and academic policymakers. All of these people can look into this data to learn more about the education that are being given to our children, and it can possibly be used to navigate solutions to help improve any unfavorable conditions. Like it did for me, it can help others put their life into a greater perspective in respect to others, and it makes one realize that lives can very substantially even in the same state. However, we must note the realism of this data, in that many families cannot simply move to a better-performing school due to extenuating circumstances, or perhaps some schools lack resources needed for improvement. We should also keep in mind that any findings are not wholly indicative of every single student's academic success- just because a school has low test scores does not mean that every student there struggles academically. This data analysis is in no way intended to shame anybody or the way they live. 

Let it be known that no students are identified and that all student data is completely anonymous and not on an individual level. 

# Analysis

###   1. Basic Summary Statistics

First, I will use the package ggplot2 to create graphical visualizations and simple descriptive methods to provide general insights into the
data of the schools across the 115 districts. Some visualizations that will appear in this research paper include a boxplot and a density plot.  Each visualization will be accompanied by a description that includes the title, the independent and/or dependent variables, and the interpretation of the graph. Here, I will be looking at the following questions: what districts have the most students? How much of North Carolina's students are considered economically disadvantaged, and how does that differ from district to district? What district has to highest minority enrollment? What district has the most resources, like revenue, teachers, and counselors?

###   Actual Analysis

First, I will use the package ggplot2 to create graphical visualizations and simple descriptive methods to provide general insights into the
data of the schools across the 115 districts. Some visualizations that will appear in this research paper include a boxplot and a density plot.  Each visualization will be accompanied by a description that includes the title, the independent and/or dependent variables, and the interpretation of the graph. Here, I will be looking at the following questions: what districts have the most students? How much of North Carolina's students are considered economically disadvantaged, and how does that differ from district to district? What district has to highest minority enrollment? What district has the most resources, like revenue, teachers, and counselors?

<img width="1056" height="723" alt="Boxplot" src="https://github.com/user-attachments/assets/ad4eb72d-2d4a-4f47-b7c8-163c59ecad8f" />

This boxplot shows the distribution of the amount of students in each of the school districts- it's amazing to see how large some of North Carolina's schools can be, with outliers reaching beyong 135,000 students, rivalling the size of a few universities. In contrast, the smallest school district has only 475 students. The median, which was found through summary() since it's hard to see on the boxplot, is around 5,776, and 50% of school districts have between 2,652 and 12,920 students (see below).

```{r summary statistics, message=FALSE, warning=FALSE}
summary(education2$Number.of.Students)
```
<img width="1191" height="103" alt="Summary" src="https://github.com/user-attachments/assets/f39a0aa7-1114-49d8-9717-312eecd8db91" />

```{r boxplot2, message=FALSE, warning=FALSE}
mini_ed1 <- education2[c(5,13,14)]
mini_ed1 <- mini_ed1 %>% 
  mutate_if(is.character, as.numeric)
# Assuming 'my_data' is your dataframe
data_long <- mini_ed1 %>% pivot_longer(cols = everything(), names_to = "variable", values_to = "value") 

ggplot(data_long, aes(y = variable, x = value, fill = variable)) + 
  geom_boxplot() + 
  labs(title = "Distribution of Minority Enrollment Students, \n Economically Disadvantaged Students, \n and Certified Teachers Across NC's SChool Districts", x = "Percent", y = "Observation") +
  theme(legend.position = "none",
        plot.title = element_text(hjust=0.5, face='bold')) + 
  scale_fill_manual(values=c("mediumseagreen","mediumaquamarine","seagreen1")) 
  
```

<img width="1033" height="718" alt="Boxplot 2" src="https://github.com/user-attachments/assets/b5204779-bfb9-4b26-9657-ec110debfa3d" />


These boxplots show the distributions of the total minority enrollment, total percent of economically disadvantaged students, and the average percent of certified teachers across all of North Carolina's school districts. The total minority enrollment distribution seems pretty balanced as the median is around 50% minority enrollment, with 50% of NC schools have lower minority enrollments and 50% having higher numbers. Half of all school districts fall between having around 30% and 70% minority enrollment. The distribution of economically disadvantaged students has a much lower interquartile range but a similar median around the 50% mark, with half of all schools having between 40% and 65% percentages. The distribution of the percent of certified teachers is much higher than the previous two, with a median around 90%. Half of all schools have between 87% and 95% of their teachers certified, with a minimum of about 75%. Notably, there is also an outlier of a school district that has less than 50% of their teachers certified.


```{r bargraph, message=FALSE, warning=FALSE}
#make a new column that mulitplies each race by the total amount and then make a bar graph 
mini_ed3 <- education2
var<- mini_ed3 %>% mutate(
  White.Amount=(Percent.of.White.Students/100)*Number.of.Students,
  Black.Amount=(Percent.of.Black.Students/100)*Number.of.Students,
  Hispanic.Amount=(Percent.of.Hispanic.Students/100)*Number.of.Students,
  Asian.Amount=(Percent.of.Asian.Students/100)*Number.of.Students,
  Two.or.More.Amount=(Two.or.More.Races/100)*Number.of.Students,
  AIAN.Amount=(American.Indian.Alaska.Native/100)*Number.of.Students,
  NHPI.Amount=(Native.Hawaiian.Pacific.Islander/100)*Number.of.Students
  )

total <- sum(var$Number.of.Students)
white <- (sum(var$White.Amount))/total
black <- (sum(var$Black.Amount))/total
hispanic <- (sum(var$Hispanic.Amount))/total
asian <- (sum(var$Asian.Amount))/total
twomore <- (sum(var$Two.or.More.Amount))/total
aian <- (sum(var$AIAN.Amount))/total
nhpi <- (sum(var$NHPI.Amount))/total

races <- c(white, black, hispanic, asian, twomore, aian, nhpi)
lbls <- c('White', 'Black', 'Hispanic', 'Asian', 'Two or More Races', 'American Indian/Alaska Native', 'Native Hawaiian/\nPacific Islander')

percentages <- round(races*100)
lbls <- paste(lbls, percentages)

race_data <- data.frame(
  races <- c(white, black, hispanic, asian, twomore, aian, nhpi),
  lbls <- c('White', 'Black', 'Hispanic', 'Asian', 'Two or More Races', 'American Indian/\nAlaska Native', 'Native Hawaiian/\nPacific Islander')
)

ggplot(race_data, aes(x=lbls, y=races, fill=as.factor(races))) + 
  geom_bar(stat="identity") +
  labs(
    title = "Distribution of Races Across NC's Students (In Percentages)",
    x = "Race of the Student",
    y = "Percent"
  ) +
  scale_fill_brewer(palette="YlOrRd") +
  theme(legend.position = "none") +
  coord_flip() +
  theme(legend.position = "none",
        plot.title = element_text(hjust=0.5, face='bold')) 
  


```

<img width="1014" height="718" alt="Boxplot 3" src="https://github.com/user-attachments/assets/19473c2c-6f68-4c3b-918c-97d008e864b9" />


Here is a visual showing the distribution of races among the student populations of North Carolina's school districts; do note how these are representing percentages. It appears that the largest racial group is comprised of white students, with the second-largest being the number of black students, followed by Hispanic students. After those three significantly larger groups comes those who identify as two or more races, then Asian students, then American Indian/Alaska Native students and finally Native Hawaiian/Pacific Islander students. This is not very surprising as it very closely matches the racial distribution of North Carolina as a state on the whole, and growing up in North Carolina's public schools, this visualization harbors no surprises for me. 


### 2. Initial Look

Does there exist a linear relationship between a school district's total revenue and its test scores? What about a linear relationship between the total number of economically disadvantages students and test scores? First, I will check if the conditions for linear regression analysis are met, like the normality of the data, the independence of the variables, and constant variance. To answer my questions, I will be using total revenue and the total percent amount of economically disadvantaged students as my dependent variable- since these are both numerical, they satisfy one of the key requirements for conducting linear regression analysis. The reason why I want to use linear regression is to see if there exists any relationship between these variables, and if so, the strength of this relationship; I want to know if these variables can be used to predict a school's districts test scores. Multiple linear regression would allow me to assess the combined influence of all predictor variables simultaneously, while also controlling for the effects of other variables. This ensures that the relationship between each predictor and the dependent variable is evaluated more accurately, without the confounding influence of the other predictors. 

For the following linear regression models, I will be using total revenue, the average percent of certified teachers, the number of full-time counselors, and the amount of economically-disadvantaged students in each school district. The reason why I will be looking into these variables first is because I predict that this variables will have a statistically significant effect on the proficiency scores of each school district. I expect that schools with more revenue to have higher scores because they have the means to provide higher-quality education, like newer textbooks, online learning programs, and study materials. Even though they may have higher expenses to cover, I'm still making an initial predication that their scores will be higher. I predict that more certified teachers will lead to higher scores, as I think that a certified teacher will be better than an uncertified teacher because they have gone through more training and may offer better help to students. I'm predicting that the more full-time counselors there are, the higher scores will be because I think more counselors can help students navigate school better, like with studying and organization. However, this is just a guess as many students don't often go to the counselor for these things, but I'm thinking that it'll have some degree of a positive effect. I also predict that the number of economically disadvantaged students is inversely related to a school district's proficiency scores with similar logic to my reasoning for my prediction for total revenue: more money means more access to better resources, and less money can be a cause to many limitations. Let's see if my predications have any accuracy to them. 


#### High School Math Proficiency
```{r HS math, message=FALSE, warning=FALSE}
test <- education %>% 
  drop_na(High.School.Math.Proficiency, Total.Revenue, Average.Percent.of.Certified.Teachers)
nrow(test)

model <- lm(High.School.Math.Proficiency ~ Total.Revenue + Average.Percent.of.Certified.Teachers + Economically.Disadvantaged + Number.of.Full.Time.Counselors, test)
plot(model, which=c(1, 2))

```
<img width="1110" height="688" alt="RF1" src="https://github.com/user-attachments/assets/beeff546-f0d9-48ed-9538-9c3cb7cca587" />

<img width="1095" height="669" alt="QQ1" src="https://github.com/user-attachments/assets/ed2ebb32-cd0a-430e-92bc-35a3c542082e" />



```{r HS Math LR, message=FALSE, warning=FALSE}
summary(model)

```
<img width="1134" height="517" alt="Screenshot 2025-07-18 131424" src="https://github.com/user-attachments/assets/eee190d9-aca7-466c-b099-7b7ae63439fc" />


#### High School Reading Proficiency

```{r HS Reading, message=FALSE, warning=FALSE}
test2 <- education %>% 
  drop_na(High.School.Reading.Proficiency, Total.Revenue, Average.Percent.of.Certified.Teachers)
nrow(test2)

model2 <- lm(High.School.Reading.Proficiency ~ Total.Revenue + Average.Percent.of.Certified.Teachers + Economically.Disadvantaged + Number.of.Full.Time.Counselors, test2)
plot(model2, which=c(1, 2))
```
<img width="1088" height="710" alt="RF2" src="https://github.com/user-attachments/assets/1ed3278f-b748-4570-ae75-4d29f8adcc18" />

<img width="1089" height="675" alt="QQ2" src="https://github.com/user-attachments/assets/a02988a3-df39-40ba-becb-7fda1199b843" />


```{r HS Reading LR, message=FALSE, warning=FALSE}
summary(model2)
```
<img width="887" height="510" alt="Screenshot 2025-07-18 131509" src="https://github.com/user-attachments/assets/ef654fcc-8854-453d-81b1-c7be6962e56b" />


#### Middle School Math Proficiency

```{r MS Math, message=FALSE, warning=FALSE}
test3 <- education %>% 
  drop_na(Middle.School.Math.Proficiency, Total.Revenue, Average.Percent.of.Certified.Teachers)
nrow(test3)

model3 <- lm(Middle.School.Math.Proficiency ~ Total.Revenue + Average.Percent.of.Certified.Teachers + Economically.Disadvantaged + Number.of.Full.Time.Counselors, test3)
plot(model3, which=c(1, 2))

```
<img width="1058" height="688" alt="RF3" src="https://github.com/user-attachments/assets/bdd7b7ad-f8ef-434f-9b9f-4ff4e0ec7a54" />

<img width="1097" height="654" alt="QQ3" src="https://github.com/user-attachments/assets/5f6a41e2-47c7-4e44-854c-0924f94d2d71" />


```{r MS Math LR, message=FALSE, warning=FALSE}
summary(model3)
```

<img width="883" height="508" alt="Screenshot 2025-07-18 131808" src="https://github.com/user-attachments/assets/56a6a89c-8081-49a2-9b68-82d05e71f81e" />


#### Middle School Reading Proficiency

```{r MS Reading, message=FALSE, warning=FALSE}
test4 <- education %>% 
  drop_na(Middle.School.Reading.Proficiency, Total.Revenue, Average.Percent.of.Certified.Teachers)
nrow(test4)

model4 <- lm(Middle.School.Reading.Proficiency ~ Total.Revenue + Average.Percent.of.Certified.Teachers + Economically.Disadvantaged + Number.of.Full.Time.Counselors, test4)
plot(model4, which=c(1, 2))
```
<img width="1027" height="688" alt="RF4" src="https://github.com/user-attachments/assets/54ae8d1d-ce11-42c8-9c30-a54c5711a0f8" />

<img width="1039" height="673" alt="QQ4" src="https://github.com/user-attachments/assets/734131de-aebb-4ebc-bd74-26df52e1db3c" />


```{r MS Reading LR, message=FALSE, warning=FALSE}
summary(model4)
```
<img width="904" height="511" alt="Screenshot 2025-07-18 131854" src="https://github.com/user-attachments/assets/b2a1cd7e-e0cb-4d53-b37b-37cf3d715201" />


#### Elementary School Math Proficiency

```{r Elem Math, message=FALSE, warning=FALSE}
test5 <- education %>% 
  drop_na(Elementary.School.Math.Proficiency, Total.Revenue, Average.Percent.of.Certified.Teachers)
nrow(test5)

model5 <- lm(Elementary.School.Math.Proficiency ~ Total.Revenue + Average.Percent.of.Certified.Teachers + Economically.Disadvantaged + Number.of.Full.Time.Counselors, test5)
plot(model5, which=c(1, 2))
```
<img width="1048" height="663" alt="RF5" src="https://github.com/user-attachments/assets/6232ba89-0568-49df-840e-abe2a91f2541" />

<img width="1071" height="654" alt="QQ5" src="https://github.com/user-attachments/assets/39cfa040-7324-4749-ade4-03878b820205" />


```{r Elem Math LR, message=FALSE, warning=FALSE}
summary(model5)
```
<img width="892" height="507" alt="Screenshot 2025-07-18 131945" src="https://github.com/user-attachments/assets/4278c06a-0fb9-4cc1-be04-d60d1458fcb6" />


#### Elementary School Reading Proficiency

```{r Elem Reading, message=FALSE, warning=FALSE}
test6 <- education %>% 
  drop_na(Elementary.School.Reading.Proficiency, Total.Revenue, Average.Percent.of.Certified.Teachers)
nrow(test6)

model6 <- lm(Elementary.School.Reading.Proficiency ~ Total.Revenue + Average.Percent.of.Certified.Teachers + Economically.Disadvantaged + Number.of.Full.Time.Counselors, test6)
plot(model6, which=c(1, 2))
```
<img width="1047" height="665" alt="RF6" src="https://github.com/user-attachments/assets/0063b86f-edda-4012-85e8-945fa6663d2f" />

<img width="1025" height="660" alt="QQ6" src="https://github.com/user-attachments/assets/ca41c4fd-0ef8-44ac-864a-c711f2dd29e1" />


```{r Elem Reading LR, message=FALSE, warning=FALSE}
summary(model6)
```
<img width="903" height="512" alt="Screenshot 2025-07-18 132029" src="https://github.com/user-attachments/assets/5d2083f8-2017-41a6-9200-077c0cc94d69" />

For each of the linear regression models above, I checked if the variables that I picked for analysis (total revenue of the school district, the average percent of certified teachers, the amount of economically disadvantaged students, and the number of full-time counselors) were fit to use for multiple linear regression. To do this, I used the Q-Q plot to check if the data was normally distributed- the points on the graph lined up pretty well with the diagonal line overall, and so this condition is satisfied. I also used a residuals vs. fitted plot to make sure that the relationship was linear and all the data is random and independent. None of these residual plots show any signs of major homoskedasticity, and while there were a few outliers, the red line was mostly close to being flat for most of the data, so these conditions are almost met. 

In these multiple-linear regression models, I am looking to see if the school districts' total revenue, amount of certified teachers, amount of full-time counselors, and amount of economically disadvantaged students have any significant effect on their math and reading proficiency test scores across all three levels of school (elementary, middle, and high). The intercept in all of the models tells us the predicted proficiency score if all the variables are at zero (the school has zero total revenue, they have zero certified teachers, they have no full-time counselors, and no economically disadvantaged students). For middle and elementary schools, there scores are below 50%, but interestingly for high school both math and reading proficiency scores are predicted to be in the seventies if all variables are set to zero. 

All models here have at least one variable that has a p-value of less than 0.05 (the alpha level I will be using), which means that all models are statistically significant. What came as a surprise to me is that in all of the models, a one-unit increase in variables such as total revenue and the total number of economically disadvantaged students led to a decrease in predicted proficiency scores- I had expected that if a district has more total revenue, then they have more financial resources to provide their students with a higher-quality education which would lead to higher scores. However, this made me realize that total revenue only tells me how much money a school district receives and not how they allocate it- this will be analyzed later. Something that I expected to happen was that an increase in the amount of certified teachers and full-time counselors would be predicted to lead to an increase in proficiency scores. 

# Conclusion of Initial Look
In this section, I looked to see if there were any patterns that arose between the conditions of a school district and how they performed. First of all, I looked at some basic summary statistics of North Carolina's 115 school districts for a snapshot of what I'm going to be looking at. North Carolina's schools have a median of around fifty percent for both total minority enrollment and the percent of economically disadvantaged students. Most teachers are certified, but there is an outlier that's far below the usual amount of all the other districts. The largest student racial group consists of white students, followed by black student, followed by Hispanic students. Then other groups like Asian, Native American, Pacific Islander, and those who identify as two or more races are far below in numbers. 

In comparing the school district's total revenue, the amount of certified teachers and full-time counselors, and the percent of economically disadvantaged students, a pattern does seem to arise- indeed, these factors may indicate that there could be a relationship. The staff seems to have a positive effect on a district's proficiency scores across elementary, middle, and high schools. However, financial variables like the total revenue of a school and the amount of economically disadvantaged students seem to have a negative effect on proficiency scores as they rise across all grade levels. How could this be?

We also must remember that correlation does not necessarily imply causation- just because a school district has a higher amount of economically disadvantaged students does not mean that they will perform worse than others. However, there could potentially be a relationship here- we can guess that if a student is economically disadvantaged and therefore qualifies for free/reduced lunch, then their family would likely not have the means to afford their kids tutors and study materials. Perhaps their families don't have the time, resources, or ability to be able to drive their kid to after-school mentoring programs, or extracurricular activities that can benefit their children's learning. It's common for parents to have to work a lot to support their families, and so they lay responsibility to their kids to care for their siblings and the house, leaving them with less time to study and worry about extra school. While this is plausible, it certainly can't be true for everyone, but it does fuel further thought on finances and how they can affect a child's education. 


### 3. Second Look

North Carolina is lucky to be home to three distinct and unique regions- the  to the west, the central piedmont, and the eastern coastal plains. For my second look at the data, I will be looking at the regions of the 115 school districts. Here, I want to know if the location of the school district in North Carolina has any effect on their proficiency scores, and if scores across North Carolina's three regions are significantly different at all. My prediction is that scores in the mountainous areas will tend to be lower than that of the piedmont, which I expect to be the highest of the state. As mentioned in the abstract, the piedmont is the most populous part of the state, and many of North Carolina's major cities are located in this region. Also, previous educational experiences have taught me that mountainous areas tend to be poorer because of the difficult terrain which hinders large growth of communities and agricultural industries. Mountain communities are usually created to be near mines (for example, coal mines), which would be the source of their income. However, as coal demands dwindled over the years and as other industries became more lucrative, these towns unfortunately become poorer and lesser-managed. Although I previously investigated that a school district's total revenue may possibly not have the positive impact on proficiency scores that I was expecting, the number of economically-disadvantaged students did have the impact I thought it might have. However, an initial guess is never good enough to conclude anything definitive, and so in this section I will be analyzing any differences between the proficiency scores of the three regions. 

To do this, I will be checking the conditions of normality using the Shapiro-Wilkes Test to ensure that I can use the data for my next step- two-sample t-tests. For each of the three regions, I will only be analyzing math and reading proficiency scores at the high school level for time's sake. I will be conducting the two-sample t-test six times- for math and reading scores, I will compare the mountain region to the piedmont region, the piedmont region to the coastal region, and finally the coastal region to the mountain region. For each of the six tests, our null hypothesis will be that there is no significant difference between the two compared regions' scores, and the alternate hypothesis will be that there does exist a statistically significant difference.

<img width="573" height="234" alt="Screenshot 2025-07-24 215336" src="https://github.com/user-attachments/assets/89d9ec06-2258-4529-a4b0-74565dfacad9" />

<img width="549" height="247" alt="Screenshot 2025-07-24 215403" src="https://github.com/user-attachments/assets/cd0cc25e-9962-4930-9c57-2d61a7f8f43e" />

<img width="552" height="250" alt="Screenshot 2025-07-24 215424" src="https://github.com/user-attachments/assets/004ede86-cb9e-4d4a-89ba-ffd37ef03e02" />

To ensure the validity of the two sample t-test, the distributions of math and reading proficiency grades must be approximately normal for all three regions. I assessed the normality assumption using the Shapiro-Wilk test. Since all p-values are greater than 0.05, we cannot reject the null hypothesis that the data does come from a normally distributed source. All w-statistics are also high, offering more reassurance that our data follows a normal distribution. This all assures that we can use all regions' data for our two-sample t-tests. 



