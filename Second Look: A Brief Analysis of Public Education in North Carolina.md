### 3. Second Look

North Carolina is lucky to be home to three distinct and unique regions- the  to the west, the central piedmont, and the eastern coastal plains. For my second look at the data, I will be looking at the regions of the 115 school districts. Here, I want to know if the location of the school district in North Carolina has any effect on their proficiency scores, and if scores across North Carolina's three regions are significantly different at all. My prediction is that scores in the mountainous areas will tend to be lower than that of the piedmont, which I expect to be the highest of the state. As mentioned in the abstract, the piedmont is the most populous part of the state, and many of North Carolina's major cities are located in this region. Also, previous educational experiences have taught me that mountainous areas tend to be poorer because of the difficult terrain which hinders large growth of communities and agricultural industries. Mountain communities are usually created to be near mines (for example, coal mines), which would be the source of their income. However, as coal demands dwindled over the years and as other industries became more lucrative, these towns unfortunately become poorer and lesser-managed. Although I previously investigated that a school district's total revenue may possibly not have the positive impact on proficiency scores that I was expecting, the number of economically-disadvantaged students did have the impact I thought it might have. However, an initial guess is never good enough to conclude anything definitive, and so in this section I will be analyzing any differences between the proficiency scores of the three regions. 

To do this, I will be checking the conditions of normality using the Shapiro-Wilkes Test to ensure that I can use the data for my next step- two-sample t-tests. For each of the three regions, I will only be analyzing math and reading proficiency scores at the high school level for time's sake. I will be conducting the two-sample t-test six times- for math and reading scores, I will compare the mountain region to the piedmont region, the piedmont region to the coastal region, and finally the coastal region to the mountain region. For each of the six tests, our null hypothesis will be that there is no significant difference between the two compared regions' scores, and the alternate hypothesis will be that there does exist a statistically significant difference.

```{r Shapiro Wilk Tests, message=FALSE, warning=FALSE}
mountain <- education3 %>% filter(Region=="Mountain")

mountain <- mountain %>% 
  mutate_if(is.character, as.numeric)

mountain_HSmath <- shapiro.test(mountain$High.School.Math.Proficiency)
print(mountain_HSmath)

mountain_HSreading <- shapiro.test(mountain$High.School.Reading.Proficiency)
print(mountain_HSreading)

#################################################################

piedmont <- education3 %>% filter(Region=="Piedmont")

piedmont <- piedmont %>% 
  mutate_if(is.character, as.numeric)

piedmont_HSmath <- shapiro.test(piedmont$High.School.Math.Proficiency)
print(piedmont_HSmath)

piedmont_HSreading <- shapiro.test(piedmont$High.School.Reading.Proficiency)
print(piedmont_HSreading)

#################################################################

coastal <- education3 %>% filter(Region=="Coastal")

coastal <- coastal %>% 
  mutate_if(is.character, as.numeric)

coastal_HSmath <- shapiro.test(coastal$High.School.Math.Proficiency)
print(coastal_HSmath)

coastal_HSreading <- shapiro.test(coastal$High.School.Reading.Proficiency)
print(coastal_HSreading)

```

<img width="573" height="234" alt="Screenshot 2025-07-24 215336" src="https://github.com/user-attachments/assets/89d9ec06-2258-4529-a4b0-74565dfacad9" />

<img width="549" height="247" alt="Screenshot 2025-07-24 215403" src="https://github.com/user-attachments/assets/cd0cc25e-9962-4930-9c57-2d61a7f8f43e" />

<img width="552" height="250" alt="Screenshot 2025-07-24 215424" src="https://github.com/user-attachments/assets/004ede86-cb9e-4d4a-89ba-ffd37ef03e02" />

To ensure the validity of the two sample t-test, the distributions of math and reading proficiency grades must be approximately normal for all three regions. I assessed the normality assumption using the Shapiro-Wilk test. Since all p-values are greater than 0.05, we cannot reject the null hypothesis that the data does come from a normally distributed source. All w-statistics are also high, offering more reassurance that our data follows a normal distribution. This all assures that we can use all regions' data for our two-sample t-tests. 

```{r Two Sample T-Tests}
MP_HS_math <- t.test(mountain$High.School.Math.Proficiency, piedmont$High.School.Math.Proficiency)
print(MP_HS_math)

MP_HS_reading <- t.test(mountain$High.School.Reading.Proficiency, piedmont$High.School.Reading.Proficiency)
print(MP_HS_reading)

###############################

PC_HS_math <- t.test(piedmont$High.School.Math.Proficiency, coastal$High.School.Math.Proficiency)
print(PC_HS_math)

PC_HS_reading <- t.test(piedmont$High.School.Reading.Proficiency, coastal$High.School.Reading.Proficiency)
print(PC_HS_reading)

###############################

CM_HS_math <- t.test(coastal$High.School.Math.Proficiency, mountain$High.School.Math.Proficiency)
print(CM_HS_math)

CM_HS_reading <- t.test(coastal$High.School.Reading.Proficiency, mountain$High.School.Reading.Proficiency)
print(CM_HS_reading)

```
<img width="1030" height="491" alt="Screenshot 2025-07-24 215707" src="https://github.com/user-attachments/assets/244494b7-fef9-4c86-85b3-d986dde4597b" />

For our first set of t-tests, I analyzed the differences in math and reading proficiency scores between the mountain and piedmont regions. As stated in the 'Second Look' abstract section, our null hypothesis states that their is no significant difference between the two. For math, the t-statistic is 5.0929, while for reading the t-statistic is 3.7482; for both, a noticeable difference has been evaluated. Since both numbers are positive, this means that the mean of the proficiency scores of the mountains are higher than that of the piedmont. With both having a p-value of far less than our alpha-level of 0.05 (3.242e-6, 0.0003837), we can reject our null hypothesis. Furthermore, we find evidence to support that our alternate hypothesis (stating that there is a singnificant difference) is true. 

<img width="1012" height="493" alt="Screenshot 2025-07-24 215748" src="https://github.com/user-attachments/assets/d97d69b6-e578-46cb-b1ab-41a3d537c6dc" />

For the second set of t-tests, I compared the piedmont and coastal regions. The math proficiency t-statistic is -1.0475, while the reading proficiency t-statistic is 1.5228. The difference noted here is a bit smaller than our previous set of t-tests, but is still notable nonetheless. However, it appears that the p-values of 0.298 and 0.1317 are much larger than our alpha level of 0.05, meaning that we fail to reject our null hypothesis and that we fail to find evidence to support that our alternate hypothesis is true. Basically, we cannot find any statistically significant difference between the piedmont's proficiency scores and the coastal region's scores, so we have to stick with the idea that they are pretty much similar.

<img width="1026" height="501" alt="Screenshot 2025-07-24 215856" src="https://github.com/user-attachments/assets/9284b39a-d158-4710-97d1-1d644e788a97" />

For the last set of t-tests, I compared the coastal and mountain regions' scores. The math proficiency t-statistic is -4.0768, and the reading proficiency t-statistic is -5.5924. Once again, a notable difference in mean scores have been noted, and the mountain region has the higher score (the negative t-statistic indicates that the second mean is higher). With p-values that are way less than the alpha level of 0.05, we reject our null hypothesis and find evidence to support our alternate hypothesis. 

My predictions seem to be far from the truth, according to the tests that I have conducted. The mountain region, much to my surprise, has the highest proficiency scores of the state, all with statistically significant differences when compared to the piedmont and the coastal region. In fact, nothing seems to support my previous theory, that because the mountain region is probably poorer than the rest of the state that their scores would be the lowest. It's important for me to realize that my prediction was wrong, but it's also important to know why- to do so, I will analyze a map of North Carolina with every county displaying the number of economically disadvantaged students in the district. 

<img width="1803" height="806" alt="Screenshot 2025-07-24 220124" src="https://github.com/user-attachments/assets/20e4b89e-d1c2-4f09-9e7a-1acad46e0614" />

This map has completely surprised me- it seems that not only are the mountains not home to the poorest districts in the state, but rather that the eastern coastal plains and the southeastern part of North Carolina has the most economically-disadvantaged students. But not only does this strongly disprove my theory on the mountain region, it also offers some evidence to disprove that the more economically disadvantaged students there are, the lower their scores. Perhaps this t-test or the linear regression was simply a case of correlation and not causation (they kind of conflict with one another), but a big takeaway that I can see from these is that predicting what factors effect a school district's scores may not be as straightforward or as easy as it may initially seem.

# Overall Conclusion

In both my initial and second look, it appears that what I predicted to be a cause for higher or lower proficiency scores were not accurate. Total revenue does not indicate that a school would have more money that would lead to higher scores, the mountain region is not as poor as I thought it would be, and just because a district has more economically disadvantaged students does not mean that their scores are going to be lower. Perhaps my initial guesses were influenced by the very thing that drove my analysis- that I knew that I didn't know the full picture of my home state's public schools. I admit that I did have personal bias towards the piedmont region- after all, that's where I was born and raised, and I've always thought that our schools have been better than a lot of other options in North Carolina. While I can't conclude absolute certainty of my findings from this analysis, it does give a lot of food for thought. 

So is there anything that we can do? It's hard for me to say that there's a clear solution when I myself don't know the full picture yet. It would be easy to say that a school district should allocate more of their funds to instruction and providing more resources to lower-income children, but there is always something there that can complicate this. However, I do think this will be a possible start to a solution. As for students who come from low-income backgrounds, there are possible ways to supplement learning for free. Using free online resources, like YouTube and Khan Academy, are great ways to learn concepts for better understanding. If possible, asking for the teacher for extra help during the school day can eliminate the need to stay after school. There are major problems with these proposed fixes, though, that I must acknowledge. My first solution would not work for those who either don't have access to their own technology, their family doesn't have the technology, or if their household doesn't have reliable internet connections. Also, I mentioned that time is such an important factor, and for some, this still may not be possible. The second solution is not as problematic as the first, but perhaps the teacher doesn't have enough time to cater towards every student's individual needs, maybe because there are so many students and not enough staff. 

It may be difficult to find a perfect solution (or even the best) and it may be even harder to actually implement it, but it's important to remember two things. Firstly, school test scores are not strict indicators of a person's ability to succeed in life. Test scores, on both an individual and district level, absolutely has nothing to do with one's worth. Secondly, any step towards improvement, no matter how small or seemingly insignificant, is important. Grandiose solutions won't appear without taking those first few starter steps. I hope that by analyzing all of this data that I can help in being part of one of those first steps through telling a story. 


Note: Here are some more visualizations for further thought. 

<img width="1793" height="796" alt="Screenshot 2025-07-24 220927" src="https://github.com/user-attachments/assets/0b4e954c-7aff-41ae-86bd-1050713a8a92" />

<img width="1826" height="808" alt="Screenshot 2025-07-24 220847" src="https://github.com/user-attachments/assets/44ad9e95-dcf4-4fdb-be4d-f5c7386a9356" />

<img width="1334" height="789" alt="Screenshot 2025-07-22 100900" src="https://github.com/user-attachments/assets/2fd97ffd-22d0-48e5-b9ec-6d9c28f545b2" />



