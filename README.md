# Does Oscar Winning/Nomination Affect Longevity?
Authors: Tanqing Feng, Zhongyi (Jame) Guo, Junjie Lu, Yuewei Ling, Sean Tsung <br>


*The authors are listed in alphabetical order by their last names and have contributed equally to this work.*

## Summary

We developed three Cox regression models to analyze the longevity differences among three groups: actors and actresses who have won Academy Awards (winners), those who have only been nominated (nominees), and those who have never been nominated (controls). These models considered the dynamic nature of group membership and included adjustments for the following confounding effects: birth year, name changes, involvement in the drama genre, total number of films, and the number of films rated four stars. Our finding indicates that there is no significant difference in longevity among the three groups.

## Methods

Demographic characteristics are expressed as mean ± standard deviation, median (interquartile range), and range for continuous variables, and are expressed as counts (proportions) for categorical variables. Actors and actresses were divided into three groups–winners (won Academy Awards), nominees (only ever nominated), and controls (never nominated). 

We identified actors or actresses with potentially erroneous records by checking for the following cases: (i) the number of Academy Awards won exceeds the number of nominations; (ii) the year of their first win is earlier than the year they were first nominated; (iii) the year their first film was released is later than the year they were first nominated; (iv) the number of four-star films exceeds the total number of films; (v) the year of death is later than the year of their first win. Actors and actresses in the first four scenarios were removed from our analysis, whereas those in the final scenario underwent additional validation to determine whether they warranted exclusion.

Correlation analysis was conducted to explore the potential relationships between variables: sex and number of wins, sex and number of nominations, total number of films and number of nominations, drama genre and number of nominations, sex number of four-star films, and sex and number of wins.

Cox regression models were established to analyze the longevity among winners versus controls, winners versus nominees, and nominees versus controls. Baseline characteristics showing significant intergroup differences were identified as potential confounders. Categorical variables and continuous variables that we found did not violate the linearity properties (based on martingale residual plots) were included in the final model. Proportional hazard assumptions were tested through diagnostic plots (log-log and Schoenfeld residuals) and interaction term testing. Our analysis was stratified by award status to address immortal time bias, recognizing their status only after they had won or been nominated. 

All analyses were conducted using R software (version 4.1.2), employing two-tailed tests with a significance threshold set at α = 0.05.

## Results

*Data preprocessing*

No inconsistencies were detected in the number of wins, nominations, four-star films, and total number of films. However, one individual (ID 1430) had the anomaly of achieving her first win before her initial nomination, which was abnormal as one cannot win an Oscar prior to being nominated. Additionally, three individuals (ID 908, ID 1192, ID 1521) had the unusual circumstance where their first films were released after their first year of nominations, which was also abnormal because one cannot be nominated for the Academy Award without having released a film. Consequently, these four individuals were excluded from the dataset, leaving 1666 individuals for our analysis. Notably, ID 1075 should be ***Massimo Troisi***, who earned posthumous Oscars nominations, a detail confirmed by examining real-life historical records.

*Correlation Analysis*

The distribution of Academy Award wins among actors and actresses displays a similar pattern (Figure 1). For both groups, the majority have not won any awards, followed by smaller percentages who have won once or twice. A very small number of actors or actresses have won three times. Only one actress won four Academy Awards.

<div align="center"> <img src="/visualization/fig1_sex_num_win.png" /> </div>
<p align="center"> <strong>Figure 1.</strong> Stacked Percentage Bar Plot for Number of Wins by Sex </p><br>

Most actors and actresses had no nominations for an Academy Award (Figure 2). However, among the actress group, although less frequent across nomination categories, there were instances of higher nomination times (11 and 12), implying these individuals might be more established or recognized. Both distributions aligned closely, displaying right-skewness, indicating rare instances of individuals with exceptionally high nomination counts. This emphasized the rarity and challenge of earning Academy Award nominations multiple times.

<div align="center"> <img src="/visualization/fig2_sex_num_nom.png" /> </div>
<p align="center"> <strong>Figure 2.</strong> Histogram for Number of Nominations by Sex</p><br>

The positive slope of the linear regression between the number of Academy Award nominations and the total number of films suggests that as actors or actresses produced more films, their likelihood of being nominated for an Academy Award increased (Figure 3). 

<div align="center"> <img src="/visualization/fig3_films_total_num_nom.png" /> </div>
<p align="center"><strong>Figure 3.</strong> Number of Nominations and Total Number of Films</p><br>

The histogram depicting drama films occupied a larger area compared to that of non-drama films among nominated films (Figure 4), with both distributions exhibiting right skewness. One potential explanation for this trend was that narrative stories had a greater ability to engage and resonate with audiences, thus increasing their likelihood of receiving Academy Award nominations.

<div align="center"> <img src="/visualization/fig4_genre_num_nom.png" /> </div>
<p align="center"><strong>Figure 4.</strong> Histogram for Number of Nominations by Film Type</p><br>

The density plot section in the rain cloud plot section depicting the number of four-star films exhibited heavy right-skewness for both actors and actresses (Figure 5), aligning with the expectation that creating such films should be challenging. Additionally, actors had a higher median number of four-star films compared to actresses, while the interquartile range of actors surpassed that of actresses, indicating a potential sex bias in terms of film rating. Moreover, there were more outliers in the distribution of the number of four-star films among actors, suggesting that some actors might contribute many more four-star films than the other actors.

<div align="center"> <img src="/visualization/fig5_num_4star_films_sex.png" /> </div>
<p align="center"><strong>Figure 5.</strong> Rain Cloud Plot of Four-star Films by Sex</p><br>

The White actors and actresses had a higher mean and lower standard deviation of the number of times winning an Academy Award than all other ethnicities combined (Figure 6). This could imply that a race and ethnicity disparity might exist between the White ethnicity and other ethnicities in winning an Academy Award.

<div align="center"> <img src="/visualization/fig6_race_num_win.png" /> </div>
<p align="center"><strong>Figure 6.</strong> Mean Number of Wins by Race with Standard Deviation (SD)</p><br>

Among all continuous variables, only year of birth, year of the person’s first film, and age were normally distributed (Figure 7).

<div align="center"> <img src="/visualization/fig7_bar_histogram.png" /> </div>
<p align="center"><strong>Figure 7.</strong> Bar Plots and Histograms of all Variables</p> <br><br>

Compared to nominees and controls, winners were significantly older (mean age 67.4 vs. 63.1 and 66.0 years, respectively; *p<0.001*) and had higher median numbers of total films (52 vs. 40 and 33; *p<0.001*), four-star films (9 vs. 6 and 4; *p<0.001*), and nominations (median 2 vs. 1; *p<0.001*). A higher proportion of winners had changed their name (28.6% vs. 22.4% and 9.2%; *p<0.001*) and worked in the drama genre (81.9% vs. 78.5% and 72.1%; *p=0.001*) relative to the nominee and control groups (Table 1).

<p align="center"><strong>Table 1.</strong> Baseline characteristics of actors and actresses who won Academy Awards (winners), were only ever nominated (nominees), or were never nominated (controls)</p>
<div align="center"> <img src="/visualization/table1_page1.png" /> </div>
<div align="center"> <img src="/visualization/table1_page2.png" /> </div>
<div align="center"> <img src="/visualization/table1_page3.png" /> </div>

Note:
**p<0.05,* ***p<0.01,* ****p<0.001*
*Continuous variables are expressed as mean ± standard deviation, median (interquartile range), and range, and categorical variables are expressed as counts (proportions)*

Actors and actresses who win Academy Awards did not have the longer longevity compared with those who are never nominated (HR: 0.94 [0.74, 1.20]) (Table 2). Similarly, winning Academy Awards was not associated with increased longevity compared to individuals who were never nominated (HR: 0.94 [0.74, 1.20]) (Table 3), and actors and actresses who were only ever nominated for an academy award did not show significant better longevity compared with those who were never nominated (HR: 1.17 [0.99, 1.39]) (Table 4). Birth year was identified as a significant confounding factor, while other variables did not show significant associations in three scenarios. 

<p align="center"><strong>Table 2.</strong> Multivariate Cox proportional hazards model for longevity of actors and actresses who win academy awards compared to those who are only ever nominated for an academy award</p>
<div align="center"> <img src="/visualization/table2.png" /> </div>

<p align="center"><strong>Table 3.</strong> Multivariate Cox proportional hazards model for longevity of actors and actresses who win academy awards compared to those who are never nominated for an academy award</p>
<div align="center"> <img src="/visualization/table3.png" /> </div>

<p align="center"><strong>Table 4.</strong> Multivariate Cox proportional hazards model for longevity of actors and actresses who are only ever nominated for an academy award compared to those who are never nominated</p>
<div align="center"> <img src="/visualization/table4_page1.png" /> </div>
<div align="center"> <img src="/visualization/table4_page2.png" /> </div>

## Conclusion
In conclusion, this study did not find evidence to support the notion that winning or being nominated for an Academy Award confers a longevity advantage. The results suggest that the prestige and recognition associated with these accolades do not translate into increased lifespan for actors and actresses. Further research is needed to explore other potential factors that may influence longevity in this population.

## References
1. Redelmeier DA, Singh SM. Survival in Academy Award–Winning Actors and Actresses. *Annals of Internal Medicine.* 2001;134(10):955. doi:https://doi.org/10.7326/0003-4819-134-10-200105150-00009
2. Sylvestre MP, Huszti E, Hanley JA. Do Oscar Winners Live Longer than Less Successful Peers? A Reanalysis of the Evidence. *Annals of Internal Medicine.* 2006;145(5):361. doi:https://doi.org/10.7326/0003-4819-145-5-200609050-00009
