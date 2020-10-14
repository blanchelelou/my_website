---
categories:
- ""
- ""
date: "2017-10-31T22:26:09-05:00"
description: How satisfied are your employees?
draft: false
image: employee.jpg
keywords: ""
slug: magna
title: Your people matter

---
# IBM HR Analytics

![](https://www.techfunnel.com/wp-content/uploads/2018/02/Benefits-of-HR-Metrics-and-Analytics.jpg)

We will analyze a data set on Human Resource Analytics.

The [IBM HR Analytics Employee Attrition & Performance data set](https://www.kaggle.com/pavansubhasht/ibm-hr-analytics-attrition-dataset) is a fictional data set created by IBM data scientists.

Among other things, the data set includes employees' income, their distance from work, their position in the company, their level of education, etc. A full description can be found on the website.

```{r load-libraries, warning=FALSE, message=FALSE, echo=FALSE}
library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(mosaic)
library(ggthemes)
library(lubridate)
library(fivethirtyeight)
library(here)
library(skimr)
library(janitor)
library(vroom)
library(tidyquant)
library(rvest)    # scrape websites
library(purrr)  
library(lubridate) #to handle dates
library(kableExtra)
library(knitr)
library(dplyr)
library(gapminder)
library(ggplot2)
library(patchwork)
library(scales)
library(tidytext)
```

## Load Data

First, we load the data.

```{r}

#load the data set
hr_dataset <- read_csv(here::here("data", "datasets_1067_1925_WA_Fn-UseC_-HR-Employee-Attrition.csv"))

#glimpse to get a quick overview of the data
glimpse(hr_dataset)

```

>Data set reveals that there are 9 qualitative and 26 quantitative parameters about employees working at IBM. However, this is not a true picture as there are many level-based and rating-based variables such as Daily Rate, Environment Satisfaction etc. which have been coded as numerical values. Therefore, a data cleaning is necessary. 

>One important observation is that the data has no missing values.

## Clean Data

Next, we clean the data set, as variable names are in capital letters, some variables are not really necessary, and some variables, e.g., `education` are given as a number rather than a more useful description.


```{r}

#clean the data
hr_cleaned <- hr_dataset %>% 
  clean_names() %>% 
  mutate(
    education = case_when(
      education == 1 ~ "Below College",
      education == 2 ~ "College",
      education == 3 ~ "Bachelor",
      education == 4 ~ "Master",
      education == 5 ~ "Doctor"
    ),
    environment_satisfaction = case_when(
      environment_satisfaction == 1 ~ "Low",
      environment_satisfaction == 2 ~ "Medium",
      environment_satisfaction == 3 ~ "High",
      environment_satisfaction == 4 ~ "Very High"
    ),
    job_satisfaction = case_when(
      job_satisfaction == 1 ~ "Low",
      job_satisfaction == 2 ~ "Medium",
      job_satisfaction == 3 ~ "High",
      job_satisfaction == 4 ~ "Very High"
    ),
    performance_rating = case_when(
      performance_rating == 1 ~ "Low",
      performance_rating == 2 ~ "Good",
      performance_rating == 3 ~ "Excellent",
      performance_rating == 4 ~ "Outstanding"
    ),
    work_life_balance = case_when(
      work_life_balance == 1 ~ "Bad",
      work_life_balance == 2 ~ "Good",
      work_life_balance == 3 ~ "Better",
      work_life_balance == 4 ~ "Best"
    )
  ) %>% 
  select(age, attrition, daily_rate, department,
         distance_from_home, education,
         gender, job_role,environment_satisfaction,
         job_satisfaction, marital_status,
         monthly_income, num_companies_worked, percent_salary_hike,
         performance_rating, total_working_years,
         work_life_balance, years_at_company,
         years_since_last_promotion)

```

We decided to glimpse the data to gain an overview of the data set and use the skim output.

```{r}

#glimpse to get a quick overview of the data after cleaning
glimpse(hr_cleaned)

#make sure the data is free from missing values
skim(hr_cleaned)

```

> We can see that we are dealing with a nice data set with no missing values. After data cleaning, we are left with 19 parameters in total, of which - 10 are qualitative and 9 are quantitative.

## Exploratory Data Analysis

### Attrition rate across departments

```{r}

# creating variable
overalll_attrition <- hr_cleaned %>% 
  # grouping by attrition
  group_by(attrition) %>% 
  # counting variable
  summarise(count = n()) %>% 
  # adding a variable
  mutate(overall_attrition = paste(round(count*100/sum(count),0),"%",sep="")) %>%
  # filtering
  filter(attrition=="Yes") %>%  
  # selecting overall attrition
  select(overall_attrition) 

hr_cleaned %>% 
  # grouping by variables
  group_by(attrition, department) %>% 
  # counting by grouped variables
  summarise(count=n()) %>% 
  # grouping by department
  group_by(department) %>% 
  # adding a variable and rounding
  mutate(attrition_rate=paste(round(count*100/sum(count),0),"%",sep="")) %>% 
  # filtering
  filter(attrition=="Yes") %>% 
  # selecting certain variables
  select(department, attrition_rate) %>% 
  # ungrouping previously grouped variables
  ungroup() %>% 
  # adding an additional row with a variable
  add_row(department = 'Overall' , attrition_rate = overalll_attrition[[1]]) 

```

>From the chart and table, we can find that the overall attrition is around 16%. This is in line with our previous analysis which indicated that around 20% employees felt dissatisfied with the environmental conditions.

>Over 20% of Sales employees attrite, and Research & Development has minimum attrition. This data makes sense as Research & Development is a proprietary role and IBM must have put in place non-compete policies for such employees. On the other hand, sales is a highly competitive fast-paced role and thus high attrition is to be expected.

Besides, we think which kind of people leave also matters. If only low performers stay with IBM and all high achievers are leaving, this would be pretty bad. Thus, we have a look at attrition rate in combination with performance_rating.

```{r}

ggplot(hr_cleaned, 
aes(x = attrition, 
fill = performance_rating)) + 

  # show performance rating proportion by attrition (with bars both at 100%)
  geom_bar(position = "fill") + 
  labs(fill = "performance rating",
       y = "proportion",
       title = "Attrition and performance rating are independent",
       subtitle = "Performance rating proportion by attrition")

```

>From the chart, we can see that the performance of people has no influence on attrition.

### Distribution of `age`, `years_at_company`, `monthly_income` and `years_since_last_promotion`

Other important factors for a company are the distributions of `age`, `years_at_company`, `monthly_income` and `years_since_last_promotion` . Hence, we have a quick look at some summary statistics and the distribution.

```{r}

#specifying x axis
ggplot(hr_cleaned,aes(x=age))+  

  #generating density plot
  geom_density(fill="#8b9dc3")+ 
  
  #facet by department
  facet_wrap(~department) 

```

```{r}

hr_cleaned %>% 

  #grouped by department
  group_by(department) %>%  
  
  #calculating mean_age and rounded it to 0 decimal place
  summarize(mean_age=round(mean(age),0), 
            #calculating median age
            median_age = round(median(age),0)) 

```

>The age of employees working in different departments seems to be normally distributed with a mean age of around 37 Yrs and median age of around 36 Yrs. 

>The bump to the right extreme of these curves represent perhaps the senior management age group as they are expected to have more experience.

```{r}

#specifying x axes
ggplot(hr_cleaned,aes(x=years_at_company)) + 

  #generating density plot
  geom_density(fill="#8b9dc3") +   
  
  #adding x label
  labs (x="Years at Company") + 
  
  #facet by department
  facet_wrap(~department) 

```

```{r}

hr_cleaned %>% 

  #grouped by department
  group_by(department) %>%  
  
  #calculating mean_years_at_company and rounded it to 0 decimal place
  summarize(mean_years_at_company=round(mean(years_at_company),0), 
  
            #calculating median years_at_company
            median_years_at_company = round(median(years_at_company),0)) 

```

>As for years at company, across departments the distribution seems to be very right skewed, suggesting most employees leave in around 15 years.

```{r}

#specifying x  axes
ggplot(hr_cleaned,aes(x=monthly_income)) +  

  #generating density plot
  geom_density(fill="#8b9dc3") +  
  
  #adding x label
  labs(x="Monthly Income")+ 
  
  #facet by department
  facet_wrap(~department) 

```

```{r}

hr_cleaned %>% 

  #grouped by department
  group_by(department) %>%  
  
  #calculating mean_monthly_income and rounded it to 0 decimal place
  summarize(mean_monthly_income=round(mean(monthly_income),0), 
  
            #calculating median monthly_income
            median_monthly_income = round(median(monthly_income),0)) 

```

>For each department, monthly income is right skewed with average monthly income of Research & Development being on higher side - given the nature of their job role and the experience required. Most of the Sales people draw around USD 5,500 - 6,000 monthly. Human Resources seems to be earning the least on a relative scale.

```{r}

#specifying x axes
ggplot(hr_cleaned,aes(x=years_since_last_promotion)) + 

  #generating density plot
  geom_density(fill="#8b9dc3") + 
  
  #adding x label
  labs(x="Years Since Last Promotion") +  
  
  #facet by department
  facet_wrap(~department) 

```

```{r}

hr_cleaned %>% 
  #grouped by department
  group_by(department) %>%  
  
  #calculating mean_years_since_last_promotion and rounded it to 0 decimal place
  summarize(mean_years_since_last_promotion=round(mean(years_since_last_promotion),0), 
  
  #calculating median years_since_last_promotion
  median_years_since_last_promotion = round(median(years_since_last_promotion),0)) 

```

>Distribution of years since last promoted indicates that Human Resources and Sales employees spend more time between promotions than those in Research & Development.

### Distribution of `job_satisfaction` and `work_life_balance`

We now want to analyse job_satisfaction and work_life_balance. First, we have a look at the mere numbers.

```{r}

ggplot(hr_cleaned, 
aes(x = factor(job_satisfaction, 
levels = c("Low", "Medium", "High", "Very High")))) +
  geom_bar(aes(y = stat(prop), group = 1)) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "Job Satisfaction",
       title = "Proportion of People by Job Satisfaction")

```

```{r}

hr_cleaned %>%
  group_by(job_satisfaction) %>%
  summarise(n = n()) %>%
  
  # calculate proportion of people by job satisfaction level
  mutate(proportion = n / sum(n)) %>% 
  
  # reorder job satisfaction levels properly
  arrange(desc(factor(job_satisfaction, levels = c("Low", "Medium", "High", "Very High")))) 

```

```{r}

ggplot(hr_cleaned, aes(x = factor(work_life_balance, levels = c("Bad", "Good", "Better", "Best")))) +

  # do not show absolute counts but the proportion in percent (stat(prop))
  geom_bar(aes(y = stat(prop), group = 1)) + 
  
  # show y values as percentages
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "Work-life-balance",
       title = "Proportion of People by Work-life-balance")

```


```{r}

hr_cleaned %>%
  group_by(work_life_balance) %>%
  summarise(n = n()) %>%
  
  # calculate proportion of people by work life balance level
  mutate(proportion = n / sum(n)) %>% 
  
  # reorder work life balance levels properly
  arrange(desc(factor(work_life_balance, levels = c("Bad", "Good", "Better", "Best")))) 

```

>We can see that over 60% people are highly satisfied with their job. Furthermore, only around 5% of people have a bad work-life-balance. 

Then, we investigate the relationship between those two variables.

```{r}

# order job satisfactioon properly
ggplot(hr_cleaned, 
aes(x = factor(job_satisfaction, levels = c("Low", "Medium", "High", "Very High")), 

# order w-l-b properly
fill = factor(work_life_balance, 
levels = c("Best", "Better", "Good", "Bad")))) + 
  geom_bar(position = "fill") +
  labs(fill = "work life balance",
       y = "proportion",
       title = "Relationship between Job Satisfaction and Work Life Balance",
       subtitle = "Work life balance proportion by job satisfaction",
       x = "Job Satisfaction")

```

>Tt seems like there is no apparent relationship between work-life-balance and job satisfaction!

### Relationship between monthly income and education

```{r}

# show density, not counts (stat(density))
ggplot(hr_cleaned, 
mapping = aes(x = monthly_income, y = stat(density))) + 
  geom_freqpoly(mapping = aes(colour = education)) +
  labs(x = "monthly income",
       title = "Monthly Income Distribution by Education") +
       
  # order facets properly
  facet_wrap(~factor(education, levels = c("Below College", "College", "Bachelor", "Master", "Doctor"))) + 
  
  # use a nice theme
  theme_gdocs() 

```

```{r}

ggplot(hr_cleaned, 
aes(y = factor(education, 
levels = c("Below College", "College", "Bachelor", "Master", "Doctor")), 
                       x = monthly_income)) +
  geom_boxplot() +
  labs(x = "Monthly Income",
       y = "Education",
       title = "Monthly Income Boxplots by Education")

```

>In the boxplot above, we can see that the median monthly income is higher with higher education, with the exemption of a lower median monthly income for a Bachelor education as opposed to College education. 

Then we look at average monthly income by education level.

```{r}

avg_income_by_edu <- hr_cleaned %>% 
  group_by(education) %>% 
  
  # compute average monthly income by education level
  summarise(mean = mean(monthly_income)) 

# order education levels properly
ggplot(avg_income_by_edu, 
aes(x = factor(education, levels = c("Below College", "College", "Bachelor", "Master", "Doctor")), y = mean)) +
  geom_col() +
  labs(x = "Education Level",
       y = "Average Monthly Income",
       title = "Higher Education Means Higher Average Income!")
```

> It also shows a tendency for higher education yielding higher average monthly incomes.

### Relationship between monthly income and gender

```{r}

# show density, not counts (stat(density))
ggplot(hr_cleaned, mapping = aes(x = monthly_income, y = stat(density))) + 
  geom_freqpoly(mapping = aes(colour = gender)) +
  labs(x = "Monthly Income",
       title = "Monthly Income Distribution by Gender") +
  facet_wrap(~gender)

```

```{r}

ggplot(hr_cleaned, 
aes(y = gender, x = monthly_income)) +
  geom_boxplot() +
  labs(x = "Monthly Income",
       y = "Gender",
       title = "Monthly Income Boxplots by Gender")

```

>Although it is expected to have a gender pay gap, it seems like women are better off than men at IBM.

### Relationship between income and job role.

```{r}

ggplot(hr_cleaned, aes(y = reorder(job_role, monthly_income), x = monthly_income)) + # order by monthly income
  geom_boxplot() +
  labs(x = "Monthly Income",
       y = "Job Role",
       title = "Monthly Income Boxplots by Job Role")

```

>As it seems, managers and research directors are the highest paid at IBM. And Sales representatives are least well paid.

Then we brings the effect of age given the job role.

```{r}

ggplot(hr_cleaned, aes(x = age, y = monthly_income)) +
  # set opacity of points
  geom_point(alpha = 0.4) + 
  # one scatterplot for each job role
  facet_wrap(~job_role) + 
  labs(y = "Monthly Income",
       title = "Effect of Age on Monthly Income Given the Job Role")

```

>Even though the effect of age on monthly income is not too apparent when faceting on job role, we also have to consider that one usually takes on other roles throughout a career. 

### Relationship betwwen income and age

```{r}

ggplot(hr_cleaned, aes(x = age, y = monthly_income)) + 

  # set opacity of points
  geom_point(alpha = 0.4) + 
  geom_smooth() +
  labs(y = "Monthly Income",
       title = "Relationship between Income and Age")

```

>This is actually something that seems to be intuitive for most people: higher income with higher age, as staying with the same company for a long time means, you have gone through some tough times together, and therefore had some paycheck increases over the years.

