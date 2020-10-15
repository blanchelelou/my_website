---
categories:
- ""
- ""
date: "2017-10-31T22:26:13-05:00"
description: Who uses social media?
draft: false
image: media.jpg
keywords: ""
slug: socialmedia
title: The social media trap
---

# General Social Survey (GSS)

The [General Social Survey (GSS)](http://www.gss.norc.org/) gathers data on American society in order to monitor and explain trends in attitudes, behaviours, and attributes. Many trends have been tracked for decades, so one can see the evolution of attitudes, etc in American Society.


In this section, we analyze data from the **2016 GSS sample data**, using it to estimate values of *population parameters* of interest about US adults. The GSS sample data file has 2867 observations of 935 variables, but we are only interested in very few of these variables and you are using a smaller file.


```{r, read_gss_data, cache=TRUE}
gss <- read_csv(here::here("data", "smallgss2016.csv"), 
                na = c("", "Don't know",
                       "No answer", "Not applicable"))
```

We will be creating 95% confidence intervals for population parameters. The variables we have are the following:

- hours and minutes spent on email weekly. The responses to these questions are recorded in the `emailhr` and `emailmin` variables. For example, if the response is 2.50 hours, this would be recorded as emailhr = 2 and emailmin = 30.
- `snapchat`, `instagrm`, `twitter`: whether respondents used these social media in 2016
- `sex`: Female - Male
- `degree`: highest education level attained

## Instagram and Snapchat, by sex

Can we estimate the *population* proportion of Snapchat or Instagram users in 2016?

### New variable`snap_insta', measuring the union of snapchat and instagram users

```{r, snap_insta}
gss <- gss %>%
    mutate(snap_insta = case_when(snapchat == "Yes"                 ~ "Yes",
                                instagrm == "Yes"                 ~ "Yes",    
                                snapchat == "No" & instagrm == "No"~ "No",
                                snapchat == "NA" & instagrm == "NA" ~ "NA")
    )
```

### Determining the proportion of Snapchat or Insta users in the sample, of those who gave definitive answers

```{r, proportion_snap_insta}
proportion_snap_insta<- gss %>% 
  count(snap_insta) %>% 
  pivot_wider(names_from=snap_insta , values_from=n) %>% 
  mutate(prop_yes=Yes/(No+Yes))
  
proportion_snap_insta %>% 

  # Playing around with KBL package to produce a more aesthetically-pleasant table
  kbl(col.names = c("Indefinite Responses", 
  "Neither Snapchat nor Insta User", 
  "Snapchat or Insta User", 
  "Proportion of Snapchat or Insta users in sample"),
      caption = '37.5% of the sample population use at least either Snapchat or Instagram') %>%
  kable_styling(fixed_thead = T, 
  full_width = F, 
  font_size = 12)

```

### Constructing 95% confidence intervals for men and women who used either Snapchat or Insta

```{r, snap_insta_by_sex}

#Calculating the proportion of snapchat and insta users, grouped by sex
gender_split <- gss %>%
  filter(snap_insta!="NA") %>%
  group_by(sex) %>%
  summarize(Total = count(sex), 
  users = count(snap_insta == "Yes"), 
  Proportion = round(users/Total,3))

#Determining the standard error and subsequently the confidence intervals for both sexes
library(kableExtra)
gender_split <- gender_split %>%
  mutate(std_error = round(sqrt(Proportion*(1-Proportion)/Total),3)) %>%
  # 95% confidence interval calculation (z score of 2)
  mutate(lower_level = round(Proportion - 1.96*std_error,3), 
         upper_level = round(Proportion + 1.96*std_error,3))

#Cleaning up the table
gender_split%>%
  rename('Sex' = 'sex',
         'Lower CI'  = 'lower_level',
         'Upper CI' = 'upper_level' ,
         'Standard Error' = 'std_error',  
         'Number of Users' = 'users', 
         'Proportion of users' = 'Proportion' )%>%
  kbl(caption = '95% confidence intervals on the proportion of Snapchat or Insta users, grouped by sex')%>%
  kable_styling(fixed_thead = T, full_width = F, font_size = 12 )
```

## Twitter, by education level

Can we estimate the *population* proportion of Twitter users by education level in 2016?. 

There are 5 education levels in variable `degree` which, in ascending order of years of education, are Lt high school, High School, Junior college, Bachelor, Graduate. 

### Adjusting the variable type of `degree'
```{r, degree_factor}
twitter_gss<- gss %>% 
  na.omit(degree) %>% 
  mutate(degree = factor(degree, 
                         levels = c("Lt high school", 
                                    "High School", 
                                    "Junior college", 
                                    "Bachelor", 
                                    "Graduate"))) %>% 
  arrange((degree))
```

### Creating a new variable `bachelor_graduate' that distinguishes between pre and post bachelor degrees
```{r, bachelor_graduate}
twitter_gss <- twitter_gss %>% 
  mutate(bachelor_graduate = case_when(
    degree == "Bachelor" |  degree == "Graduate" ~ "Yes", 
    degree == "Lt high school" | degree == "High School" | degree == "Junior college" ~ "No"
    ))
```

### Calculating the proportion of `bachelor_graduate`s that use twitter
```{r, twitter_users}
proportion_twitter<- twitter_gss %>% 
  filter(bachelor_graduate == "Yes") %>%
  count(twitter) %>% 
  pivot_wider(names_from=twitter , values_from=n) %>% 
  mutate(prop_yes=Yes/(No+Yes))

proportion_twitter %>% 

  # Playing around with KBL package to produce a more aesthetically-pleasant table
  kbl(col.names = c("Indefinite Responses", 
  "Do not use Twitter", 
  "Twitter User", 
  "Proportion of Twitter users in sample of Bachelor Grads"),
      caption = '23.3% of bachelor grads from the sample population use twitter') %>%
  kable_styling(fixed_thead = T, full_width = F, font_size = 12, )
```

### Constructing 95% confidence intervals for bachelor grads who use and don't use twitter (excluding NAs)

```{r, twitter_prop}

#Calculating the proportion of snapchat and insta users, grouped by sex
bachelor_prop <- twitter_gss %>%
  filter(bachelor_graduate =="Yes") %>%
  filter(twitter!="NA") %>%
  group_by(twitter) %>%
  summarise(count = n()) %>% 
  mutate(Proportion = count/sum(count))

#Determining the standard error and subsequently the confidence intervals for both sexes

library(kableExtra)
bachelor_prop <- bachelor_prop %>%
  mutate(std_error = round(sqrt(Proportion*(1-Proportion)/count),3)) %>%
  
  # 95% confidence interval calculation (z score of 2)
  mutate(lower_level = round(Proportion - 1.96*std_error,3), 
         upper_level = round(Proportion + 1.96*std_error,3))

#Cleaning up the table
bachelor_prop %>%
    # Playing around with KBL package to produce a more aesthetically-pleasant table
  kbl(col.names = c("Twitter User?", 
  "Total in Sample",
  "Proportion of Bachelor Grads",
  "Standard Error", 
  "Lower CI",
  "Upper CI"),
      caption = '95% confidence intervals on the proportion of bachelor grads, grouped by their twitter use')%>%
  kable_styling(fixed_thead = T, full_width = F, font_size = 12 )
```

These confidence intervals do not overlap.

## Email usage

Can we estimate the *population* parameter on time spent on email weekly?

### Combining emailhr and emailmin into a new variable `email'

```{r, email_combo, cache=TRUE}

email <- gss %>%
  mutate_at(vars(emailhr,emailmin), 
  funs(as.numeric)) %>% 
  mutate(email=emailmin*(emailhr*60)) %>% 
  arrange(desc(email))

email

```

### Finding the mean and median number of minutes respondents spend on email weekly and visualising the distribution of `email'

```{r, email_dist, cache=TRUE}

email_summary <- email %>%
   na.omit(email) %>%
  summarize(mean = mean(email), 
  median = median(email)) 

email_summary

ggplot(email, aes(y=email)) +
  geom_boxplot()+
  scale_y_log10() +
  theme_clean() +
  labs(title="Boxplot showing how long US Adults spend emailing per week!",
       y="Email Minutes")

```


Typically, the mean is a better measure when the distribution is roughly normal. However, if the curve is more flat and/or skewed, as is the case here,  the median is the best gauge of the typical amounnt of time Americans spend on email weekly

### Calculating a 95% bootstrap confidence interval for the mean amount of time Americans spend on email weekly

```{r, email_bootstrap, cache=TRUE}

ci_bootstrap_email <- email %>% 
  specify(response = email) %>% 
  generate(reps = 100, type="bootstrap") %>% 
  calculate(stat = "mean") %>% 
  get_confidence_interval(level = 0.95, type = "percentile") %>% 
  mutate(lower_ci = lower_ci/60, upper_ci = upper_ci/60)

ci_bootstrap_email %>%

    # Playing around with KBL package to produce a more aesthetically-pleasant table
  kbl(col.names = c("Lower CI","Upper CI"),
      caption = '95% bootstap confidence intervals for the mean amount of time Americans spend emailing weekly')%>%
  kable_styling(fixed_thead = T, full_width = F, font_size = 12 )

```

### Interpreting this interval in context of the data, reporting its endpoints in “humanized” units

We found the confidence interval for the mean email usage in the sample to be from 6h24m to 7h28m. Given that the sample mean is 6h57m, we this result does not seem odd and fits the narrative of a confidence interval.

### Would you expect a 99% confidence interval to be wider or narrower than the interval you calculated above? Explain your reasoning.

As we know with confidence intervals, if we increasing our confidence, we also increase the corresponding margin of error, which results in a wider interval. Therefore, if we increase the confidence level to 99%, then we expect the range to compensate by becoming wider. 

