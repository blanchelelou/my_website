---
categories:
- ""
- ""
date: "2017-10-31T21:28:43-05:00"
description: "Which stocks will make you rich"
draft: false
image: finance.jpg
keywords: ""
slug: ipsum
title: Financial Stocks
---

# Returns of financial stocks

I will use the `tidyquant` package to download historical data of stock prices, calculate returns, and examine the distribution of returns. 

I will first identify which stocks we want to download data for, and for this we must know their ticker symbol; Apple is known as AAPL, Microsoft as MSFT, McDonald's as MCD, etc. The file `nyse.csv` contains 508 stocks listed on the NYSE, their ticker `symbol`, `name`, the IPO  (Initial Public Offering) year, and the sector and industry the company is in.


```{r load_nyse_data, message=FALSE, warning=FALSE}
nyse <- read.csv(here::here("data","nyse.csv"))
```

Based on this dataset, I will create a table and a bar plot that shows the number of companies per sector, in descending order

```{r companies_per_sector}
# count the number of companies by sector
companies_by_sector <- nyse %>%
  group_by(sector) %>%
  count() %>%
  arrange(desc(n))

companies_by_sector %>% 
  # rename for better reading
  rename(`number of companies` = n) %>% 
  # create nice html table
  kbl() %>%
  kable_material_dark(bootstrap_options = c("striped", "hover", "condensed", "responsive")) # have a nice HTML table
```

```{r companies_per_sector} 
companies_by_sector <- nyse %>%
  group_by(sector) %>%
  summarise(count = n()) 

#create a bar plot
ggplot(companies_by_sector, aes(x = reorder(sector, count), y = count)) + 
  
  geom_col(fill = "#1380A1") + 
  
  theme_bw()+
  theme(plot.title = element_text(face = 'bold', size = 15), 
        axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size= 8)) +

# add label to each bar
  geom_label(aes(label = count), 
             hjust = 1,
             vjust = 0.5,
             fill = NA,
             label.size = NA,
             color = 'White',
             size = 4) +
  
  labs(x = "", 
       y = "Number of Companies", 
       title = 'Finance & Consumer Services companies reign NYSE',
       subtitle = 'NYSE number of companies by sector') +
  
  coord_flip()

```

Next, I have chosen a few stocks to analyse.

```{r get_price_data, message=FALSE, warning=FALSE, cache=TRUE}
# Notice the cache=TRUE argument in the chunk options. Because getting data is time consuming, 
# cache=TRUE means that once it downloads data, the chunk will not run again next time you knit your Rmd

myStocks <- c("AXP","BLK","KO","AMZN","MCD","BP","SPY" ) %>%
  tq_get(get  = "stock.prices",
         from = "2011-01-01",
         to   = "2020-08-31") %>%
  group_by(symbol) 
  
# examine the structure of the resulting data frame
glimpse(myStocks) 

```

Financial performance analysis depend on returns; If I buy a stock today for 100 and I sell it tomorrow for 101.75, my one-day return, assuming no transaction costs, is 1.75%. So given the adjusted closing prices, our first step is to calculate daily and monthly returns.


```{r calculate_returns, message=FALSE, warning=FALSE, cache=TRUE}

#calculate daily returns
myStocks_returns_daily <- myStocks %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "daily", 
               type       = "log",
               col_rename = "daily_returns",
               cols = c(nested.col))  

#calculate monthly  returns
myStocks_returns_monthly <- myStocks %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               type       = "arithmetic",
               col_rename = "monthly_returns",
               cols = c(nested.col)) 
```

Below is q table with the monthly returns, min, max, median, and SD for each of the stocks.

```{r summarise_monthly_returns}

myStocks_returns_monthly_summary <- myStocks_returns_monthly %>% 
  group_by(symbol) %>% 
  summarise(min = min(monthly_returns), 
            median = median(monthly_returns), 
            max = max(monthly_returns), 
            mean = mean(monthly_returns), 
            sd = sd(monthly_returns)) # calculate summary statistics for monthly returns by stock

myStocks_returns_monthly_summary %>% 
  kbl() %>%
  kable_material_dark(bootstrap_options = c("striped", "hover", "condensed", "responsive")) # have a nice HTML table

```

Below is a density plot, using `geom_density()`, for each of the stocks.

```{r density_monthly_returns}

myStocks_returns_monthly %>% 
  filter(symbol != "SPY") %>% # only display density of the stocks (not the ETF)
  ggplot(aes(x = monthly_returns, fill = symbol)) + # add some color (fill) just for fun
  geom_density() + 
  facet_wrap(~symbol) + # create a density plot for each stock
  labs(title = "Distribution of monthly returns per stock", 
       x = "Monthly returns", y = "Density") +
  theme(legend.position = "none") # show no legend

```

From this plot, we can infer that Blackrock (BLK) is the riskiest plot and is highly volatile because its deviation from 0.0 is the strongest. In contrast, Coca Cola (KO) is the least risky and is risk averse with small changes in monthly return.

Finally, make a plot that shows the expected monthly return (mean) of a stock on the Y axis and the risk (standard deviation) in the X-axis. Please use `ggrepel::geom_text_repel()` to label each stock

``` {r risk_return_plot}

myStocks_returns_monthly_summary %>% 
  
  # only display the stocks (not the ETF)
  filter(symbol != "SPY") %>% 
  
  # plot scatterplot and increase size of points
  ggplot(aes(x=sd, y = mean, label = symbol)) +
  geom_point(aes(colour = symbol), size = 4) +
  
  # add the stock names as labels to the points
  ggrepel::geom_text_repel(aes(colour = symbol)) + 
  
  # add title and axis labels to graph 
  labs(title = 'Risk/Return profile of stocks', subtitle = 'Higher the risk, higher the gain ?',
       x = 'Risk (standard deviation of monthly returns)', 
       y ="Average monthly return") +
  
  # use a nice theme
  theme_bw() + 
  
  # display x values as percentages
  scale_x_continuous(labels = scales::percent) + 
  
  # display y values as percentages
  scale_y_continuous(labels = scales::percent) + 
  
  # hide legend
  theme(legend.position = "none") 

```

The plot shows Amazon (AMZN) is the stock with the strongest risk-reward profile : it has the highest average monthly return and has the highest risk as indicated by its position on the top right corner of the graph. Indeed, in general, higher risks lead to a higher mean monthly return. However, what is interesting to note is BP's position that proves a high risk does not necessarily imply a high return - BP has the lowest monthly returns.
