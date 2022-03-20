# nlss
Source data files and R programs that read in the data, manipulate it, then output visualisations and statistics.
The source files contain toy data.

### Languages and Tools
<div>
  <img src="https://github.com/devicons/devicon/blob/master/icons/r/r-original.svg" title="r" alt="r" width="70" height="70"/>&nbsp;
  <img src="https://github.com/devicons/devicon/blob/master/icons/rstudio/rstudio-original.svg" title="RStudio" alt="RStudio" width="70" height="70"/>&nbsp;
</div>

### Packages
<div>
  <img src="https://github.com/tidyverse/dplyr/raw/main/man/figures/logo.png" height="100" style="max-width: 100%;"/>&nbsp;
  <img src="https://github.com/tidyverse/ggplot2/raw/main/man/figures/logo.png" height="100" style="max-width: 100%;"/>&nbsp;
  <img src="https://github.com/tidyverse/tidyr/raw/main/man/figures/logo.png"  height="100" style="max-width: 100%;"/>&nbsp;
  <img src="https://github.com/sjmgarnier/viridis/raw/master/man/figures/logo.png" height="100" style="max-width: 100%;"/>&nbsp;
  <img src="https://github.com/tidyverse/readr/raw/main/man/figures/logo.png" height="100" style="max-width: 100%;"/>&nbsp;
</div>
<br>
<br>

# Outputs
## Example 1
The cumulative mean session length metric prominently displays AB test variant session length estimates trending over time to what appears to be their true central tendencies.

Prototype A (proto_A) consistently outperforms Prototype B (proto_B) since early in the test.

![white cumulative mean session lengths](https://user-images.githubusercontent.com/25012294/155900699-1236ce95-fb6f-41a4-8d31-76b7d738cf17.png)
<br>
<br>
## Example 2
The consistent outperformance of proto_B by proto_A has been statistically significant at the 95% confidence level for much of the test.
(Overlapping confidence intervals represent p values > 0.05 and non overlapping intervals represent p values <= 0.05.) 
Confidence intervals narrowing as the test matured and a persisting delta are signs that proto_A may be the true session length maximizing variant.

```r
data %>% 
  mutate(date = as.Date(SessionStart)) %>% 
  arrange(Prototype, date) %>% 
  group_by(Prototype) %>% 
  mutate(
          observations = 1,
          cumulative_avg_session_length = cummean(sessionLength),
          cumulative_obs = cumsum(observations)
         ) %>% 
  group_by(Prototype, date) %>% 
  mutate(
          cumulative_avg_session_length = last(cumulative_avg_session_length),
          cumulative_obs = max(cumulative_obs),
          cumulative_session_length_squared_errors = cumsum(as.numeric(sessionLength - cumulative_avg_session_length)^2)
         ) %>% 
  group_by(Prototype, date) %>%
  summarise(
             cumulative_session_length_ci_lower = max(cumulative_avg_session_length) - 1.39 * (sqrt(max(cumulative_session_length_squared_errors)/max(cumulative_obs)) / sqrt(max(cumulative_obs))),
             cumulative_avg_session_length = max(cumulative_avg_session_length),
             cumulative_session_length_ci_upper = max(cumulative_avg_session_length) + 1.39 * (sqrt(max(cumulative_session_length_squared_errors)/max(cumulative_obs)) / sqrt(max(cumulative_obs)))
         ) %>% 
  filter(date != '2018-07-21') %>% 
  ggplot(aes(x = date, y = cumulative_avg_session_length, group = Prototype, col = Prototype)) +
    geom_line(alpha = 0.5) +
    geom_errorbar(
                   aes(
                        ymin = cumulative_session_length_ci_lower,
                        ymax = cumulative_session_length_ci_upper
                      ),
                   width = 0.15
                  ) +
    ggthemes::theme_tufte() +
    xlab('Session Start Date') +
    ylab('Session Length (Min.)') +
    ggtitle('Cumulative Mean Session Length: 83.4% Confidence Intervals') +
    scale_color_viridis(
                         option = 'D',
                         discrete = TRUE,
                         begin = 0.2,
                         end = 0.8
                        )
```
Geometry printout

![cumulative session length confidence intervals white](https://user-images.githubusercontent.com/25012294/155900758-190d57e7-82a2-4fe8-816f-2ed9c86eb7c3.png)
<br>
<br>
## Example 3
This shows tranaction frequencies for different online marketing channels for each month of the first 12 months of users' lifetimes.

```r
graph_function <- function(channel_name) {
  data %>% 
    filter(channel == channel_name) %>% 
    mutate(months_since_ftd = floor(as.numeric(difftime(activity_date, ftd_date, units = "days")) / (365.25 / 12))) %>% 
    filter(
            ftd_date <= '2017-12-31',
            ftd_date != activity_date,
            months_since_ftd %in% seq(0, 11, by = 1) 
           ) %>% 
    group_by(channel, months_since_ftd) %>% 
    summarise(frequency = n()) %>% 
    ggplot(
           aes(
                y = as.factor(channel),
                x = months_since_ftd,
                fill = frequency
               )
            ) +
      geom_tile() +
      scale_x_continuous(breaks = seq(0, 11, by = 1)) +
      theme_minimal() +
      ggtitle(channel_name) +
      theme(
            panel.grid.minor.x = element_line(color = 'black'),
            panel.grid.major.y = element_line(color = 'black'),
            panel.grid.major.x = element_line(color = 'black'),
            plot.background = element_rect(fill = "black"),
            axis.text.y = element_blank(),
            plot.title = element_text(size = 9, color = 'grey'),
            legend.key.size = unit(.25, "cm"),
            legend.key.width = unit(0.5, "cm"),
            legend.title = element_text(size = 9, color = 'grey'),
            legend.text = element_text(size = 7, color = 'grey')
            ) +
      viridis::scale_fill_viridis(
                                   option = 'A',
                                   discrete = FALSE,
                                   name = 'Transaction Frequency',
                                   labels = scales::comma
                                  )
}


# Graph 4
gridExtra::grid.arrange(
                          graph_function('Affiliate'),
                          graph_function('Cross Sell'),
                          graph_function('Direct'),
                          graph_function('Facebook'),
                          graph_function('Incentive'),
                          graph_function('Other'),
                          graph_function('PPC') + 
                            theme(axis.title.x = element_text(color = 'gray40', size = 9)) +
                            xlab('Months Since 1st Transaction'),
                          ncol = 1
                        )
```
Geomtery printout

![retention black graph](https://user-images.githubusercontent.com/25012294/155893445-4f5a4ab2-db09-4272-80e8-fc68a1aaf1ec.png)

This shows the retention rate for each marketing channel.
```r
data %>% 
  group_by(channel) %>% 
  mutate(segment_size = n_distinct(player_id)) %>%
  mutate(months_since_ftd = floor(as.numeric(difftime(activity_date, ftd_date, units = "days")) / (365.25 / 12))) %>% 
  filter(
          ftd_date <= '2017-12-31',
          ftd_date != activity_date,
          months_since_ftd %in% seq(1, 12, by = 1)
         ) %>% 
  group_by(channel, months_since_ftd) %>% 
  summarise(rate = n_distinct(player_id) / max(segment_size)) %>% 
  ggplot(
          aes(
               y = as.factor(channel),
               x = as.factor(months_since_ftd),
               fill = rate
              )
          ) +
    geom_tile() +
    xlab('Months Since 1st Transaction') +
    ggtitle('Retention Rate') +
    viridis::scale_fill_viridis(
                                 option = 'inferno',
                                 discrete = FALSE,
                                 name = 'Rate',
                                 labels = scales::percent
                                ) +
    theme(
           plot.background = element_rect(colour = 'black', fill = 'black'),
           plot.title = element_text(colour = 'gray 40', size = 11),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.background = element_blank(),
           legend.background = element_blank(),
           legend.text = element_text(colour = 'gray40', size = 8),
           legend.title = element_text(colour = 'gray40', size = 9),
           legend.box.background = element_blank(),
           legend.key.width = unit(0.5, 'cm'),
           axis.title.x = element_text(colour = 'gray40', size = 9),
           axis.text.y = element_text(colour = 'gray40', size = 9),
           axis.ticks.y = element_blank()
          )
```
![retention rate](https://user-images.githubusercontent.com/25012294/159117554-0eb91e6d-c082-4313-bacb-6c47829fb3da.png)
<br>
<br>
## Example 4
The output data set represents cohorted marketing channel churn rates (from 0 to 1). Specifically, a rate of users who did not make a transaction a 2nd day within the first 30.4375 (365.25/12; 0.25 accounting for leap years) days following a 1st transaction. So, for instance, 74% (0.74) of users registered in M1 through the Affiliate channel did not make another transaction within the first 30.4375 days following their 1st transaction.
```r
churn <- data %>% 
  arrange(player_id, activity_date) %>% 
  group_by(player_id, channel) %>% 
  mutate(
          row_number = row_number(),
          lead_activity_date = lead(activity_date),
          lead_row_number = lead(row_number)
         ) %>% 
  filter(lead_row_number %in% c(2, NA), row_number == 1) %>% 
  mutate(
          months_delta = as.numeric(difftime(lead_activity_date, ftd_date, units = "days")) / (365.25 / 12),
          churn_class = if_else(months_delta > 1 | is.na(months_delta), 1, 0),
          cohort = if_else(as.yearmon(ftd_date) == 'Jan 2017', 'M1',
                           if_else(as.yearmon(ftd_date) == 'Feb 2017', 'M2',
                                   if_else(as.yearmon(ftd_date) == 'Mar 2017', 'M3',
                                           if_else(as.numeric(as.yearmon(ftd_date)) >= 2017.25 & as.numeric(as.yearmon(ftd_date)) <= 2017.917, 'M4-12', 'M13+')
                                           )
                                   )
                           ),
          cohort = factor(cohort, levels = c('M1', 'M2', 'M3', 'M4-12', 'M13+'))
          )
```

```r
churn %>% 
  group_by(cohort, channel) %>% 
  summarise(day_30_churn_rate = mean(churn_class)) %>% 
  spread(cohort, day_30_churn_rate)
#> # A tibble: 7 x 6
#>   channel       M1    M2    M3 `M4-12` `M13+`
#>   <chr>      <dbl> <dbl> <dbl>   <dbl>  <dbl>
#> 1 Affiliate  0.740 0.699 0.759   0.720  0.714
#> 2 Cross Sell 0.737 0.731 0.7     0.703  0.735
#> 3 Direct     0.731 0.743 0.751   0.675  0.659
#> 4 Facebook   0.695 0.701 0.701   0.658  0.723
#> 5 Incentive  0.877 0.796 0.827   0.854  0.853
#> 6 Other      0.611 0.929 0.778   0.759  0.729
#> 7 PPC        0.705 0.701 0.703   0.688  0.677
```
<img src="https://user-images.githubusercontent.com/25012294/157022311-6f1e15cf-4fe7-4276-86cd-f34de528d0bf.png" height = "200" width = "max-width: 100%;"/>&nbsp;


The table above represents the percentage of users who did not deposit a 2nd day within the first 30.4375 days following their 1st transaction. So, for instance, 74% of users who registered in January 2017 (M1) through the Affiliate channel did not make another transaction within the first 30.4375 days following their first transaction.

It is wise in this scenario to classify users as churned only if they have not deposited a 2nd day within their first 30.4375 days because it allows for fair cohort comparison. It lends to fairness because users’ probabilities of churning persists throughout the entirety of their lifetimes. For instance, users acquired in January 2017 can churn in any month from January 2017 to December 2018. Whereas users acquired in November 2018 only have 1 month to exhibit churning behavior.

Not assessing churn based on whether or not a user transacted again within the first 30.4375 days would probably bias cohort churn rates. Causing difficulty in comparing cohorts.  As M1 cohorts would probably always have the highest churn rates. Based on the assumption that as users’ lifetimes increase, probability of churning increases.
<br>
<br>
## Example 5
Online marketing channel 12 month user lifetime values.

First the data manipulation.
```r
month_map <- data.frame(
                         month = c(seq(1, 12, by = 1)),
                         key = ''
                        )

player_map <- data.frame(
                          player_id = data$player_id,
                          channel = data$channel,
                          key = ''
                         ) %>% 
  distinct()

player_month_map <- inner_join(
                                player_map,
                                month_map,
                                by = 'key'
                               )

data2 <- data %>% 
  mutate(
          months_since_ftd = floor(as.numeric(difftime(activity_date, ftd_date, units = "days")) / (365.25 / 12)) + 1,
          key = ''
         ) %>% 
  filter(ftd_date < '2018-01-01') %>% 
  group_by(player_id, months_since_ftd, channel) %>% 
  summarise(total_deposits = sum(deposits)) %>% 
  arrange(player_id)

cumulative_12_month_channel_ltvs <- left_join(
                                               player_month_map,
                                               data2,
                                               by = c('month' = 'months_since_ftd', 'player_id', 'channel')
                                              ) %>% 
  group_by(player_id) %>% 
  mutate(cumulative_deposits = cumsum(replace_na(total_deposits, 0))) %>% 
  group_by(month, channel) %>% 
  summarise(avgLTV = sum(cumulative_deposits) / n_distinct(player_id))
```

Then a visual that is optimized for readability, precision and comparison.
```r
cumulative_12_month_channel_ltvs %>% 
  ggplot(
          aes(
               x = as.factor(month),
               y = avgLTV,
               col = channel,
               group = channel
              )
          ) +
    geom_line(size = 0.8) +
    xlab('Month') +
    ylab('Avg. LTV') +
    labs(color = 'Channel') +
    ggtitle('Cumulative Average LTV') +
    theme_minimal() +
    scale_y_continuous(limits = c(0, 125)) +
    theme(panel.grid.major.y = element_line(color = "grey"))
```
Printout

![white ltv graph](https://user-images.githubusercontent.com/25012294/155892628-45b6560d-3df8-463d-803b-e0a1b0c1fc88.png)

The graph above displays the cumulative LTV for marketing channels, for each month of users’ tenures, up to the 12th month of users’ tenures, for users with tenures of at least 12 months. So when the x axis equals 12, the y axis represents the sum of transactions by all users during the first 12 months of their tenures, by users who have had a tenure of at least 12 months, divided by the number of distinct users who have had a tenure of at least 12 months.


In a way that leads your attention to something the most valuable.
```r
cumulative_12_month_channel_ltvs %>%
  mutate(label = if_else(month == 12, channel, NA)) %>% 
  ggplot(
         aes(
              x = as.factor(month),
              y = avgLTV,
              col = avgLTV,
              group = channel
             )
          ) +
    viridis::scale_color_viridis(
                                  discrete = FALSE,
                                  direction = 1,
                                  option = 'magma'
                                 ) +
    geom_line(size = 0.1) +
    geom_text(
               aes(label = label),
               nudge_x = 2.5,
               hjust = 1,
               size = 3.5
              ) +
    xlab('Month') +
    labs(color = 'LTV') +
    ggtitle('Cumulative Average LTV') +
    theme_minimal() +
    scale_y_continuous(limits = c(0, 125)) +
    theme(
           panel.grid.major.y = element_line(color = "black"),
           panel.grid.minor.y = element_line(color = 'black'),
           panel.grid.minor.x = element_line(color = 'black'),
           panel.grid.major.x = element_line(color = 'black'),
           plot.background = element_rect(fill = "black"),
           plot.title = element_text(size = 10, color = 'grey'),
           axis.text.y = element_blank(),
           axis.title.x = element_text(color = 'gray40', size = 9),      
           legend.key.size = unit(.25, "cm"),
           legend.key.width = unit(0.5, "cm"),
           legend.title = element_text(size = 9, color = 'gray40'),
           legend.text = element_text(size = 7, color = 'gray40'),
           legend.position = 'left'
          )
```
Printout

![Rplot](https://user-images.githubusercontent.com/25012294/158074372-6d44dd81-442c-4583-8148-0c8f30c3d95a.png)

Lifetime revenue per monthly cohort.
```r
data %>%
  filter(activity_date >= ftd_date) %>% 
  arrange(ftd_date, activity_date) %>%
  mutate(yearmon_fd = as.yearmon(ftd_date)) %>% 
  group_by(yearmon_fd, activity_date) %>%
  summarise(total_daily_deposits = sum(deposits)) %>%
  mutate(
          life_time_revenue = cumsum(total_daily_deposits),
          activity_date = if_else(
                                  yearmon_fd == as.yearmon(activity_date),
                                                                          as.Date(yearmon_fd + 0.1),
                                                                                                    as.Date(activity_date)
                                  )
          ) %>% 
  ggplot(
          aes(
               x = activity_date,
               y = life_time_revenue,
               col = as.factor(yearmon_fd)
             )
        ) +
    geom_line() +
      ggtitle('Cumulative Lifetime Revenue') +
      scale_y_continuous(labels = scales::dollar_format(prefix = '£')) +
      viridis::scale_color_viridis(option = 'viridis', discrete = TRUE) +
      theme(
             plot.background = element_rect(colour = 'black', fill = 'black'),
             plot.title = element_text(
                                        colour = 'gray20',
                                        size = 14,
                                        vjust = -5
                                       ),            
             panel.grid = element_blank(),
             panel.background = element_blank(),
             axis.ticks.y = element_blank(),
             axis.ticks.x = element_blank(),
             axis.text.x = element_text(angle = 45),
             axis.title.x = element_blank(),
             legend.position = 'none'
            )

```
![cumulative lifetime revenue](https://user-images.githubusercontent.com/25012294/159141810-92a91123-a0b7-478f-aba4-42f500c20bbb.png)



