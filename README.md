# nlss
Here are some data source files and R programs that read in the data, manipulate the data, then output visualisations and statistics.
The source files were given to me by companies I was interviewing for. The software used to respond to the take home assignment sections of the interview processes.


## Example Visualisations
### Example 1
The cumulative mean session length metric prominently displays AB test variant session length estimates trending over time to what appears to be their true central tendencies.

Prototype A (proto_A) consistently outperforms Prototype B (proto_B) since early in the test.

![Screenshot 2022-02-25 235228](https://user-images.githubusercontent.com/25012294/155818266-467aeecf-c9ac-4627-a13f-cc809eb24d24.png)



### Example 2
The consistent outperformance of proto_B by proto_A has been statistically significant at the 95% confidence level for much of the test.
(Overlapping confidence intervals represent p values > 0.05 and non overlapping intervals represent p values <= 0.05.) 
Confidence intervals narrowing as the test matured and a persisting delta are signs that proto_A may be the true session length maximizing variant.

```
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
    geom_errorbar(aes(ymin = cumulative_session_length_ci_lower, ymax = cumulative_session_length_ci_upper), width = 0.15) +
    ggthemes::theme_tufte() +
    xlab('Session Start Date') +
    ylab('Session Length (Min.)') +
    ggtitle('Cumulative Mean Session Length:\n83.4% Confidence Intervals') +
    scale_color_viridis(option = 'D', discrete = TRUE, begin = 0.2, end = 0.8)
```
![Screenshot 2022-02-25 235535](https://user-images.githubusercontent.com/25012294/155818740-df1b6321-74a6-48b3-a3d7-e7a02cece2ce.png)



### Example 3
This shows retention and engagement estimates for different online marketing channels.

```
graph_function <- function(channel_name) {
  data %>% 
    filter(channel == channel_name) %>% 
    mutate(months_since_ftd = floor(as.numeric(difftime(activity_date, ftd_date, units = "days"))/(365.25/12))) %>% 
    filter(
            ftd_date <= '2017-12-31',
            ftd_date != activity_date,
            months_since_ftd %in% seq(0, 11, by = 1) 
            ) %>% 
    group_by(channel, months_since_ftd) %>% 
    summarise(frequency = n()) %>% 
    ggplot(aes(y = as.factor(channel), x = months_since_ftd, fill = frequency)) +
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
      viridis::scale_fill_viridis(option = 'A', discrete = FALSE, name = 'Deposit Frequency', labels = scales::comma)
}

gridExtra::grid.arrange(
                          graph_function('Affiliate'),
                          graph_function('Cross Sell'),
                          graph_function('Direct'),
                          graph_function('Facebook'),
                          graph_function('Incentive'),
                          graph_function('Other'),
                          graph_function('PPC'),
                          ncol = 1
)
```
![Screenshot 2022-02-26 000824](https://user-images.githubusercontent.com/25012294/155819516-f0309b8f-06e3-4b93-aa2d-0148acf76708.png)



### Example 4
The output data set represents cohorted marketing channel churn rates (from 0 to 1). Specifically, a rate of users who did not make a transaction a 2nd day within the first 30.4375 (365.25/12) days following a 1st transaction. So, for instance, 74% (0.74) of users registered in M1 through the Affiliate channel did not make another transaction within the first 30.4375 days following their 1st transaction.

```
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
          months_delta = as.numeric(difftime(lead_activity_date, ftd_date, units = "days"))/(365.25/12),
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

churn %>% 
  group_by(cohort, channel) %>% 
  summarise(day_30_churn_rate = mean(churn_class)) %>% 
  spread(cohort, day_30_churn_rate)
```

Output dataset
```
# A tibble: 7 x 6
  channel       M1    M2    M3 `M4-12` `M13+`
  <chr>      <dbl> <dbl> <dbl>   <dbl>  <dbl>
1 Affiliate  0.740 0.699 0.759   0.720  0.714
2 Cross Sell 0.737 0.731 0.7     0.703  0.735
3 Direct     0.731 0.743 0.751   0.675  0.659
4 Facebook   0.695 0.701 0.701   0.658  0.723
5 Incentive  0.877 0.796 0.827   0.854  0.853
6 Other      0.611 0.929 0.778   0.759  0.729
7 PPC        0.705 0.701 0.703   0.688  0.677
```
