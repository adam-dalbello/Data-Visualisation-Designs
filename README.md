# nlss
Source data files and R programs that read in the data, manipulate it, then output visualisations and statistics.
The source files contain toy data.
<br>
<br>

### Languages and Tools
<div>
  <img src="https://github.com/devicons/devicon/blob/master/icons/r/r-original.svg" title = "r" alt = "r" width = "60" height = "60"/>&nbsp;
  <img src="https://github.com/devicons/devicon/blob/master/icons/rstudio/rstudio-original.svg" title = "RStudio" alt = "RStudio" width = "60" height = "60"/>&nbsp;
</div>

### Major Packages
<div>
  <img src="https://github.com/tidyverse/dplyr/raw/main/man/figures/logo.png" height = "100" style = "max-width: 100%;"/>&nbsp;
  <img src="https://github.com/tidyverse/ggplot2/raw/main/man/figures/logo.png" height = "100" style = "max-width: 100%;"/>&nbsp;
  <img src="https://github.com/tidyverse/tidyr/raw/main/man/figures/logo.png"  height = "100" style = "max-width: 100%;"/>&nbsp;
  <img src="https://github.com/rstudio/gt/raw/master/man/figures/logo.svg" height = "100" style = "max-width: 100%;"/>&nbsp;
  <img src="https://raw.githubusercontent.com/tidyverse/tibble/main/man/figures/logo.png" height = "100" style = "max-width: 100%;"/>&nbsp; 
</div>
<br>
<br>

# Outputs
## Example 1
The cumulative mean session length metric prominently displays AB test variant session length estimates trending over time to what appears to be their true central tendencies.

```r
data %>%
  ggplot(
    aes(
      SessionStart,
      as.numeric(sessionLength),
      group = Prototype,
      col = Prototype
    )
  ) +
  geom_point(size = .5) +
  xlab("Session Start Time") +
  ylab("Session Length (Min.)") +
  ggtitle("Session Length") +
  viridis::scale_color_viridis(
    option = "D",
    discrete = TRUE,
    begin = 0,
    end = 0.6
  ) +
  theme(
    panel.background = element_rect(fill = 'grey94'),
    text = element_text(family = 'Segoe UI')
  ) +
  scale_y_continuous(labels = scales::comma)
```

Prototype A (proto_A) consistently outperforms Prototype B (proto_B) since early in the test.

![dotted cumulative session length](https://user-images.githubusercontent.com/25012294/161422204-5823b6b3-7081-40d2-accf-e67432cc4681.png)
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
    cumulative_session_length_ci_lower = max(cumulative_avg_session_length) - 1.39 * (sqrt(max(cumulative_session_length_squared_errors) / max(cumulative_obs)) / sqrt(max(cumulative_obs))),
    cumulative_avg_session_length = max(cumulative_avg_session_length),
    cumulative_session_length_ci_upper = max(cumulative_avg_session_length) + 1.39 * (sqrt(max(cumulative_session_length_squared_errors) / max(cumulative_obs)) / sqrt(max(cumulative_obs)))
  ) %>%
  filter(date != "2018-07-21") %>%
  ggplot(
    aes(
      date,
      cumulative_avg_session_length,
      group = Prototype,
      col = Prototype
    )
  ) +
  geom_line(alpha = 0.2) +
  geom_point(alpha = 0.2) +
  geom_linerange(
    aes(
      ymin = cumulative_session_length_ci_lower,
      ymax = cumulative_session_length_ci_upper
    ),
    size = 5,
    alpha = 0.5
  ) +
  xlab("Session Start Date") +
  ylab("Session Length (Min.)") +
  ggtitle("Cumulative Mean Session Length: 83.4% Confidence Intervals") +
  viridis::scale_color_viridis(
    option = "D",
    discrete = TRUE,
    begin = 0,
    end = 0.6
  ) +
  theme(
    panel.background = element_rect(fill = 'grey94'),
    text = element_text(family = 'Segoe UI')
  )
```
Geometry printout

![cumulative session length white](https://user-images.githubusercontent.com/25012294/161718959-621c411b-0a3b-4960-b882-581473324e70.png)
<br>
<br>
## Example 3
This shows the retention rate for each marketing channel.

```r
data %>%
  group_by(channel) %>%
  mutate(segment_size = n_distinct(player_id)) %>%
  mutate(months_since_ftd = floor(as.numeric(difftime(activity_date, ftd_date, units = "days")) / (365.25 / 12))) %>%
  filter(
    ftd_date <= "2017-12-31",
    ftd_date != activity_date,
    months_since_ftd %in% seq(1, 12, by = 1)
  ) %>%
  group_by(channel, months_since_ftd) %>%
  summarise(rate = n_distinct(player_id) / max(segment_size)) %>%
  ggplot(
    aes(
      as.factor(months_since_ftd),
      as.factor(channel),      
      fill = rate
    )
  ) +
  geom_tile() +
  xlab("Months Since 1st Transaction") +
  ggtitle("Retention Rate") +
  viridis::scale_fill_viridis(
    option = "inferno",
    discrete = FALSE,
    name = "Rate",
    labels = scales::percent
  ) +
  theme(
    text = element_text(family = 'Segoe UI'),
    plot.background = element_rect(colour = "black", fill = "black"),
    plot.title = element_text(colour = "gray 40", size = 11),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.background = element_blank(),
    legend.text = element_text(colour = "gray40", size = 8),
    legend.title = element_text(colour = "gray40", size = 9),
    legend.box.background = element_blank(),
    legend.key.width = unit(0.5, "cm"),
    axis.title.x = element_text(colour = "gray40", size = 9),
    axis.text.y = element_text(colour = "gray40", size = 9),
    axis.ticks.y = element_blank()
  )
```
![retention rate](https://user-images.githubusercontent.com/25012294/161424377-33442aae-0615-4699-bc87-984047773819.png)
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
                                     if_else(as.numeric(as.yearmon(ftd_date)) >= 2017.25 & as.numeric(as.yearmon(ftd_date)) <= 2017.917, 'M4-12', 'M13+'
                                     )
                             )
                     )
             ),
    cohort = factor(cohort, levels = c('M1', 'M2', 'M3', 'M4-12', 'M13+'))
  )

churn_table <- churn %>% 
  group_by(cohort, channel) %>% 
  summarise(day_30_churn_rate = mean(churn_class)) %>% 
  spread(cohort, day_30_churn_rate)
  
churn_table %>%   
  gt(rowname_col = 'channel') %>% 
  tab_header(title = '30 Day Churn Rates') %>% 
  fmt_percent(columns = contains('M')) %>% 
  data_color(
    columns = contains('M'),
    colors = scales::col_numeric(
      palette = "Reds",
      domain = c(
        min(as.matrix(churn_table[ , -1])),
        max(as.matrix(churn_table[ , -1]))
        )
      )
  ) %>% 
  tab_spanner(
    label = "Cohorts (2017 - 2018)",
    columns = contains('M')
  ) %>% 
  cols_width(everything() ~ px(120)) %>% 
  cols_align(
    align = "center",
    columns = contains('M')
  ) %>% 
  tab_options(data_row.padding = px(1))

print(churn_table)
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
![churn rate table](https://user-images.githubusercontent.com/25012294/161070552-182e86cc-a81f-4870-85ce-eb837df5d38d.png)

The table above represents the percentage of users who did not deposit a 2nd day within the first 30.4375 days following their 1st transaction. So, for instance, 74% of users who registered in January 2017 (M1) through the Affiliate channel did not make another transaction within the first 30.4375 days following their first transaction.

It is wise in this scenario to classify users as churned only if they have not deposited a 2nd day within their first 30.4375 days because it allows for fair cohort comparison. It lends to fairness because users’ probabilities of churning persists throughout the entirety of their lifetimes. For instance, users acquired in January 2017 can churn in any month from January 2017 to December 2018. Whereas users acquired in November 2018 only have 1 month to exhibit churning behavior.

Not assessing churn based on whether or not a user transacted again within the first 30.4375 days would probably bias cohort churn rates. Causing difficulty in comparing cohorts.  As M1 cohorts would probably always have the highest churn rates. Based on the assumption that as users’ lifetimes increase, probability of churning increases.
<br>
<br>
## Example 5
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
    activity_date,
    life_time_revenue,
    col = as.factor(yearmon_fd)
  )
) +
  geom_line() +
  ggtitle("Cumulative Lifetime Revenue") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "£")) +
  viridis::scale_color_viridis(option = "viridis", discrete = TRUE) +
  theme_minimal() +
  theme(
    text = element_text(family = 'Segoe UI'),
    plot.background = element_rect(colour = "black", fill = "black"),
    plot.title = element_text(
      colour = "gray20",
      size = 14,
      vjust = -2
    ),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(angle = 45),
    axis.title.x = element_blank(),
    legend.position = "none"
  )
```
![cumulative lifetime revenue](https://user-images.githubusercontent.com/25012294/161422741-91ffbc4e-0ee9-48db-bbbe-319c7e32514e.png)
