# nlss
Source data and R programs. The programs read in the data, manipulate it, then output visualisations and statistics. 
Source files containing toy data. The goal advancing visualisations. So some could be percceived as progressive or impractical. Some report ready.
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
  <img src="https://raw.githubusercontent.com/tidyverse/tibble/main/man/figures/logo.png" height = "100" style = "max-width: 100%;"/>&nbsp; 
</div>
<br>
<br>
<br>

# Programs and Visualisations
## Instance 1: AB Test Impact Estimate
The cumulative mean session length metric prominently displays AB test variant session length estimates trending over time to what appear to be true central tendencies.

```r
data %>%
  arrange(SessionStart) %>%
  group_by(Prototype) %>%
  mutate(cumulative_avg_session_length_mins = cummean(sessionLength)) %>%
  ggplot(
    aes(
      SessionStart,
      cumulative_avg_session_length_mins,
      group = Prototype,
      col = Prototype
    )
  ) +
  geom_point(size = .5) +
  xlab("Session Start Time") +
  ylab("Session Length (Min.)") +
  ggtitle("Cumulative Mean Session Length") +
  viridis::scale_color_viridis(
    option = "D",
    discrete = TRUE,
    begin = 0,
    end = 0.6
  ) +
  theme(
    panel.background = element_rect(fill = 'grey97'),
    text = element_text(family = 'Segoe UI'),
    plot.title = element_text(size = 9),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9)
  )
```
<br>
Prototype A (proto_A) consistently outperforms Prototype B (proto_B).
<br>
<br>

![dotted cumulative session length](https://user-images.githubusercontent.com/25012294/176927346-7ff7b3c8-7a3e-47d3-b3e5-1f954f00149c.png)
<br>
<br>

## Instance 2: Estimating the Presence of Randomness in AB Tests
Proto_A outperformance of proto_B has been statistically significant at 95% confidence for the majority of the test. (Overlapping confidence intervals represent statistical significance. Non overlapping intervals represent non statistically significant difference.) Narrowing confidence intervals and a persistent difference represent a high probability that proto_A maximizes session length.

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
  geom_errorbar(
    aes(
      ymin = cumulative_session_length_ci_lower,
      ymax = cumulative_session_length_ci_upper
    ),
    col = 'black',
    alpha = 0.5,
    width = 0.35
  ) +
  geom_line(alpha = 0.2) +
  geom_linerange(
    aes(
      ymin = cumulative_session_length_ci_lower,
      ymax = cumulative_session_length_ci_upper
    ),
    size = 5
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
    panel.background = element_rect(fill = 'grey97'),
    text = element_text(family = 'Segoe UI'),
    plot.title = element_text(size = 9),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9)
  )
```
<br>
![cumulative confidence intervals white](https://user-images.githubusercontent.com/25012294/176927847-4b8966bb-fe8b-4f7b-b679-2326ac685790.png)
<br>
<br>

## Instance 3: Estimating Marketing Channel Retention
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
    plot.title = element_text(colour = "gray 40", size = 9),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.background = element_blank(),
    legend.text = element_text(colour = "gray40", size = 8),
    legend.title = element_text(colour = "gray40", size = 9),
    legend.box.background = element_blank(),
    legend.key.width = unit(2, "mm"),
    axis.title.x = element_text(colour = "gray40", size = 9),
    axis.text.y = element_text(colour = "gray40", size = 8),
    axis.text.x = element_text(colour = 'gray40', size = 8),
    axis.ticks.y = element_blank()
  )
```
<br>
![retention rate](https://user-images.githubusercontent.com/25012294/176928347-94822bbb-ae70-4c4f-958a-4193982445b1.png)
<br>
<br>

## Instance 4: Abstracting Cohort Lifetime Revenue
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
  xlab('Date') +
  ylab('Revenue') +
  scale_y_continuous(labels = scales::dollar_format(prefix = "£")) +
  viridis::scale_color_viridis(option = "viridis", discrete = TRUE) +
  theme(
    text = element_text(family = 'Segoe UI'),
    plot.background = element_rect(colour = "black", fill = "black"),
    plot.title = element_text(color = "gray40", size = 9),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(angle = 40, size = 8),
    axis.text.y = element_text(size = 8),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  )
  )
```
<br>
![cumulative lifetime revenue](https://user-images.githubusercontent.com/25012294/176928486-28daa8e0-124d-4f1f-a8cd-8c6559007c1c.png)

