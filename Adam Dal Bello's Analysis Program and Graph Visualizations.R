library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)



################################## Data set ##################################
data <- read_excel("Snr Business Analyst_data.xlsx")

# Removing activity dates earlier than ftd dates, as well as removing negative deposit values, \n
# wasn't necessary since these errors only effected .008% of observations (rows) \n
# These errors will not have had much of an impact on results. \n 
# Leaving in negative deposit values may even represent refunds and therefore practically effect LTV estimates.



################################## Analysis ##################################

### A. Direct attribution model
mr_prepped_data <- data %>%
  group_by(ftd_date, channel) %>%
  summarise(registrations = n_distinct(player_id)) %>%
  spread(channel, -ftd_date)

# Graph 1
mr_prepped_data %>%
  gather(Channel, Registrations, -ftd_date) %>%
  ggplot(aes(x = ftd_date, y = Registrations, col = Channel, group = Channel)) +
  geom_line() +
  xlab("First Time Deposit Date") +
  ylab("Registrations") +
  ggtitle("Daily Registrations per Channel") +
  ggthemes::theme_tufte()

## Primary regression model
fit <- lm(
  Direct ~ 0 + Affiliate + `Cross Sell` + Direct + Facebook + Incentive + Other + PPC,
  data = mr_prepped_data,
  na.action = na.omit
)

# Coefficients
fit

# Table 2
summary(fit)

# Graph 2
mr_prepped_data %>%
  gather(Channel, Registrations, -ftd_date) %>%
  ggplot(aes(x = ftd_date, y = Registrations, col = Channel, group = Channel)) +
  geom_line() +
  xlab("First Time Deposit Date") +
  ylab("Registrations") +
  ggtitle("Daily Registrations per Channel") +
  theme_light() +
  facet_grid(Channel ~ .)

## Alternative regression model
mr_prepped_data_alt <- data %>%
  group_by(ftd_date, channel) %>%
  summarise(registrations = n_distinct(player_id)) %>%
  filter(ftd_date >= "2018-07-01") %>%
  spread(channel, -ftd_date)

fit_alt <- lm(
  Direct ~ 0 + Affiliate + `Cross Sell` + Direct + Facebook + PPC,
  data = mr_prepped_data_alt,
  na.action = na.omit
)

# Alternative coefficients
fit_alt

summary(fit_alt)


### B. Lifetime value analysis
month_map <- data.frame(
  month = c(seq(1, 12, by = 1)),
  key = ""
)

player_map <- data.frame(
  player_id = data$player_id,
  channel = data$channel,
  key = ""
) %>%
  distinct()

player_month_map <- inner_join(
  player_map,
  month_map,
  by = "key"
)

data2 <- data %>%
  mutate(
    months_since_ftd = floor(as.numeric(difftime(activity_date, ftd_date, units = "days")) / (365.25 / 12)) + 1,
    key = ""
  ) %>%
  filter(ftd_date < "2018-01-01") %>%
  group_by(player_id, months_since_ftd, channel) %>%
  summarise(total_deposits = sum(deposits)) %>%
  arrange(player_id)

cumulative_12_month_channel_ltvs <- left_join(
  player_month_map,
  data2,
  by = c("month" = "months_since_ftd", "player_id", "channel")
) %>%
  group_by(player_id) %>%
  mutate(cumulative_deposits = cumsum(replace_na(total_deposits, 0))) %>%
  group_by(month, channel) %>%
  summarise(avgLTV = sum(cumulative_deposits) / n_distinct(player_id))

# Graph 3
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
  xlab("Month") +
  ylab("Avg. LTV") +
  labs(color = "Channel") +
  ggtitle("Cumulative Average LTV") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 125)) +
  theme(panel.grid.major.y = element_line(color = "grey"))

# Graph 3 Alternate
cumulative_12_month_channel_ltvs %>%
  mutate(label = ifelse(month == 12, channel, NA)) %>%
  ggplot(aes(x = as.factor(month), y = avgLTV, col = avgLTV, group = channel)) +
  viridis::scale_color_viridis(
    discrete = FALSE,
    direction = 1,
    option = "magma"
  ) +
  geom_line(size = 0.01) +
  geom_text(
    aes(label = label),
    nudge_x = 2.5,
    hjust = 1,
    size = 3.5
  ) +
  xlab("Month") +
  ylab("Avg. LTV") +
  labs(color = "LTV") +
  ggtitle("Cumulative Lifetime Value (LTV)") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 125)) +
  theme(
    panel.grid.major.y = element_line(color = "black"),
    panel.grid.minor.y = element_line(color = "black"),
    panel.grid.minor.x = element_line(color = "black"),
    panel.grid.major.x = element_line(color = "black"),
    plot.background = element_rect(fill = "black"),
    plot.title = element_text(size = 10, color = "grey"),
    axis.text.y = element_blank(),
    axis.title.x = element_text(color = "gray40", size = 9),
    legend.key.size = unit(.25, "cm"),
    legend.key.width = unit(0.5, "cm"),
    legend.title = element_text(size = 9, color = "gray40"),
    legend.text = element_text(size = 7, color = "gray40"),
    legend.position = "left"
  )

# Table 4
cumulative_12_month_channel_ltvs %>% 
  filter(month == 12) %>% 
  arrange(-avgLTV)


### C. Churn analysis
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

# Table 6
churn %>% 
  group_by(cohort, channel) %>% 
  summarise(day_30_churn_rate = mean(churn_class)) %>% 
  spread(cohort, day_30_churn_rate)


graph_function <- function(channel_name) {
  data %>%
    filter(channel == channel_name) %>%
    mutate(months_since_ftd = floor(as.numeric(difftime(activity_date, ftd_date, units = "days")) / (365.25 / 12))) %>%
    filter(
      ftd_date <= "2017-12-31",
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
      panel.grid.minor.x = element_line(color = "black"),
      panel.grid.major.y = element_line(color = "black"),
      panel.grid.major.x = element_line(color = "black"),
      plot.background = element_rect(fill = "black"),
      axis.text.y = element_blank(),
      plot.title = element_text(size = 9, color = "grey"),
      legend.key.size = unit(.25, "cm"),
      legend.key.width = unit(0.5, "cm"),
      legend.title = element_text(size = 9, color = "grey"),
      legend.text = element_text(size = 7, color = "grey")
    ) +
    viridis::scale_fill_viridis(
      option = "A",
      discrete = FALSE,
      name = "Transaction Frequency",
      labels = scales::comma
    )
}


# Graph 4
gridExtra::grid.arrange(
  graph_function("Affiliate"),
  graph_function("Cross Sell"),
  graph_function("Direct"),
  graph_function("Facebook"),
  graph_function("Incentive"),
  graph_function("Other"),
  graph_function("PPC") +
    theme(axis.title.x = element_text(color = "gray40", size = 9)) +
    xlab("Months Since 1st Transaction"),
  ncol = 1
)


# Graph 4 Alternate
data %>%
  mutate(months_since_ftd = floor(as.numeric(difftime(activity_date, ftd_date, units = "days")) / (365.25 / 12))) %>%
  filter(
    ftd_date <= "2017-12-31",
    ftd_date != activity_date,
    months_since_ftd %in% seq(1, 12, by = 1)
  ) %>%
  group_by(channel, months_since_ftd) %>%
  summarise(frequency = n_distinct(player_id)) %>%
  ggplot(
    aes(
      y = as.factor(channel),
      x = as.factor(months_since_ftd),
      fill = frequency
    )
  ) +
  geom_tile() +
  xlab("Months Since 1st Transaction") +
  ylab("Channel") +
  ggtitle("Retention") +
  viridis::scale_fill_viridis(
    option = "inferno",
    discrete = FALSE,
    name = "Returns",
    labels = scales::comma
  ) +
  theme(
    plot.background = element_rect(colour = "black", fill = "black"),
    plot.title = element_text(colour = "gray 40", size = 11),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "black"),
    legend.background = element_blank(),
    legend.text = element_text(colour = "gray40", size = 8),
    legend.title = element_text(colour = "gray40", size = 9),
    legend.box.background = element_blank(),
    legend.key.width = unit(0.5, "cm"),
    axis.title.x = element_text(colour = "gray40", size = 9),
    axis.text.y = element_text(colour = "gray40", size = 9),
    axis.ticks.y = element_blank()
  )


# Graph 4 Alternate 2
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
      y = as.factor(channel),
      x = as.factor(months_since_ftd),
      fill = rate
    )
  ) +
  geom_tile() +
  geom_text(aes(label = round(rate, 2)), size = 3) +
  xlab("Months Since 1st Transaction") +
  ggtitle("Retention Rate") +
  viridis::scale_fill_viridis(
    option = "inferno",
    discrete = FALSE,
    name = "Rate",
    labels = scales::percent
  ) +
  theme(
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
    


################################## Appendices ##################################

### Appendix A
data %>%
  mutate(months_since_ftd = floor(as.numeric(difftime(activity_date, ftd_date, units = "days")) / (365.25 / 12))) %>%
  filter(
    ftd_date <= "2017-12-31",
    months_since_ftd %in% seq(0, 11, by = 1)
  ) %>%
  group_by(player_id, channel) %>%
  summarise(revenue = sum(deposits)) %>%
  ggplot(aes(x = log(revenue), y = channel, col = channel)) +
  geom_boxplot(width = .4) +
  stat_summary(fun = mean, colour = "darkred", geom = "point", shape = 18, size = 2) +
  theme_minimal() +
  xlab("Log of LTV") +
  ylab("Channel") +
  labs(color = "Channel") +
  ggtitle("Natural Log Distributions of 12 Month Channel LTVs: 2017 Cohort")

### Appendix B
data %>%
  mutate(months_since_ftd = floor(as.numeric(difftime(activity_date, ftd_date, units = "days")) / (365.25 / 12))) %>%
  filter(
    ftd_date <= "2017-12-31",
    months_since_ftd %in% seq(0, 11, by = 1)
  ) %>%
  group_by(player_id, channel) %>%
  summarise(revenue = sum(deposits)) %>%
  group_by(channel) %>%
  summarise(
    ci_lower = mean(revenue) - (1.96 * (sd(revenue) / n())),
    mean = mean(revenue),
    ci_upper = mean(revenue) + (1.96 * (sd(revenue) / n()))
  ) %>%
  ggplot(aes(x = channel, col = channel)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper)) +
  ggthemes::theme_tufte() +
  theme(legend.position = "none") +
  ylab("LTV") +
  xlab("Channel") +
  ggtitle("95% Confidence Intervals of 12 Month LTVs: 2017 Cohort")



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
  ggtitle("Cumulative Lifetime Revenue") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "£")) +
  viridis::scale_color_viridis(option = "viridis", discrete = TRUE) +
  theme_minimal() +
  theme(
    plot.background = element_rect(colour = "black", fill = "black"),
    plot.title = element_text(
      colour = "gray20",
      size = 14,
      vjust = -5
    ),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(angle = 45),
    axis.title.x = element_blank(),
    legend.position = "none"
  )
