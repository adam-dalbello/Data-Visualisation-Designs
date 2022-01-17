library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)

data <- read_csv(
                  'Data Analyst Home Assignment - Question 4 Data Set.csv', 
                  col_types = cols(
                                    SessionStart = col_datetime(format = '%d/%m/%Y %H:%M:%S'),
                                    SessionEnd = col_datetime(format = '%d/%m/%Y %H:%M:%S')
                                  )
                ) %>% 
    mutate(sessionLength = difftime(SessionEnd, SessionStart, units = 'mins'))

# Table 1
data %>% 
  group_by(Prototype) %>% 
  summarise(
              users = n_distinct(UserID),
              sessions = n_distinct(SessionID),
              sessions_per_user = n_distinct(SessionID)/n_distinct(UserID),
              
              p25_session_length = round(quantile(sessionLength, prob = 0.25), 2),
              p50_session_length = round(quantile(sessionLength, prob = 0.50), 2),
              avg_session_length = round(mean(sessionLength), 2),
              p75_session_length = round(quantile(sessionLength, prob = 0.75), 2),
              
              p25_levels_completed = quantile(LevelsCompleted, prob = 0.25),
              p50_levels_completed = quantile(LevelsCompleted, prob = 0.50),
              avg_levels_completed = round(mean(LevelsCompleted), 2),
              p75_levels_completed = quantile(LevelsCompleted, prob = 0.75)
  ) %>% 
  left_join(
    data %>% 
      group_by(Prototype, UserID, as.Date(SessionStart)) %>% 
      summarise(sessions = n_distinct(SessionID)) %>%
      group_by(Prototype) %>% 
      summarise(avg_daily_sessions_per_user = mean(sessions))
  ) %>% 
  gather(Measure, Value, -Prototype, factor_key = TRUE) %>% 
  spread(Prototype, Value) %>% 
  write.table('clipboard', sep = '\t', row.names = FALSE)

# Session length t-test
proto_A_session_lengths <- data %>% filter(Prototype == 'proto_A') %>% select(sessionLength) %>% mutate(sessionLength = as.numeric(sessionLength)) %>% as.matrix() %>% as.vector()
proto_B_session_lengths <- data %>% filter(Prototype == 'proto_B') %>% select(sessionLength) %>% mutate(sessionLength = as.numeric(sessionLength)) %>% as.matrix() %>% as.vector()

t.test(proto_A_session_lengths, proto_B_session_lengths)

# Levels completed t-test
proto_A_levels_completed <- data %>% filter(Prototype == 'proto_A') %>% select(LevelsCompleted) %>% as.matrix() %>% as.vector()
proto_B_levels_completed <- data %>% filter(Prototype == 'proto_B') %>% select(LevelsCompleted) %>% as.matrix() %>% as.vector()

t.test(proto_A_levels_completed, proto_B_levels_completed)

# Sessions per user t-test
proto_A_daily_sessions_per_user <- data %>% filter(Prototype == 'proto_A') %>% group_by(UserID, as.Date(SessionStart)) %>% summarise(sessions = n_distinct(SessionID)) %>% as.data.frame() %>% select(sessions) %>% as.matrix() %>% as.vector()
proto_B_daily_sessions_per_user <- data %>% filter(Prototype == 'proto_B') %>% group_by(UserID, as.Date(SessionStart)) %>% summarise(sessions = n_distinct(SessionID)) %>% as.data.frame() %>% select(sessions) %>% as.matrix() %>% as.vector()

t.test(proto_A_daily_sessions_per_user, proto_B_daily_sessions_per_user)

# Graph 1
data %>% 
  ggplot(aes(x = SessionStart, y = as.numeric(sessionLength), group = Prototype, col = Prototype)) +
    geom_point(size = .5) +
    ggthemes::theme_tufte() +
    xlab('Session Start Time') +
    ylab('Session Length (Min.)') +
    ggtitle('Session Length') +
    scale_color_viridis(option = 'D', discrete = TRUE, begin = 0.2, end = 0.8) +
    scale_y_continuous(labels = scales::comma)

# Graph 2
data %>% 
  ggplot(aes(x = Prototype, y = as.numeric(sessionLength), group = Prototype, col = Prototype)) +
    geom_boxplot() +
    ggthemes::theme_tufte() +
    xlab('Session Start Time') +
    ylab('Session Length (Min.)') +
    ggtitle('Session Length Distribution') +
    scale_color_viridis(option = 'D', discrete = TRUE, begin = 0.2, end = 0.8) +
    scale_y_continuous(labels = scales::comma)

# Graph 3
data %>%
  arrange(SessionStart) %>% 
  group_by(Prototype) %>% 
  mutate(cumulative_avg_session_length_mins = cummean(sessionLength)) %>% 
  ggplot(aes(x = SessionStart, y = cumulative_avg_session_length_mins, group = Prototype, col = Prototype)) +
    geom_point(size = .5) +
    ggthemes::theme_tufte() +
    xlab('Session Start Time') +
    ylab('Session Length (Min.)') +
    ggtitle('Cumulative Mean Session Length') +
    scale_color_viridis(option = 'D', discrete = TRUE, begin = 0.2, end = 0.8)

# Graph 4
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

# Graph 5
data %>% 
  ggplot(aes(x = SessionStart, y = LevelsCompleted, group = Prototype, col = Prototype)) +
    geom_point() +
    ggthemes::theme_tufte() +
    xlab('Session Start Time') +
    ylab('Levels Completed') +
    ggtitle('Levels Completed Distribution') +
    scale_color_viridis(option = 'D', discrete = TRUE, begin = 0.2, end = 0.8)

# Graph 6
data %>% 
  mutate(date = as.Date(SessionStart)) %>% 
  arrange(Prototype, date) %>% 
  group_by(Prototype) %>% 
  mutate(
          observations = 1,
          cumulative_avg_levels_completed = cummean(LevelsCompleted),
          cumulative_obs = cumsum(observations)
          ) %>% 
  group_by(Prototype, date) %>% 
  mutate(
          cumulative_avg_levels_completed = last(cumulative_avg_levels_completed),
          cumulative_obs = max(cumulative_obs),
          cumulative_levels_completed_squared_errors = cumsum(as.numeric(LevelsCompleted - cumulative_avg_levels_completed)^2)
         ) %>% 
  group_by(Prototype, date) %>%
  summarise(
             cumulative_levels_completed_ci_lower = max(cumulative_avg_levels_completed) - 1.39 * (sqrt(max(cumulative_levels_completed_squared_errors)/max(cumulative_obs)) / sqrt(max(cumulative_obs))),
             cumulative_avg_levels_completed = max(cumulative_avg_levels_completed),
             cumulative_levels_completed_ci_upper = max(cumulative_avg_levels_completed) + 1.39 * (sqrt(max(cumulative_levels_completed_squared_errors)/max(cumulative_obs)) / sqrt(max(cumulative_obs)))
  ) %>% 
  filter(date != '2018-07-21') %>% 
  ggplot(aes(x = date, y = cumulative_avg_levels_completed, group = Prototype, col = Prototype)) +
    geom_line(alpha = 0.5) +
    geom_errorbar(aes(ymin = cumulative_levels_completed_ci_lower, ymax = cumulative_levels_completed_ci_upper), width = 0.15) +
    ggthemes::theme_tufte() +
    xlab('Session Start Date') +
    ylab('Levels Completed') +
    ggtitle('Cumulative Mean Levels Completed:\n83.4% Confidence Intervals') +
    scale_color_viridis(option = 'D', discrete = TRUE, begin = 0.2, end = 0.8)

# Graph 7
data %>% 
  mutate(date = as.Date(SessionStart)) %>% 
  group_by(Prototype, UserID, date) %>%
  summarise(sessions = n_distinct(SessionID)) %>% 
  arrange(Prototype, date) %>% 
  group_by(Prototype) %>% 
  mutate(
          observations = 1,
          cumulative_obs = cumsum(observations),
          cumulative_daily_avg_sessions = cummean(sessions)
         ) %>% 
  group_by(Prototype, date) %>% 
  mutate(
          cumulative_daily_avg_sessions = last(cumulative_daily_avg_sessions),
          cumulative_obs = max(cumulative_obs),
          cumulative_daily_sessions_squared_errors = cumsum(as.numeric(sessions - cumulative_daily_avg_sessions)^2)
         ) %>% 
  group_by(Prototype, date) %>%
  summarise(
             cumulative_sessions_ci_lower = max(cumulative_daily_avg_sessions) - 1.39 * (sqrt(max(cumulative_daily_sessions_squared_errors)/max(cumulative_obs)) / sqrt(max(cumulative_obs))),
             cumulative_daily_avg_sessions = max(cumulative_daily_avg_sessions),
             cumulative_sessions_ci_upper = max(cumulative_daily_avg_sessions) + 1.39 * (sqrt(max(cumulative_daily_sessions_squared_errors)/max(cumulative_obs)) / sqrt(max(cumulative_obs)))
  ) %>% 
  filter(date != '2018-07-21') %>% 
  ggplot(aes(x = date, y = cumulative_daily_avg_sessions, group = Prototype, col = Prototype)) +
    geom_line(alpha = 0.5) +
    geom_errorbar(aes(ymin = cumulative_sessions_ci_lower, ymax = cumulative_sessions_ci_upper), width = 0.15) +
    ggthemes::theme_tufte() +
    xlab('Session Start Date') +
    ylab('Sessions') +
    ggtitle('Cumulative Mean Daily Sessions per User:\n83.4% Confidence Intervals') +
    scale_color_viridis(option = 'D', discrete = TRUE, begin = 0.2, end = 0.8)

# Graph 8
data %>% 
  mutate(date = as.factor(as.Date(SessionStart))) %>% 
  group_by(Prototype, UserID, date) %>%
  summarise(sessions = n_distinct(SessionID)) %>% 
  ggplot(aes(x = date, y = log(sessions), col = Prototype)) + 
    geom_boxplot() +
    ggthemes::theme_tufte() +
    xlab('Session Start Date') +
    ylab('Log of Sessions') +
    ggtitle('Daily Sessions per User Distribution') +
    scale_color_viridis(option = 'D', discrete = TRUE, begin = 0.2, end = 0.8) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

