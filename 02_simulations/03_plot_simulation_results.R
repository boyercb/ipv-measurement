plot_df <- 
  get_simulations(results) %>% 
  as_tibble() %>%
  mutate(
    treat_effects = rep(names(tau_models), each = 2000), 
    estimator = str_replace(estimator, " ", "\n")
  )


plot_df %>%
  ggplot(., aes(x = estimate, y = estimator)) +
  geom_jitter(alpha = 0.10, height = 0.3, shape = 16) +
  facet_wrap(
    ~ factor(treat_effects),
    nrow = 2,
    ncol = 3,
    labeller = label_parsed
  ) +
  geom_segment(aes(
    x = estimate,
    y = y + 0.35,
    yend = y - 0.35,
    xend = estimate,
    group = estimator
  ),
  data = plot_df %>%
    group_by(treat_effects, estimator) %>%
    summarise(estimate = mean(estimate)) %>%
    mutate(y = ifelse(estimator == "continuous\noutcome", 2, 1)),
  color = "red") +
  labs(x = expression("Estimate"~(widehat(tau))), y = "") +
  theme_bw()

power_df <- 
  get_diagnosands(results) %>% 
  as_tibble() %>%
  filter(inquiry != "ATE_frequency") %>%
  mutate(
    treat_effects = rep(names(tau_models), each = 2),
    estimator_label = str_replace(estimator, " ", "\n")
  )

power_df %>%
  ggplot(aes(y = power, x = factor(estimator))) +
  facet_wrap(~factor(treat_effects), labeller = label_parsed, nrow = 2, ncol = 3, scales = "free") +
  geom_point() +
  geom_segment(aes(y = power - 1.96 * `se(power)`, yend = power + 1.96 * `se(power)`, xend = estimator)) +
  geom_text(aes(
    label = round(power, 2),
    x = as.numeric(factor(estimator)) - 0.1
  ),
  size = 3
  ) +
  labs(x = expression("Power"~(beta)), y = "") +
  theme_minimal(base_size = 14, base_family = "Palatino") +
  theme(panel.grid.major.x = element_blank()) 

power_df %>%
  ggplot(aes(y = rmse, x = factor(estimator))) +
  facet_wrap(~factor(treat_effects), labeller = label_parsed, nrow = 2, ncol = 3, scales = "free") +
  geom_point() +
  geom_segment(aes(y = rmse - 1.96 * `se(rmse)`, yend = rmse + 1.96 * `se(rmse)`, xend = estimator)) +
  geom_text(aes(
    label = round(rmse, 2),
    x = as.numeric(factor(estimator)) - 0.1
  ),
  size = 3
  ) +
  labs(x = "RMSE", y = "") +
  theme_minimal(base_size = 14, base_family = "Palatino") +
  theme(panel.grid.major.x = element_blank()) 

power_df %>%
  ggplot(aes(y = coverage, x = factor(estimator))) +
  facet_wrap(~factor(treat_effects), labeller = label_parsed, nrow = 2, ncol = 3, scales = "free") +
  geom_point() +
  geom_segment(aes(y = coverage - 1.96 * `se(coverage)`, yend = coverage + 1.96 * `se(coverage)`, xend = estimator)) +
  geom_text(aes(
    label = round(coverage, 2),
    x = as.numeric(factor(estimator)) - 0.1
  ),
  size = 3
  ) +
  labs(x = "Coverage", y = "") +
  theme_minimal(base_size = 14, base_family = "Palatino") +
  theme(panel.grid.major.x = element_blank()) 

