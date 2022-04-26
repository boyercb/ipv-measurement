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
    estimator = case_when(
      estimator == "binary outcome" ~ "Y[binary]",
      estimator == "continuous outcome" ~ "Y[sum]",
    ), 
    empirical = rep(rep(countries, each = 2), 16),
    probs = rep(rep(
      c(
        "cessation only",
        "cessation + reduction",
        "reduction only",
        "cessation + reduction + increase"
      ),
      each = 18
    ), 4),
    treat_effects = rep(names(tau_models), each = 72),
    estimator_label = str_replace(estimator, " ", "\n")
  )


b1_power_df <- 
  get_diagnosands(b1_results) %>% 
  as_tibble() %>%
  filter(inquiry != "ATE_frequency") %>%
  mutate(
    estimator = case_when(
      estimator == "binary outcome" ~ "$Y_{binary}$",
      estimator == "continuous outcome" ~ "$Y_{sum}$"
    ), 
    rmse = if_else(estimator == "$Y_{sum}$", rmse / 27, rmse),
    bias = if_else(estimator == "$Y_{sum}$", bias / 27, bias),
    probs = case_when(
      probs == "c(0.7, 0.3, 0, 0)" ~ "c",
      probs == "c(0.7, 0.1, 0.2, 0)" ~ "cr",
      probs == "c(0.7, 0, 0.3, 0)" ~ "r",
      probs == "c(0.7, 0.1, 0.15, 0.05)" ~ "cri"
    ),
    treat_effects = rep(names(tau_models), each = 8),
    estimator_label = str_replace(estimator, " ", "\n")
  ) %>%
  select(
    probs,
    treat_effects,
    estimator,
    bias,
    # `se(bias)`,
    rmse,
    # `se(rmse)`,
    power,
    # `se(power)`,
    coverage,
    # `se(coverage)`
  ) %>%
  pivot_wider(
    names_from = treat_effects,
    names_glue = "{treat_effects}_{.value}",
    names_sort = TRUE,
    values_from = c(bias, rmse, power, coverage),
  ) %>%
  select(
    estimator,
    starts_with("constant_"),
    starts_with("physical_"),
    starts_with("sexual_")
  )


tab1 <- kable(
  b1_power_df,
  digits = 3,
  col.names = c("", rep(c("Bias", "RMSE", "Power", "Coverage"), 3)),
  caption = "Simulation results based on Becoming One study in Uganda.\\label{tab:b1_sims}",
  booktabs = TRUE,
  format = "latex",
  align = "lcccccccccccc",
  escape = FALSE
) %>% 
  kable_styling(position = "center", font_size = 7) %>%
  add_header_above(
    c(" " = 1,
      "All acts" = 4,
      "Physical only" = 4,
      "Sexual only" = 4)
  ) %>%
  group_rows("Cessation only", 1, 2) %>%
  group_rows("Cessation + reduction", 3, 4) %>%
  group_rows("Reduction only", 5, 6) %>%
  group_rows("Cessation + reduction + increase", 7, 8) %>%
  footnote( 
    general = "Monte carlo performance estimates based on 1,000 simulated assignments and 100 bootstrap samples. ",
    threeparttable = TRUE,
    footnote_as_chunk = TRUE,
    escape = FALSE,
    general_title = "Notes:"
  )


save_kable(tab1, "table1.tex")
 
appendix_tab <- 
power_df %>%
  select(
    probs,
    empirical,
    treat_effects,
    estimator,
    power,
  ) %>%
  pivot_wider(
    names_from = c(empirical, estimator),
    names_glue = "{empirical}_{estimator}_{.value}",
    names_sort = TRUE,
    values_from = power,
  ) %>%
  select(-treat_effects) %>%
  kable(
    .,
    digits = 3,
    col.names = c("", rep(c("$Y_{binary}$", "$Y_{sum}$"), 9)),
    caption = "Monte carlo simulation of power using DHS data.\\label{tab:dhs_sims_power}",
    booktabs = TRUE,
    format = "latex",
    align = "lp{1cm}p{1cm}p{1cm}p{1cm}p{1cm}p{1cm}p{1cm}p{1cm}p{1cm}p{1cm}p{1cm}p{1cm}p{1cm}p{1cm}p{1cm}p{1cm}p{1cm}p{1cm}",
    escape = FALSE
  ) %>% 
  kable_styling(position = "center", font_size = 7) %>%
  add_header_above(
    c(" " = 1,
      "Ukraine (2007)" = 2,
      "Nepal (2016)" = 2,
      "Philippines (2017)" = 2,
      "Peru (2012)" = 2,
      "Dominican Republic (2013)" = 2, 
      "Tajikistan (2017)" = 2,
      "Uganda (2016)" = 2,
      "Nigeria (2013)" = 2,
      "Papua New Guinea (2016)" = 2),
  ) %>%
  group_rows("All acts", 1, 4) %>%
  group_rows("Physical only", 5, 8) %>%
  group_rows("Sexual only", 9, 12) %>%
  group_rows("Moderate only", 13, 16) %>%
  footnote( 
    general = "Monte carlo performance estimates based on 1,000 simulated assignments and 100 bootstrap samples. ",
    threeparttable = TRUE,
    footnote_as_chunk = TRUE,
    escape = FALSE,
    general_title = "Notes:"
  )

save_kable(appendix_tab, "tablea1.tex")

colors <-
  c(
    '#e6194B',
    '#3cb44b',
    '#ffe119',
    '#4363d8',
    '#f58231',
    '#42d4f4',
    '#f032e6',
    '#fabed4',
    '#469990',
    '#dcbeff',
    '#9A6324',
    '#fffac8',
    '#800000',
    '#aaffc3',
    '#000075',
    '#a9a9a9',
    '#ffffff',
    '#000000'
  )

power_df %>%
  ggplot(aes(
    y = power,
    x = estimator,
    linetype = probs,
    color = treat_effects,
    group = fct_cross(treat_effects, probs)
  )) +
  facet_grid(treat_effects~empirical) +
  geom_point() +
  geom_line() +
  geom_segment(aes(
    y = power - 1.96 * `se(power)`,
    yend = power + 1.96 * `se(power)`,
    xend = estimator
  )) +
  geom_text(aes(
    label = round(power, 2),
    x = ifelse(estimator == "Y[binary]",
               as.numeric(factor(estimator)) - 0.15,
               as.numeric(factor(estimator)) + 0.15)
  ),
  size = 3,
  ) +
  labs(y = expression("Power"~(beta)), x = "") +
  scale_x_discrete(labels = function(l) parse(text = l)) +
  scale_color_manual(
    name = "",
    values = colors,
    labels = c(
      "constant",
      "moderate only",
      "physical only",
      "sexual only"
    )
  ) +
  # scale_color_manual(
  #   name = "",
  #   values = colors,
  #   labels = c(
  #     "cessation",
  #     "cessation + reduction",
  #     "cessation + reduction + increase",
  #     "reduction"
  #   )
  # ) +
  theme_minimal(base_size = 14, base_family = "Palatino") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank(), legend.position = "none") 



power_df %>%
  filter(treat_effects == "constant") %>%
  ggplot(aes(
    y = power,
    x = estimator,
    linetype = probs,
    shape = probs,
    group = probs
  )) +
  facet_wrap(~empirical, nrow = 3, ncol = 3) +
  geom_point(size = 2.5) +
  geom_line() +
  # geom_text(aes(
  #   label = round(power, 2),
  #   x = ifelse(estimator == "Y[binary]",
  #              as.numeric(factor(estimator)) - 0.2,
  #              as.numeric(factor(estimator)) + 0.2)
  # ),
  # size = 3.5, family = "Palatino"
  # ) +
  labs(y = expression("Power"~(beta)), x = NULL) +
  scale_x_discrete(labels = function(l) parse(text = l)) +
  theme_minimal(base_size = 14, base_family = "Palatino") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "bottom"
  )

power_df %>%
  filter(treat_effects == "constant") %>%
  select(estimator, empirical, probs, power) %>%
  group_by(empirical, probs) %>%
  summarise(
    power_diff = diff(power),
    .groups = "drop"
  ) %>%
  ggplot(aes(
    y = probs,
    x = power_diff
    # shape = probs
  )) +
  facet_wrap(~empirical, nrow = 3, ncol = 3) +
  geom_vline(xintercept = 0, linetype = "dashed") + 
  geom_point(size = 2.5) +
  geom_text(aes(
    label = round(power_diff, 2),
    x = ifelse(power_diff < 0,
               power_diff - 0.2,
               power_diff + 0.2)
  ),
  size = 3.5, family = "Palatino"
  ) +
  labs(y = NULL, x = expression("Power difference"~(beta[Y[binary]] - beta[Y[sum]]))) +
  scale_x_discrete(labels = function(l) parse(text = l)) +
  theme_minimal(base_size = 14, base_family = "Palatino") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "bottom"
  )

power_df %>%
  filter(treat_effects == "constant") %>%
  select(estimator, empirical, probs, power) %>%
  group_by(empirical, probs) %>%
  summarise(
    power_diff = diff(power),
    .groups = "drop"
  ) %>%
  ggplot(aes(
    y = empirical,
    x = power_diff
    # shape = probs
  )) +
  facet_grid(~fct_relevel(probs, c("cessation", "reduction"))) +
  geom_vline(xintercept = 0, linetype = "dashed") + 
  geom_point(size = 2.5) +
  geom_text(aes(
    label = round(power_diff, 2),
    x = ifelse(power_diff < 0,
               power_diff - 0.2,
               power_diff + 0.2)
  ),
  size = 3.5, family = "Palatino"
  ) +
  labs(y = NULL, x = expression("Power difference"~(beta[Y[binary]] - beta[Y[sum]]))) +
  scale_x_discrete(labels = function(l) parse(text = l)) +
  scale_shape_discrete(labels = label_parsed) +
  theme_minimal(base_size = 14, base_family = "Palatino") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom"
  )

p <- 
  power_df %>%
  filter(treat_effects == "constant") %>%
  select(estimator, empirical, probs, power) %>%
  group_by(empirical, probs) %>%
  mutate(
    power_diff = diff(power)
  ) %>%
  ggplot(aes(
    y = empirical,
    x = power,
    shape = estimator,
    color = factor(I(power_diff > 0)),
    group = empirical
  )) +
  facet_grid(~fct_relevel(probs, c("cessation only", "cessation + reduction", "reduction only")), labeller = label_wrap_gen(width = 25)) +
  geom_point(size = 3) +
  geom_line(linetype = "dotted") +
  geom_text(aes(
    label = round(power, 2),
    x = ifelse(power_diff < 0 & estimator == "Y[sum]",
               power - 0.15,
               ifelse(power_diff < 0 & estimator == "Y[binary]",
                      power + 0.15,
               ifelse(power_diff >= 0 & estimator == "Y[sum]",
                      power + 0.15,
               ifelse(power_diff >= 0 & estimator == "Y[binary]",
                      power - 0.15, 
                      power
               ))))
  ),
  size = 4, family = "Palatino"
  ) +
  labs(y = NULL, x = bquote("Power"~(beta))) +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  scale_color_brewer(guide = "none", palette = "Set2") +
  scale_shape_discrete(name = "outcome", labels = scales::parse_format()) +
  coord_cartesian(clip = "off") +
  # scale_y_discrete(labels = function(l) parse(text = l), position = "right") +
  theme_minimal(base_size = 18, base_family = "Palatino") +
  theme(
    strip.text.y.left = element_text(angle = 0),
    axis.ticks = element_line(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "top"
  )

ggsave("07_results/02_figures/sims.pdf", p, width = 13, height = 7, units = "in")

power_df %>%
  filter(treat_effects == "constant") %>%
  mutate(rmse = if_else(estimator == "Y[sum]", rmse / 27, rmse))
  ggplot(aes(
    y = rmse,
    x = estimator,
    linetype = probs,
    shape = probs,
    group = probs
  )) +
  facet_wrap(~empirical, nrow = 3, ncol = 3) +
  geom_point(size = 2) +
  geom_line() +
  # geom_segment(aes(
  #   y = power - 1.96 * `se(power)`,
  #   yend = power + 1.96 * `se(power)`,
  #   xend = estimator
  # )) +
  geom_text(aes(
    label = round(rmse, 2),
    x = ifelse(estimator == "Y[binary]",
               as.numeric(factor(estimator)) - 0.2,
               as.numeric(factor(estimator)) + 0.2)
  ),
  size = 3.5, family = "Palatino"
  ) +
  labs(y = expression("Power"~(beta)), x = NULL) +
  scale_x_discrete(labels = function(l) parse(text = l)) +
  theme_minimal(base_size = 14, base_family = "Palatino") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "bottom"
  )

power_df %>%
  ggplot(aes(y = rmse, x = fct_cross(estimator), color = treat_effects)) +
  facet_wrap(~factor(empirical), nrow = 3, ncol = 3, scales = "free") +
  geom_point() +
  geom_segment(aes(y = rmse - 1.96 * `se(power)`, yend = rmse + 1.96 * `se(rmse)`, xend = estimator)) +
  geom_text(aes(
    label = round(power, 2),
    x = as.numeric(factor(estimator)) - 0.1
  ),
  size = 3
  ) +
  labs(x = "RMSE", y = "") +
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

