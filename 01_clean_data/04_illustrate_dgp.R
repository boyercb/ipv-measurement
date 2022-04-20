
set.seed(83764)

# single-act model --------------------------------------------------------

single_act_sim <- tibble(
  sim = 1:840,
  push = rbinom(840, 1, 1 - theta[2]) * rpois(840, lambda[2]),
  nb_push = rbinom(840, 1, 1 - nb_theta[2]) * 
    rnbinom(840, mu = nb_lambda[2], size = nb_phi[2])
) 

single_act_sim <- categorize_counts(single_act_sim, c("push", "nb_push"))

p1 <- 
  ggplot() +
  geom_bar(
    aes(x = physical_push_5mo_freq_i_w, y = ..prop.., fill = "Observed"),
    data = subset(elw, Z == 0),
    #binwidth = 1,
    alpha = 0.45
  ) + 
  geom_bar(
    aes(x = push_star, y = ..prop.., fill = "Simulated"),
    data = single_act_sim,
    #binwidth = 1,
    alpha = 0.45
  ) + 
  geom_text(
    aes(x = physical_push_5mo_freq_i_w,
        label = round(prop, 2),
        y = prop + 0.02),
    position = position_dodge(0.9),
    size = 3,
    family = "Palatino",
    vjust = 0,
    data = subset(elw, Z == 0) %>% 
      group_by(physical_push_5mo_freq_i_w) %>%
      summarise(prop = n() / nrow(subset(elw, Z == 0)))
  ) +
  theme_minimal(base_family = "Palatino", base_size = 11) +
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  scale_fill_brewer(name = "", palette = "Set1") +
  labs(
       x = "\nViolence category (Y*)",
       y = "Probability\n"
  )

ggsave(
  filename = "07_results/02_figures/single_act_zip.pdf",
  plot = p1,
  device = "pdf",
  width = 3.5,
  height = 3
)

p2 <- 
  ggplot() +
  geom_bar(
    aes(x = physical_push_5mo_freq_i_w, y = ..prop.., fill = "Observed"),
    data = subset(elw, Z == 0),
    #binwidth = 1,
    alpha = 0.45
  ) + 
  geom_bar(
    aes(x = nb_push_star, y = ..prop.., fill = "Simulated"),
    data = single_act_sim,
    #binwidth = 1,
    alpha = 0.45
  ) + 
  geom_text(
    aes(x = physical_push_5mo_freq_i_w,
        label = round(prop, 2),
        y = prop + 0.02),
    position = position_dodge(0.9),
    size = 3,
    vjust = 0,
    family = "Palatino",
    data = subset(elw, Z == 0) %>% 
      group_by(physical_push_5mo_freq_i_w) %>%
      summarise(prop = n() / nrow(subset(elw, Z == 0)))
  ) +
  theme_minimal(base_family = "Palatino", base_size = 11) +
  theme(legend.position = c(0.8, 0.87), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  scale_fill_brewer(name = "", palette = "Set1") +
  labs(
    x = "\nViolence category (Y*)",
    y = "Probability\n"
  )

ggsave(
  filename = "07_results/02_figures/single_act_zinb.pdf",
  plot = p2,
  device = "pdf",
  width = 3.5,
  height = 3
)


# multiple act model ------------------------------------------------------

design <- ipv_design(
  N = 1680,
  lambda = nb_lambda,
  theta = nb_theta,
  phi = nb_phi,
  Rho = Rho,
  tau = function(x, col, type) { x }
)

df <- draw_data(design)

p1 <- ggplot(df, aes(x = Y_star)) +
  geom_bar(aes(y = ..prop..)) +
  theme_minimal(base_family = "Palatino", base_size = 11) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  labs(x = "",
       y = "Probability\n"
  )

acts <- c(
  "slaps",
  "pushes",
  "punches",
  "twists",
  "kicks",
  "chokes",
  "weapon",
  "forcesex",
  "pressuresex",
  "degrade"
)

df_long <- 
  df %>% 
  select(
    Y1_star,
    Y2_star,
    Y3_star,
    Y4_star,
    Y5_star,
    Y6_star,
    Y7_star,
    Y8_star,
    Y9_star,
    Y10_star
  ) %>%
  gather(key, value) %>%
  mutate(key = str_remove(key, "_star"))

p2 <- 
  ggplot(df_long, aes(x = value)) +
  geom_bar(aes(y = ..prop..)) +
  facet_wrap(~factor(key, levels = paste0("Y", 1:10), labels = acts), ncol = 5) + 
  theme_minimal(base_family = "Palatino", base_size = 11) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  labs(x = "\nViolence category (Y*)",
       y = "Probability\n"
       # title = "PMF of each act"
  )

cmat <- df[, paste0(paste0("Y", 1:10))]
colnames(cmat) <- acts

p3 <- 
  ggcorrplot::ggcorrplot(
    cor(cmat),
    type = 'full',
    show.diag = TRUE,
    legend.title = "Correlation"
    # lab = TRUE,
    # lab_col = "black",
    # lab_size = 2.75,
  ) +
  labs(
    title = NULL, x = NULL, y = NULL
  ) +
  geom_text(aes(label = round(value, 2)), size = 2, family = "Palatino") +
  theme_minimal(base_family = "Palatino", base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    legend.position = "none",
    axis.title = element_blank()) 



ggsave(
  filename = "07_results/02_figures/multiple_act_pmf.pdf",
  plot = p1,
  device = "pdf",
  width = 6.7,
  height = 2
)

ggsave(
  filename = "07_results/02_figures/multiple_act_pmf_act.pdf",
  plot = p2,
  device = "pdf",
  width = 6.7,
  height = 2.75
)

ggsave(
  filename = "07_results/02_figures/multiple_act_corr.pdf",
  plot = p3,
  device = "pdf",
  width = 5,
  height = 3
)

