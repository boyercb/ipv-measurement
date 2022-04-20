elw <- read_rds("../../3_data/elw.rds")

simdata <- select(elw, all_of(act_variables))
colnames(simdata) <- paste0("u", 1:10)

# load DHS data -----------------------------------------------------------

dhs_path <- "../../3_data/DHS/"

dta_list <- list(
  "UAIR51DT/UAIR51FL.DTA", # Ukraine 2007
  "NPIR7HDT/NPIR7HFL.DTA", # Nepal 2016
  "PHIR70DT/PHIR70FL.DTA", # Philippines 2017
  "PEIR61DT/PEIR61FL.DTA", # Peru 2012
  "DRIR6ADT/DRIR6AFL.DTA", # Dominican Republic 2013
  "TJIR70DT/TJIR70FL.DTA", # Tajikistan 2017
  "UGIR7ADT/UGIR7AFL.DTA", # Uganda 2016
  "NGIR6ADT/NGIR6AFL.DTA", # Nigeria 2013
  "PGIR71DT/PGIR71FL.DTA"  # Papua New Guinea 2016
)

import_dta <- function(f) {
  print(str_c("Importing data from...", f))
  dta <-
    read_stata(file = str_c(dhs_path, f),
               col_select = c(v044, d105a:d105k))
  dta <- zap_labels(dta)
  f <- str_remove(f, str_c(".", file_ext(f)))
  
  dta %>%
    mutate(file = f) %>%
    filter(v044 == 1) 
}

dhs <-
  dta_list %>%
  map_dfr(import_dta) 

dhs <- 
  dhs %>%
  as_tibble() %>%
  mutate(
    country = case_when(
      file == "UAIR51DT/UAIR51FL" ~ "Ukraine (2007)",
      file == "NPIR7HDT/NPIR7HFL" ~ "Nepal (2016)",
      file == "PHIR70DT/PHIR70FL" ~ "Philippines (2017)",
      file == "PEIR61DT/PEIR61FL" ~ "Peru (2012)",
      file == "DRIR6ADT/DRIR6AFL" ~ "Dominican Republic (2013)",
      file == "TJIR70DT/TJIR70FL" ~ "Tajikistan (2017)",
      file == "UGIR7ADT/UGIR7AFL" ~ "Uganda (2016)",
      file == "NGIR6ADT/NGIR6AFL" ~ "Nigeria (2013)",
      file == "PGIR71DT/PGIR71FL" ~ "Papua New Guinea (2016)"
    ),
    across(
    .cols = starts_with("d"),
    .fns = as.integer
    ), 
    across(
      .cols = starts_with("d"),
      .fns = ~ case_when(
        . == 0 ~ 0,
        . == 1 ~ 3,
        . == 2 ~ 2,
        . == 3 ~ 1,
        TRUE ~ NA_real_
      )
    )
  ) %>%
  rename(
    pushed = d105a,
    slapped = d105b,
    punched = d105c,
    kicked = d105d,
    strangled = d105e,
    weapon = d105f,
    attacked = d105g,
    forcedsex = d105h,
    pressuresex = d105i,
    twist = d105j,
    degrade = d105k
  ) %>%
  mutate(
    twist = if_else(country == "Peru (2012)", attacked, twist)
  ) %>%
  select(-attacked, -v044, -file)

# make table
summary_dhs <- 
  dhs %>%
  pivot_longer(-country) %>%
  drop_na() %>%
  group_by(country, name) %>%
  mutate(total = n()) %>%
  group_by(country, name, value) %>%
  summarise(pct = n() / first(total), .groups = "drop") %>%
  pivot_wider(names_from = country, values_from = pct) %>%
  arrange(name, value) %>%
  mutate(across(.cols = -c(name, value), ~ .x * 100))

kable(
  x = summary_dhs,
  format = "html",
  digits = 1
) %>%
  kable_styling()

summary_plot_dhs <- 
  dhs %>%
  pivot_longer(-country) %>%
  drop_na() %>%
  group_by(country, name) %>%
  mutate(total = n()) %>%
  group_by(country, name, value) %>%
  summarise(pct = n() / first(total), .groups = "drop") %>%
  filter(value != 0) %>%
  mutate(
    value = factor(value, labels = c("Once", "A few times", "Many times")),
    name = factor(name, levels = c(
      "pushed",
      "slapped",
      "punched",
      "kicked",
      "twist",
      "strangled",
      "weapon",
      "forcedsex",
      "pressuresex",
      "degrade"
    ))
  )

p <- 
  ggplot(summary_plot_dhs,
         aes(
           x = fct_rev(value),
           y = pct,
           fill = fct_rev(value),
           color = fct_rev(value)
         )) +
  geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(round(pct * 100, 1), "%")), hjust = -0.15, size = 3.5) +
    coord_flip(clip = 'off') +
    facet_grid(name ~ country,
               switch = "y",
               scales = "free_y",
               space = "free_y", labeller = label_wrap_gen(width = 16, multi_line = TRUE)) +
    theme_minimal(base_family = "Palatino", base_size = 14) +
    scale_fill_brewer(palette = "Reds", direction = -1) +
    scale_color_brewer(palette = "Reds", direction = -1) +
    scale_y_continuous(breaks = c(0, 0.10, 0.20), limits = c(0, 0.28)) +
    theme(
      legend.position = 'none',
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.spacing.y = unit(0, "lines"),
      panel.spacing.x = unit(1, "lines"),
      strip.background = element_blank(),
      strip.placement = "outside"
    ) +
    labs(x = NULL, y = NULL)

ggsave(
  filename = "07_results/02_figures/dhs_pmfs.pdf",
  plot = p,
  device = "pdf",
  width = 15,
  height = 10
)

dhs <-
  dhs %>%
  rename(
    u1 = pushed,
    u2 = slapped,
    u3 = punched,
    u4 = kicked,
    u5 = twist,
    u6 = strangled,
    u7 = weapon,
    u8 = forcedsex,
    u9 = pressuresex,
    u10 = degrade
  ) %>%
  select(country, u1:u10) %>%
  drop_na(u1:u9)
