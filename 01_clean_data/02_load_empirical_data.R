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
  "EGIR61DT/EGIR61FL.DTA"  # Egypt 2014
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
      file == "UAIR51DT/UAIR51FL" ~ "Ukraine",
      file == "NPIR7HDT/NPIR7HFL" ~ "Nepal",
      file == "PHIR70DT/PHIR70FL" ~ "Philippines",
      file == "PEIR61DT/PEIR61FL" ~ "Peru",
      file == "COIR72DT/COIR72FL" ~ "Colombia",
      file == "DRIR6ADT/DRIR6AFL" ~ "Dominican Republic",
      file == "TJIR70DT/TJIR70FL" ~ "Tajikistan",
      file == "UGIR7ADT/UGIR7AFL" ~ "Uganda",
      file == "NGIR6ADT/NGIR6AFL" ~ "Nigeria",
      file == "EGIR61DT/EGIR61FL" ~ "Egypt"
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
    twist = if_else(country == "Peru", attacked, twist)
  ) %>%
  select(-attacked)

# make table
summary_dhs <- 
  dhs %>%
  select(-file, -v044) %>%
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


dhs <-
  dhs %>%
  select(-v044, -file) %>%
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
  drop_na(u1)
