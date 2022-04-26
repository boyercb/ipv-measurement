
N <- 2000

tau_models <- list(
  # constant effect across all acts
  constant = function(x, col, type) {
    ifelse(
      x > 0,
      (type == "1") * x + 
        (type == "2") * 0 + 
        (type == "3") * ((x > 1) * (x - 1) + (x <= 1) * x) +
        (type == "4") * ((x < 3) * (x + 1) + (x == 3) * 3),
      x
    )
  },
  # effect on physical violence only 
  physical_only = function(x, col, type) {
    ifelse(
      x > 0 & col %in% paste0('u', 1:7),
      (type == "1") * x + 
        (type == "2") * 0 + 
        (type == "3") * ((x > 1) * (x - 1) + (x <= 1) * x) +
        (type == "4") * ((x < 3) * (x + 1) + (x == 3) * 3),
      x
    )
  },
  # effect on sexual violence only
  sexual_only = function(x, col, type) {
    ifelse(
      x > 0 & col %in% paste0('u', 8:10),
      (type == "1") * x + 
        (type == "2") * 0 + 
        (type == "3") * ((x > 1) * (x - 1) + (x <= 1) * x) +
        (type == "4") * ((x < 3) * (x + 1) + (x == 3) * 3),
      x
    )
  },
  # effect on moderate physical violence only 
  moderate_only = function(x, col, type) {
    ifelse(
      x > 0 & col %in% paste0('u', 1:2),
      (type == "1") * x + 
        (type == "2") * 0 + 
        (type == "3") * ((x > 1) * (x - 1) + (x <= 1) * x) +
        (type == "4") * ((x < 3) * (x + 1) + (x == 3) * 3),
      x
    )
  }
)
