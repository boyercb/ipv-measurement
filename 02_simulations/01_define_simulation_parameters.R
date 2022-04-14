
N <- 1680

tau_models <- list(
  # constant effect across all acts
  constant = function(x, col) {
    ifelse(
      x > 0,
      x - 1,
      x
    )
  },
  # effect on physical violence only 
  physical_only = function(x, col) {
    ifelse(
      x > 0 & col %in% paste0('u', 1:7),
      x - 1,
      x
    )
  },
  # effect on sexual violence only
  sexual_only = function(x, col) {
    ifelse(
      x > 0 & col %in% paste0('u', 8:10),
      x - 1,
      x
    )
  },
  # effect on moderate physical violence only 
  moderate_only = function(x, col) {
    ifelse(
      x == 1 & col %in% paste0('u', 1:2),
      x - 1,
      x
    )
  },
  # reduction in moderate physical violence, slight increase in severe 
  severe_backlash = function(x, col) {
    ifelse(
      x > 0 & col %in% paste0('u', 1:2),
      x - 1,
      ifelse(
        x > 0 & col %in% paste0('u', 3:10),
        x + 1,
        x
      )
    )
  },
  # reduction in physical violence, increase in sexual (perhaps due to awareness)
  sexual_backlash = function(x, col) {
    ifelse(
      x > 0 & col %in% paste0('u', 1:7),
      x - 1,
      ifelse(
        x > 0 & col %in% paste0('u', 8:10),
        x + 1,
        x
      )
    )
  }
)
