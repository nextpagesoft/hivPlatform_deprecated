ConvertTimeToQuarter <- function(time) {
  return(sprintf('%dQ%d', floor(time), (time %% 1 + 0.125) * 4))
}
