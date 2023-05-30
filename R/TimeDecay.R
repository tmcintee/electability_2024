TimeDecay <- function(year,mode = "Exp",value = 4)
{
  weight <- 1
  diff_year <- year-2020
  if(mode == "Exp")
  {
    weight <- 2^(diff_year/value)
  }
  return(weight)
}
