# Functions for the spaghetti tutorial
# Created by Kaija Gahm
# 31 January 2020

# You are given a box of pasta
box <- data.frame(ingredient = "pasta", cooked = 0)

# Heat the water
cook <- function(df, ingredient = NULL, minutes = 1){
  if(is.null(ingredient)){
    stop("Which ingredient would you like to cook?")
  } else if(ingredient == "pasta" & !("water" %in% df$ingredient)){
    stop("If you try to cook pasta without water, it'll burn! Don't do that.")
  } else if(ingredient %in% c("pasta", "water") & !(ingredient %in% df$ingredient)){
    stop("That ingredient isn't in the pot yet.")
  } else if(!(ingredient %in% c("pasta", "water"))){
    stop("Ew, why are you putting THAT in your pasta water? Try again.")
  } else{
  df <- df %>% mutate(cooked = case_when(cooked + minutes < 10 ~ cooked + minutes,
                                     cooked + minutes >= 10 ~ 10))
  return(df)
  } 
}