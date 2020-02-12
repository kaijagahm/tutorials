# Functions for the spaghetti tutorial
# Created by Kaija Gahm
# 31 January 2020

# You are given a box of pasta and an empty pot
box <- data.frame(ingredient = as.character("pasta"), cooked = 0)
pot <- data.frame(ingredient = as.character("air"), cooked = 0)

# Fill the pot
fill <- function(df){
  df %>% mutate(ingredient = recode(ingredient, "air" = "water")) %>% return()
}

#Add pasta to the pot
add <- function(what = "pasta", from = box, to = pot){
  from %>% filter(ingredient == what) %>% full_join(to) %>% return()
}

# add <- function(from = box, to = pot){
#   full_join(box, pot) %>% return()
#   box <<- data.frame(ingredient = "air",
#                      cooked = 0)
# }

# Cook (heat) the pasta (water)
cook <- function(df, what = NULL, minutes = 1){
  if(is.null(what)){
    stop("Which ingredient would you like to cook?")
  } else if(what == "pasta" & !("water" %in% df$ingredient)){
    stop("If you try to cook pasta without water, it'll burn! Don't do that.")
  } else if(what %in% c("pasta", "water") & !(what %in% df$ingredient)){
    stop("That ingredient isn't in the pot yet.")
  } else if(!(what %in% c("pasta", "water"))){
    stop("Ew, why are you putting THAT in your pasta water? Try again.")
  } else{
    df <- df %>% mutate(cooked = case_when(cooked + minutes < 10 & ingredient == what ~ cooked + minutes,
                                           cooked + minutes >= 10 & ingredient == what ~ 10,
                                           TRUE ~ cooked)) %>% return()
  } 
}


# Strain the pasta from the water
drain <- function(df){
  df %>% filter(ingredient != "water") %>% return()
}
