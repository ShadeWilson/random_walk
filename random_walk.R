library(tidyverse)

random_walk <- function(time = 10000) {
  walk <- (1:time)
  
  x <- vector("integer", length = length(walk))
  y <- vector("integer", length = length(walk))
  
  random_direction <- function(a) {
    return <- vector("character", length = length(a))
    index <- 1
    
    for (value in a) {
      return[index] <- switch(value, "up", "down", "left", "right", "stay")
      index <- index + 1
    }
    return
  }
  
  walk <- cbind(walk, x, y) %>% 
    as.tibble() %>% 
    mutate(random = floor(runif(time) * 5) + 1)
  
  walk$direction <- random_direction(walk$random)
  
  for (i in seq_along(1:(time-1))) {
    if (walk$direction[i + 1] %in% c("right", "left")) {
      walk$x[i + 1] <- if_else(walk$direction[i + 1] == "left", walk$x[i] - 1, walk$x[i] + 1)
      walk$y[i + 1] <- walk$y[i]
    } else if (walk$direction[i + 1] %in% c("up", "down")) {
      walk$y[i + 1] <- if_else(walk$direction[i + 1] == "down", walk$y[i] - 1, walk$y[i] + 1)
      walk$x[i + 1] <- walk$x[i]
    } else {
      walk$x[i + 1] <- walk$x[i]
      walk$y[i + 1] <- walk$y[i]
    }
  }
  walk <- walk %>% 
    plyr::rename(replace = c(walk = "time"))
  
  plot <- ggplot(walk, aes(x, y, color = time)) +
    geom_path()
  
  print(plot)
}

# TODO: use 0-2pi for random number, use that as angle for sin and cos to get x and y

random_walk()
  