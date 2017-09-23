library(tidyverse)

random_walk <- function(time = 10000, 
                        gradient = c("red","yellow","green", "lightblue","darkblue"),
                        step_size = 1) {
  
  stopifnot(is.numeric(time), is.character(gradient), is.numeric(step_size))
  
  walk <- (1:time)
  
  x <- vector("integer", length = length(walk))
  y <- vector("integer", length = length(walk))
  
  walk <- cbind(walk, x, y) %>% 
    as.tibble() %>% 
    mutate(random = runif(time) * 2 * pi,
           dx = sin(random),
           dy = cos(random))
  
  for (i in seq_along(1:(time-1))) {
    walk$x[i + 1] <- walk$x[i] + walk$dx[i + 1] * step_size
    walk$y[i + 1] <- walk$y[i] + walk$dy[i + 1] * step_size
  }
  
  plot <- ggplot(walk, aes(x, y, color = walk)) +
    geom_path() + 
    labs(color = "Time") +
    scale_colour_gradientn(colors = gradient) +
    theme_minimal()
  
  print(plot)
}


random_walk()

  