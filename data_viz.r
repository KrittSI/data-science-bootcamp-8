## load ggplot library
## update this file, just another comment

install.packages("ggplot2")
library(ggplot2)

ggplot(diamonds, aes(carat, price)) +
  geom_point()
