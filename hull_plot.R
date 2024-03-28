# Hull plot model Fearon


da_vec <- seq(1,1000, 1)/1001
db_vec <- seq(1,1000, 1)/1001

lista <- list()
lista1 <- list()
for ( i in 1:length(da_vec)) {
  for (j in 1:length(da_vec)) {
    ua_tmp <- (1 - db_vec[j])/(1 - da_vec[i]*db_vec[j])
    if(ua_tmp > .3 & 1-ua_tmp > .3) {
      lista[[i + length(da_vec)*(j-1)]] <- setNames(c(da_vec[i], db_vec[j]), c("da", "db"))
      lista1[[i + length(da_vec)*(j-1)]] <- c(ua_tmp, 1-ua_tmp)
    }
  }
}

# New code to filter out NULL elements
lista_non_null <- Filter(Negate(is.null), lista)
lista1_non_null <- Filter(Negate(is.null), lista1)
# Optionally, check the length of the original and the filtered list
print(paste("Original list length:", length(lista)))
print(paste("Non-null list length:", length(lista_non_null)))


library(ggplot2)
library(dplyr)

df <- bind_rows(lista_non_null)

library(tidyverse)

# Assuming 'df' contains your da and db columns

# Find the convex hull of the points being plotted
hull <- df %>%
  slice(chull(da, db))

# Define the scatterplot
p <- ggplot(df, aes(da, db)) + xlim(0,1) + ylim(0,1) +
  theme_bw()

# Overlay the convex hull
p <- p + geom_polygon(data = hull, aes(x = da, y = db), alpha = 0.5, fill = "lightblue")
p + xlab(expression(delta[A])) + ylab(expression(delta[B]))
