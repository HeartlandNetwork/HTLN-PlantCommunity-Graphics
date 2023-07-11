
# NOT RUN {
library(ggplot2)
library(cowplot)

df <- data.frame(
  x = 1:10, y1 = 1:10, y2 = (1:10)^2, y3 = (1:10)^3, y4 = (1:10)^4
)

df


p1 <- ggplot(df, aes(x, y1)) + geom_point()
p2 <- ggplot(df, aes(x, y2)) + geom_point()
p3 <- ggplot(df, aes(x, y3)) + geom_point()
p4 <- ggplot(df, aes(x, y4)) + geom_point()
p5 <- ggplot(mpg, aes(as.factor(year), hwy)) +
  geom_boxplot() +
  facet_wrap(~class, scales = "free_y")
# simple grid
#plot_grid(p1, p2, p3, p4)

plot_grid(p1)


# simple grid with labels and aligned plots
plot_grid(
  p1, p2, p3, p4,
  labels = c('A', 'B', 'C', 'D'),
  align="hv"
)

# manually setting the number of rows, auto-generate upper-case labels
plot_grid(p1, p2, p3,
          nrow = 3,
          labels = "AUTO",
          label_size = 12,
          align = "v"
)
