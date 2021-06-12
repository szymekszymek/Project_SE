df <- data.frame(x = c("a","b","c"), y = c(1,2,3))

ggplot(data = df) +
  geom_rect(data = df, aes(x = x, y=y), xmin = as.numeric(df$x[[2]]) - 0.3,
            xmax = as.numeric(df$x[[3]]) + 0.3,
            ymin = 0, ymax = 2)
