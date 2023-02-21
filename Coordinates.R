#create grouping ID variables by coordinates
xbreaks <- seq(0, 200, length.out = 21)
ybreaks <- seq(0, 85, length.out = 15)
id <- matrix(seq_len(length(xbreaks) * length(ybreaks)),
             length(xbreaks), length(ybreaks))
xi <- findInterval(games$x, xbreaks)
yi <- findInterval(games$y, ybreaks)
games$coord_id = id[cbind(xi, yi)]
unique(sort(games$coord_id)) #check coord_id in games

x2 <- findInterval(games$x2, xbreaks)
y2 <- findInterval(games$y2, ybreaks)
games$coord_id_2 <- id[cbind(x2, y2)]

xbrk <- seq(5, 195, length.out = 20)
ybrk <- seq(6.07 / 2, 78.93 + (6.07 / 2), length.out = 14)
points <- merge(xbrk, ybrk)
x_p <- findInterval(points$x, xbreaks)
y_p <- findInterval(points$y, ybreaks)
points$id <- id[cbind(x_p, y_p)]


points <- points %>%
  mutate(
    x_min = x - 5,
    x_max = x + 5,
    y_min = y - (6.07 / 2),
    y_max = y + (6.07 / 2)
  )
colnames(points) <-
  c('x_group',
    'y_group',
    'coord_id',
    'x_min',
    'x_max',
    'y_min',
    'y_max')