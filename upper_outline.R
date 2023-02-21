upper_outline = data.frame(
  x = c(125,
        172 + 28 * sin(seq(0, pi / 2, length = 20)),
        172 + 28 * sin(seq(pi / 2, 0, length = 20)),
        125),
  y = c(0,
        0 + 28 - 28 * cos(seq(0, pi / 2, length = 20)),
        85 - 28 + 28 * cos(seq(pi / 2, 0, length = 20)),
        85)
)