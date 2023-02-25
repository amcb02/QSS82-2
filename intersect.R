# Function to determine if two line segments intersect
# Function to determine if two line segments intersect
segments_intersect <- function(x, y, x2, y2) {
  # Calculate the slope of each line segment
  slope1 <- (y2 - y) / (x2 - x)
  slope2 <- (42.5 - 42.5) / (189 - 154)
  
  # Check if the line segments are parallel
  if (is.na(slope1) || is.na(slope2) || slope1 == slope2) {
    return(FALSE)
  }
  
  # Calculate the intersection point
  x_intersect <- (slope1*x - y - slope2*154 + 42.5) / (slope1 - slope2)
  y_intersect <- slope1*(x_intersect - x) + y
  
  # Check if the intersection point is within the bounds of both line segments
  if (is.na(x) || is.na(x2) || is.na(x_intersect) ||
      x_intersect < min(x, x2) || x_intersect > max(x, x2) ||
      y_intersect < min(y, y2) || y_intersect > max(y, y2) ||
      x_intersect < min(154, 189) || x_intersect > max(154, 189) ||
      y_intersect < min(42.5, 42.5) || y_intersect > max(42.5, 42.5)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

# Create a new column "intersect" in house_events
house_events$intersect <- NA
non_house_events$intersect <- NA

# Loop over each row in house_events and determine if the line segment intersects with the second line segment
for(i in 1:nrow(house_events)) {
  intersect <- segments_intersect(house_events[i,'x'], house_events[i,'y'], house_events[i,'x2'], house_events[i,'y2'])
  house_events[i,'intersect'] <- ifelse(intersect, T, F)
}

# Loop over each row in non_house_events and determine if the line segment intersects with the second line segment
for(i in 1:nrow(non_house_events)) {
  intersect <- segments_intersect(non_house_events[i,'x'], non_house_events[i,'y'], non_house_events[i,'x2'], non_house_events[i,'y2'])
  non_house_events[i,'intersect'] <- ifelse(intersect, T, F)
}


