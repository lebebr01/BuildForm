# Generate Item Parameters
itempar <- data.frame(
  subject = "Math",
  itemnumber = 1:100,
  a = rnorm(100, mean = 1, sd = .3),
  b = rnorm(100),
  c = rnorm(100, mean = 0.25, sd = .03)
)

# save to csv file
write.csv(itempar, file = "Data/itempar.csv")

# Add in a gender variable for group analysis
itempar2 <- data.frame(
  subject = "Math",
  gender = rep(c("Female", "Male"), each = 100),
  itemnumber = rep(1:100, 2),
  a = rnorm(200, mean = 1, sd = .3),
  b = c(rnorm(100), rnorm(100, mean = 0.5)),
  c = rnorm(200, mean = 0.25, sd = .03)
)

# save to file
write.csv(itempar2, file = "Data/itempar_gender.csv")
