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