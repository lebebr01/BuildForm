library(dplyr)
library(plink)
library(tidyr)

# Minimal example for testing of interactive code
tmp <- read.csv('Data/itempar.csv')
tmp2 <- filter(tmp, itemnumber %in% c(1, 5))

params2 <- tmp2 %>%
  select(a, b, c) %>%
  filter(is.na(a) == FALSE)
params2 <- data.frame(params2)
names(params2) <- c('a', 'b', 'c')

t1 <- data.frame(drm(params2, seq(-5, 5, by = .1))@prob)
item_names <- paste("item", tmp2[, 'itemnumber'], sep = "_")
colnames(t1) <- c("theta1", item_names)
t1_names <- paste0(names(t1)[2], ':', names(t1)[ncol(t1)])
t1 <- t1 %>%
  gather(item, prob, eval(parse(text = t1_names)))

write.csv(t1, file = "Data/tccdat.csv")
