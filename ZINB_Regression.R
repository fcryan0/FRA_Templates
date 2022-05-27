require(ggplot2)
require(pscl)
require(MASS)
require(boot)


zinb <- read.csv("https://stats.idre.ucla.edu/stat/data/fish.csv")
zinb <- within(zinb, {
  nofish <- factor(nofish)
  livebait <- factor(livebait)
  camper <- factor(camper)
})

summary(zinb)


## histogram with x axis in log10 scale
ggplot(zinb, aes(count, fill = camper)) +
  geom_histogram() +
  #scale_x_log10() +
  facet_grid(camper ~ ., margins=TRUE, scales="free_y")


m1 <- zeroinfl(count ~ child + camper | persons,
               data = zinb, dist = "negbin", EM = TRUE)
summary(m1)