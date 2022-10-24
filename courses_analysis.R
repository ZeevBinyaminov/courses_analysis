courses <- read.csv("courses2.csv")
courses$log_num_subscribers <- log(courses$num_subscribers)
years_in_2020 <- 2020 - as.integer(format(as.Date(courses$published_time),"%Y"))
courses$is_new <- ifelse(years_in_2020 < 5, 1, 0)

lin_mod <- lm(avg_rating ~ log_num_subscribers +  num_published_practice_tests + is_new, data=courses)
summary(lin_mod)

plot(lin_mod, which=1)
plot(lin_mod, which=2)
plot(lin_mod, which=4)

courses$residuals <- lin_mod$residuals
courses$fitted <- lin_mod$fitted.values

summary(courses$residuals)


library(ggplot2)
ggplot(data = courses, aes(x = residuals)) +
geom_histogram(fill = "green", color = "black") + 
xlab("Residuals")

shapiro.test(courses$residuals[5000:10000])
ks_test_sample <- order(unique(courses$residuals))
ks.test(ks_test_sample, 'pnorm')

library(lmtest)
bptest(lin_mod)