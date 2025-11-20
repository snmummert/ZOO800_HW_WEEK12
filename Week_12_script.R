#########################################################
##################### HW Week 12 ########################
### Sophia Mummert, Samantha Summerfield, Maddie Thall ###
##########################################################

## libraries ##
library(readxl)
library(dplyr)
library(ggplot2)


fish_data = read_excel("BSB_tagging_data.xlsx")
fish_data = na.omit(fish_data)

######## Objective 1 ########

obj1_data = fish_data[format(fish_data$Date_at_recapture, "%m") > "07", ]
OG_females = obj1_data[obj1_data$Sex_at_capture == "F", ]
changed = sum(OG_females$Sex_at_recapture != "F")
total = nrow(OG_females)

prop_changed = changed / total
alpha = changed + 1
beta = (total - changed) + 1
CI = qbeta(c(0.025, 0.975), alpha, beta)
print(CI)

x = seq(0, 1, length.out = 500)
y = dbeta(x, alpha, beta)

plot(x, y, type = "l", lwd = 2, col = "blue",
     xlab = "Proportion of females that changed sex",
     ylab = "Probability density",
     main = "Beta PDF for sex change proportion")
abline(v = prop_changed, col = "red", lwd = 2, lty = 2)
abline(v = CI[1], col = "darkgreen", lwd = 2, lty = 3)
abline(v = CI[2], col = "darkgreen", lwd = 2, lty = 3)

######## Objective 2 ########
OG_females$sexchange = ifelse(OG_females$Sex_at_capture == "F" &
                                OG_females$Sex_at_recapture %in% c("M", "I"), 1, 0)
model = glm(sexchange ~ Length_at_capture,
            data = OG_females, family = binomial)
summary(model)
# the p value of Length_at_capture is 0.0416, which indicates that there is a 
# significant relationship between length at capture and sex change for fish
# recaptured after July 1st.

coef(model)["Length_at_capture"]
# the log odds of sex change increase by approx 0.0853
# each mm increase in length at capture.

obj2_data = data.frame(Length_at_capture = seq(min(OG_females$Length_at_capture),
                                               max(OG_females$Length_at_capture),
                                               length.out = 100))
obj2_data$predicted_prob = predict(model, newdata = obj2_data, type = "response")

ggplot(OG_females, aes(x = Length_at_capture, y = sexchange)) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.5) +
  geom_line(data = obj2_data, 
            aes(x = Length_at_capture, y = predicted_prob),
            color = "darkorchid", size = 1.2) +
  labs(x = "Length at Capture (mm)",
       y = "Probability of Sex Change",
       title = "Probability of Sex Change vs. Length in Black Sea Bass")