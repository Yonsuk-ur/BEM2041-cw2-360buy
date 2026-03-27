library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

data <- read_excel("~/Downloads/360buy_SurveyData.xlsx", sheet = "Sheet1")

seg_vars <- data[, c("CusChoice", "ConstUp", "ReplacReminder", "ProdReturn", "ProInsuCov")]

m <- apply(seg_vars, 2, mean)
s <- apply(seg_vars, 2, sd)
z <- scale(seg_vars, m, s)

wss <- numeric(7)
for (k in 2:8) {
  set.seed(123)
  km <- kmeans(z, centers = k, nstart = 10)
  wss[k - 1] <- km$tot.withinss
}

png("~/Downloads/elbow_plot.png", width = 800, height = 500)
plot(2:8, wss, type = "b", pch = 19,
     xlab = "Number of Clusters",
     ylab = "Total Within-Cluster Sum of Squares",
     main = "Elbow Plot")
dev.off()

set.seed(123)
k4 <- kmeans(z, centers = 4, nstart = 10)
data$Cluster <- as.factor(k4$cluster)

segment_means <- data %>%
  group_by(Cluster) %>%
  summarise(
    Size_n = n(),
    Size_Pct = round(n() / nrow(data) * 100, 1),
    Age_Mean = round(mean(CusAgeYr), 1),
    Female_Pct = round(mean(CusGen) * 100, 1),
    Income_Mean = round(mean(LevIncome), 2),
    Choice_Mean = round(mean(CusChoice), 2),
    Updates_Mean = round(mean(ConstUp), 2),
    Replace_Mean = round(mean(ReplacReminder), 2),
    Return_Mean = round(mean(ProdReturn), 2),
    Insurance_Mean = round(mean(ProInsuCov), 2),
    HasAccount_Pct = round(mean(CusAcct) * 100, 1)
  )

segment_medians <- data %>%
  group_by(Cluster) %>%
  summarise(
    Choice_Med = median(CusChoice),
    Updates_Med = median(ConstUp),
    Replace_Med = median(ReplacReminder),
    Return_Med = median(ProdReturn),
    Insurance_Med = median(ProInsuCov)
  )

seg_long <- data %>%
  select(Cluster, CusChoice, ConstUp, ReplacReminder, ProdReturn, ProInsuCov) %>%
  group_by(Cluster) %>%
  summarise(across(everything(), mean)) %>%
  pivot_longer(-Cluster, names_to = "Variable", values_to = "Mean")

png("~/Downloads/segment_barplot.png", width = 900, height = 550)
ggplot(seg_long, aes(x = Variable, y = Mean, fill = Cluster)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Attitude Scores by Segment", x = "Variable", y = "Mean Score (1-7)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")
dev.off()

png("~/Downloads/income_boxplot.png", width = 800, height = 500)
ggplot(data, aes(x = Cluster, y = LevIncome, fill = Cluster)) +
  geom_boxplot() +
  labs(title = "Income Level Distribution by Segment", x = "Segment", y = "Income Level (1-5)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")
dev.off()

write.csv(segment_means, "~/Downloads/segment_means.csv", row.names = FALSE)
write.csv(segment_medians, "~/Downloads/segment_medians.csv", row.names = FALSE)

cat("Complete! All files saved to Downloads folder.\n")
