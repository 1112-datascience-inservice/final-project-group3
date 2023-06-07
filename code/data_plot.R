# 檢查libaray，若沒有則下載並載入
if (require(dplyr)) {
  message("library dplyr load success.")
} else {
  stop("library dplyr not exist, please install.")
}

if (require(ggplot2)) {
  message("library ggplot2 load success.")
} else {
  stop("library ggplot2 not exist, please install.")
}

# if (require(ggplot2)) {
#   print("library ggplot2 load success.")
# } else {
#   print("library ggplot2 not exist, start to install.")
#   install.packages('ggplot2', repos = "http://cran.us.r-project.org")
#   if (require(ggplot2)) {
#     print('library ggplot2 load success.')
#   } else {
#     stop("library ggplot2 load failed.")
#   }
# }

pdf("../results/data_plot.pdf")

# Heart Disease Percentage
f_in <- read.csv("../data/heart_2020_cleaned.csv")
count <- table(f_in[,1])
proportion <- prop.table(count)
barplot(proportion, 
        main = "Heart Disease", 
        xlab = "Yes or No", 
        ylab = "Persentage (%)",)
# 設定直條的顏色
bar_colors <- c("#4169E1", "red")

# 繪製直條圖，並使用新的顏色
text(x = barplot(proportion, 
        main = "Heart Disease", 
        ylim = c(0,1.2),
        las = 1,
        cex.axis = 0.8, cex.names = 1,
        xlab = "Diagnosis", 
        ylab = "Percentage (%)",
        col = bar_colors),
     y = proportion,
     labels = paste0(round(proportion * 100, 2), "%"),
     pos = 3)



#統計吸菸者比例
Smoking_per <- f_in %>%
  group_by(Smoking) %>%
  summarize(proportion = sum(HeartDisease == "Yes") / n())

# 输出結果
print(Smoking_per)

# 創建比例數據
smoking <- c("No", "Yes")
proportion <- c(0.0603, 0.122)
colors <- c("#4169E1", "red")

bottom_percent <- 1 - proportion

# 直方圖
text(x = barplot(rbind(bottom_percent, proportion),
                 beside = FALSE,
                 legend.text = FALSE,
                 las = 1,
                 density = NULL,
                 ylim = c(0,1.2),
                 cex.axis = 0.8, cex.names = 1,
                 names.arg = smoking,
                 main = "Heart Disease (%) by Smoking",
                 xlab = "Class",
                 ylab = "Percentage(%)",
                 col = colors),
     y = 0.07,
     labels = paste0(round(proportion * 100, 2), "%"),
     pos = 3,
     offset = 12.5)

#統計飲酒者比例
AlcoholDrinking_per <- f_in %>%
  group_by(AlcoholDrinking) %>%
  summarize(proportion = sum(HeartDisease == "Yes") / n())

# 輸出結果
print(AlcoholDrinking_per)

# 創建比例數據
AlcoholDrinking <- c("No", "Yes")
proportion <- c(0.0880, 0.0524)
colors <- c("#4169E1", "red")

# 計算底部的百分比
bottom_percent <- 1 - proportion

# 直方圖
text(x = barplot(rbind(bottom_percent, proportion),
                 beside = FALSE,
                 legend.text = FALSE,
                 density = NULL,
                 las = 1,
                 ylim = c(0,1.2),
                 cex.axis = 0.8, cex.names = 1,
                 names.arg = AlcoholDrinking,
                 main = "Heart Disease (%) by Alcohol Drinking",
                 cex.main = 1,
                 xlab = "Class",
                 ylab = "Percentage(%)",
                 col = colors),
     y = 0.07,
     labels = paste0(round(proportion * 100, 2), "%"),
     pos = 3,
     offset = 12.5)

#統計中風者比例
Stroke_per <- f_in %>%
  group_by(Stroke) %>%
  summarize(proportion = sum(HeartDisease == "Yes") / n())

# 輸出結果
print(Stroke_per)

# 創建比例數據
Stroke <- c("No", "Yes")
proportion <- c(0.0747, 0.364)
colors <- c("#4169E1", "red")

# 計算底部的百分比
bottom_percent <- 1 - proportion

# 雙色直方圖
text(x = barplot(rbind(bottom_percent, proportion),
                 beside = FALSE,
                 legend.text = FALSE,
                 las = 1,
                 density = NULL,
                 ylim = c(0,1.2),
                 cex.axis = 0.8, cex.names = 1,
                 names.arg = Stroke,
                 main = "Heart Disease (%) by Stroke",
                 #cex.main = 1,
                 xlab = "Class",
                 ylab = "Percentage(%)",
                 col = colors),
     y = 0.35,
     labels = paste0(round(proportion * 100, 2), "%"),
     pos = 3,
     offset = 9)

#統計不良於行比例
DiffWalking_per <- f_in %>%
  group_by(DiffWalking) %>%
  summarize(proportion = sum(HeartDisease == "Yes") / n())

# 輸出結果
print(DiffWalking_per)

# 創建比例數據
DiffWalking <- c("No", "Yes")
proportion <- c(0.063, 0.226)
colors <- c("#4169E1", "red")

# 計算底部的百分比
bottom_percent <- 1 - proportion

# 直方圖
text(x = barplot(rbind(bottom_percent, proportion),
                 beside = FALSE,
                 legend.text = FALSE,
                 density = NULL,
                 ylim = c(0,1.2),
                 cex.axis = 0.8, cex.names = 1,
                 las = 1,
                 names.arg = DiffWalking,
                 main = "Heart Disease (%) by DiffWalking",
                 #cex.main = 1,
                 xlab = "Class",
                 ylab = "Percentage(%)",
                 col = colors),
     y = 0.34,
     labels = paste0(round(proportion * 100, 2), "%"),
     pos = 3,
     offset = 9)

#統計男女心臟病比例
F_M_per <- f_in %>%
  group_by(Sex) %>%
  summarize(proportion = sum(HeartDisease == "Yes") / n())

# 輸出結果
print(F_M_per)

# 創建比例數據
sex <- c("Female", "Male")
proportion <- c(0.0669, 0.106)
colors <- c("#4169E1", "red")

# 計算底部的百分比
bottom_percent <- 1 - proportion

# 直方圖
text(x = barplot(rbind(bottom_percent, proportion),
                 beside = FALSE,
                 legend.text = FALSE,
                 density = NULL,
                 ylim = c(0,1.2),
                 las = 1,
                 cex.axis = 0.8, cex.names = 1,
                 names.arg = sex,
                 main = "Heart Disease (%) by Gender",
                 xlab = "Class",
                 ylab = "Percentage(%)",
                 col = colors),
     y = 0.18,
     labels = paste0(round(proportion * 100, 2), "%"),
     pos = 3,
     offset = 11)

#統計年齡範圍比例
AgeCategory_per <- f_in %>%
  group_by(AgeCategory) %>%
  summarize(proportion = sum(HeartDisease == "Yes") / n())

# 輸出結果
print(AgeCategory_per)

# 創建比例數據
AgeCategory <- c("28-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80 or older")
proportion <- c(0.00617, 0.00784, 0.0121, 0.0144, 0.0231, 0.0341, 0.0545, 0.074, 0.0988, 0.12, 0.156, 0.188, 0.226)
colors <- c("#4169E1", "red")

# 計算底部的百分比
bottom_percent <- 1 - proportion

# 設置邊緣空白，增加y軸標籤和y軸名稱之間的間距
par(mar = c(5, 5, 3, 2) + 0.1)

# 直方圖
barplot(rbind(bottom_percent, proportion),
        horiz = TRUE,
        beside = FALSE,
        legend.text = FALSE,
        density = NULL,
        xlim = c(0,1.2),
        cex.axis = 0.8, cex.names = 0.8,
        names.arg = AgeCategory,
        main = "Heart Disease (%) by AgeCategory",
        #cex.main = 1,
        xlab = "Percentage(%)",
        ylab = "Age",
        axis.lty = 0,
        las = 1,
        offset = 0,
        col = colors)

text(x = 1.12,
     y = rep(-0.7:13*1.2, length.out = 13),
     labels = paste0(round(proportion * 100, 2), "%"),
     pos = 3,
     offset = 1.3)


#統計種族範圍比例
Race_per <- f_in %>%
  group_by(Race) %>%
  summarize(proportion = sum(HeartDisease == "Yes") / n())

# 輸出結果
print(Race_per)

# 創建比例數據
Race <- c("Asian", "Hispanic", "Black", "Other", "White", "American Indian\nAlaskan Native")
proportion <- c(0.033, 0.0526, 0.0754, 0.0811, 0.0918, 0.104)
colors <- c("#4169E1", "red")

# 設置邊緣空白，增加y軸標籤和y軸名稱之間的間距
par(mar = c(5, 6, 3, 2) + 0.1)

# 計算底部的百分比
bottom_percent <- 1 - proportion

# 直方圖
barplot(rbind(bottom_percent, proportion),
        horiz = TRUE,
        beside = FALSE,
        legend.text = FALSE,
        density = NULL,
        xlim = c(0,1.2),
        cex.axis = 0.8, cex.names = 0.8,
        names.arg = Race,
        main = "Heart Disease (%) by Race",
        #cex.main = 1,
        xlab = "Percentage(%)",
        #ylab = "Class",
        axis.lty = 0,
        las = 1,
        offset = 0,
        col = colors)

text(x = 1.12,
     y = rep(0.08:6*1.2, length.out = 6),
     labels = paste0(round(proportion * 100, 2), "%"),
     pos = 3,
     offset = 1.3)

#統計糖尿病患者比例
Diabetic_per <- f_in %>%
  group_by(Diabetic) %>%
  summarize(proportion = sum(HeartDisease == "Yes") / n())

# 輸出結果
print(Diabetic_per)

# 創建比例數據
Diabetic <- c("Yes \n(pregnancy)", "No", "No, borderline", "Yes")
proportion <- c(0.0422, 0.065, 0.116, 0.22)
colors <- c("#4169E1", "red")

# 計算底部的百分比
bottom_percent <- 1 - proportion

# 設置邊緣空白，增加y軸標籤和y軸名稱之間的間距
par(mar = c(5, 5, 3, 2) + 0.1)

# 直方圖
barplot(rbind(bottom_percent, proportion),
        horiz = TRUE,
        beside = FALSE,
        legend.text = FALSE,
        density = NULL,
        xlim = c(0,1.2),
        cex.axis = 0.8, cex.names = 0.8,
        names.arg = Diabetic,
        main = "Heart Disease (%) by Diabetic",
        #cex.main = 1,
        xlab = "Percentage(%)",
        #ylab = "Class",
        axis.lty = 0,
        las = 1,
        offset = 0,
        col = colors)

text(x = 1.12,
          y = rep(0.2:4*1.2, length.out = 4),
          labels = paste0(round(proportion * 100, 2), "%"),
          pos = 3,
          offset = 1.3)     

#統計活動正常者比例
PhysicalActivity_per <- f_in %>%
  group_by(PhysicalActivity) %>%
  summarize(proportion = sum(HeartDisease == "Yes") / n())

# 輸出結果
print(PhysicalActivity_per)

# 創建比例數據
PhysicalActivity <- c("No", "Yes")
proportion <- c(0.138, 0.0705)
colors <- c("#4169E1", "red")

# 計算底部的百分比
bottom_percent <- 1 - proportion

# 直方圖
text(x = barplot(rbind(bottom_percent, proportion),
                 beside = FALSE,
                 legend.text = FALSE,
                 density = NULL,
                 ylim = c(0,1.2),
                 cex.axis = 0.8, cex.names = 1,
                 las = 1,
                 names.arg = PhysicalActivity,
                 main = "Heart Disease (%) by PhysicalActivity",
                 #cex.main = 1,
                 xlab = "Class",
                 ylab = "Percentage(%)",
                 col = colors),
     y = 0.17,
     labels = paste0(round(proportion * 100, 2), "%"),
     pos = 3,
     offset = 11.5)

#統計健康程度比例
GenHealth_per <- f_in %>%
  group_by(GenHealth) %>%
  summarize(proportion = sum(HeartDisease == "Yes") / n())

# 輸出結果
print(GenHealth_per)

# 創建比例數據
GenHealth <- c("Excellent", "Very good", "Good", "Fair", "Poor")
proportion <- c(0.0224, 0.0473, 0.103, 0.204, 0.341)
colors <- c("#4169E1", "red")

# 計算底部的百分比
bottom_percent <- 1 - proportion

# 設置邊緣空白，增加y軸標籤和y軸名稱之間的間距
par(mar = c(5, 5, 3, 2) + 0.1)

# 直方圖
barplot(rbind(bottom_percent, proportion),
                 horiz = TRUE,
                 beside = FALSE,
                 legend.text = FALSE,
                 density = NULL,
                 xlim = c(0,1.2),
                 cex.axis = 0.8, cex.names = 1,
                 names.arg = GenHealth,
                 main = "Heart Disease (%) by GenHealth",
                 #cex.main = 1,
                 xlab = "Percentage(%)",
                 #ylab = "Class",
                 axis.lty = 0,
                 las = 1,
                 offset = 0,
                 col = colors)

text(x = 1.12,
     y = rep(0.15:5*1.2, length.out = 5),
     labels = paste0(round(proportion * 100, 2), "%"),
     pos = 3,
     offset = 1.3)


#統計哮喘患者比例
Asthma_per <- f_in %>%
  group_by(Asthma) %>%
  summarize(proportion = sum(HeartDisease == "Yes") / n())

# 輸出結果
print(Asthma_per)

# 創建比例數據
Asthma <- c("No", "Yes")
proportion <- c(0.081, 0.115)
colors <- c("#4169E1", "red")

# 計算底部的百分比
bottom_percent <- 1 - proportion

par(mar = c(5, 4, 3, 2) + 0.1)
# 直方圖
text(x = barplot(rbind(bottom_percent, proportion),
                 beside = FALSE,
                 legend.text = FALSE,
                 density = NULL,
                 ylim = c(0,1.2),
                 cex.axis = 0.8, cex.names = 1,
                 names.arg = Asthma,
                 main = "Heart Disease (%) by Asthma",
                 #cex.main = 1,
                 xlab = "Class",
                 las = 1,
                 ylab = "Percentage(%)",
                 col = colors),
     y = 0.2,
     labels = paste0(round(proportion * 100, 2), "%"),
     pos = 3,
     offset = 12)

#統計腎臟病患者比例
KidneyDisease_per <- f_in %>%
  group_by(KidneyDisease) %>%
  summarize(proportion = sum(HeartDisease == "Yes") / n())

# 輸出結果
print(KidneyDisease_per)

# 創建比例數據
KidneyDisease <- c("No", "Yes")
proportion <- c(0.0777, 0.293)
colors <- c("#4169E1", "red")

# 計算底部的百分比
bottom_percent <- 1 - proportion

# 直方圖
text(x = barplot(rbind(bottom_percent, proportion),
                 beside = FALSE,
                 legend.text = FALSE,
                 density = NULL,
                 ylim = c(0,1.2),
                 cex.axis = 0.8, cex.names = 1,
                 names.arg = KidneyDisease,
                 main = "Heart Disease (%) by KidneyDisease",
                 #cex.main = 1,
                 xlab = "Class",
                 ylab = "Percentage(%)",
                 las = 1,
                 col = colors),
     y = 0.4,
     labels = paste0(round(proportion * 100, 2), "%"),
     pos = 3,
     offset = 9)

#統計皮膚癌患者比例
SkinCancer_per <- f_in %>%
  group_by(SkinCancer) %>%
  summarize(proportion = sum(HeartDisease == "Yes") / n())

# 輸出結果
print(SkinCancer_per)

# 創建比例數據
SkinCancer <- c("No", "Yes")
proportion <- c(0.0772, 0.167)
colors <- c("#4169E1", "red")

# 計算底部的百分比
bottom_percent <- 1 - proportion

# 直方圖
text(x = barplot(rbind(bottom_percent, proportion),
                 beside = FALSE,
                 legend.text = FALSE,
                 density = NULL,
                 ylim = c(0,1.2),
                 cex.axis = 0.8, cex.names = 1,
                 names.arg = SkinCancer,
                 main = "Heart Disease (%) by SkinCancer",
                 #cex.main = 1,
                 xlab = "Class",
                 ylab = "Percentage(%)",
                 las = 1,
                 col = colors),
     y = 0.2,
     labels = paste0(round(proportion * 100, 2), "%"),
     pos = 3,
     offset = 12)


# BMI boxplot analysis
median <- f_in %>%
  group_by(HeartDisease) %>%
  summarize(median_BMI = median(BMI))

ggplot(data = f_in) +
  geom_boxplot(mapping = aes(x = BMI, y = HeartDisease, fill = HeartDisease), notch = TRUE, inherit.aes = TRUE) +
  scale_fill_manual(values = c("No" = "#4169E1", "Yes" = "red")) +
  geom_text(
    data = median,
    aes(x = median_BMI, y = HeartDisease, label = median_BMI), 
    vjust = -0.4
    ) +
  coord_flip() +
  theme(legend.position = "none")

# PhysicalHealth boxplot analysis
median2 <- f_in %>%
  group_by(HeartDisease) %>%
  summarize(median_PhysicalHealth = median(PhysicalHealth))

ggplot(data = f_in) +
  geom_boxplot(mapping = aes(x = PhysicalHealth, y = HeartDisease, fill = HeartDisease), notch = TRUE, inherit.aes = TRUE) +
  scale_fill_manual(values = c("No" = "#4169E1", "Yes" = "red")) +
  geom_text(
    data = median2,
    aes(x = median_PhysicalHealth, y = HeartDisease, label = median_PhysicalHealth), 
    vjust = -0.4
  ) +
  coord_flip() +
  theme(legend.position = "none")

# MentalHealth boxplot analysis
median3 <- f_in %>%
  group_by(HeartDisease) %>%
  summarize(median_MentalHealth = median(MentalHealth))

ggplot(data = f_in) +
  geom_boxplot(mapping = aes(x = MentalHealth, y = HeartDisease, fill = HeartDisease), notch = TRUE, inherit.aes = TRUE) +
  scale_fill_manual(values = c("No" = "#4169E1", "Yes" = "red")) +
  geom_text(
    data = median3,
    aes(x = median_MentalHealth, y = HeartDisease, label = median_MentalHealth), 
    vjust = -0.4
  ) +
  coord_flip() +
  theme(legend.position = "none")

# SleepTime boxplot analysis
median4 <- f_in %>%
  group_by(HeartDisease) %>%
  summarize(median_SleepTime = median(SleepTime))

ggplot(data = f_in) +
  geom_boxplot(mapping = aes(x = SleepTime, y = HeartDisease, fill = HeartDisease), notch = TRUE, inherit.aes = TRUE) +
  scale_fill_manual(values = c("No" = "#4169E1", "Yes" = "red")) +
  geom_text(
    data = median4,
    aes(x = median_SleepTime, y = HeartDisease, label = median_SleepTime), 
    vjust = -0.4
  ) +
  coord_flip() +
  theme(legend.position = "none")

#Correlation Analysis
# f_ca <- read.csv("correlation.csv")
# f_ca$Diabetic <- as.numeric(f_ca$Diabetic)
# sapply(f_ca, class)
# library(corrplot)
# cor_matrix <- cor(f_ca[,-1], use = "pairwise.complete.obs")
# print(cor_matrix)
# corrplot(cor_matrix, method = "color",   tl.col = "black")
