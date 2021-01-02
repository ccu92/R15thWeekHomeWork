#作業p13大題   （第p24大題從135行開始）
cvd_all = read.csv("cvd_all.csv", fileEncoding = "big5")
set.seed(22)
# 隨機抽樣一千筆
cvd_tmp = cvd_all[sample(1:nrow(cvd_all), 1000, replace = FALSE), ]
# 只留下需要的直行
cvd_r_1000 = cbind.data.frame(cvd = cvd_tmp$心血管疾病,
                              waist = cvd_tmp$腰圍,
                              gender = cvd_tmp$性別,
                              age = cvd_tmp$年齡,
                              sbp = cvd_tmp$收縮壓,
                              dbp = cvd_tmp$舒張壓,
                              抽菸量 = cvd_tmp$抽菸量)
# 腰圍 waist --- 平均數 中位數 標準差 IQR
a=mean(cvd_r_1000$waist, na.rm = T) # waist mean
b=median(cvd_r_1000$waist, na.rm = T) # waist median
c=sd(cvd_r_1000$waist, na.rm = T) # waist sd
q = quantile(cvd_r_1000$waist, na.rm = T) 
d=(iqr = q[[4]] - q[[2]]) # waist IQR from it's Q3 - Q1
#tmp1 = data.frame("waist", a, b, c, d)
tmp1 = data.frame( item = "waist", mean = a,median = b,sd = c,"IQR" = d)


# 年齡 age --- 平均數 中位數 標準差 IQR
a=mean(cvd_r_1000$age, na.rm = T) # age mean
b=median(cvd_r_1000$age, na.rm = T) # age median
c=sd(cvd_r_1000$age, na.rm = T) # age sd
q = quantile(cvd_r_1000$age, na.rm = T) 
d=(iqr = q[[4]] - q[[2]]) # age IQR from it's Q3 - Q1
#tmp2 = data.frame("age", a, b, c, d)
tmp2 = data.frame( item = "age", mean = a,median = b,sd = c,"IQR" = d)


# 收縮壓 sbp --- 平均數 中位數 標準差 IQR
a=mean(cvd_r_1000$sbp, na.rm = T) # sbp mean
b=median(cvd_r_1000$sbp, na.rm = T) # sbp median
c=sd(cvd_r_1000$sbp, na.rm = T) # sbp sd
q = quantile(cvd_r_1000$sbp, na.rm = T) 
d=(iqr = q[[4]] - q[[2]]) # sbp IQR from it's Q3 - Q1
#tmp3 = data.frame("sbp", a, b, c, d)
tmp3 = data.frame( item = "sbp", mean = a,median = b,sd = c,"IQR" = d)


# 舒張壓 dbp --- 平均數 中位數 標準差 IQR
a=mean(cvd_r_1000$dbp, na.rm = T) # dbp mean
b=median(cvd_r_1000$dbp, na.rm = T) # dbp median
c=sd(cvd_r_1000$dbp, na.rm = T) # dbp sd
q = quantile(cvd_r_1000$dbp, na.rm = T) 
d=(iqr = q[[4]] - q[[2]]) # dbp IQR from it's Q3 - Q1
#tmp4  = data.frame("dbp", a, b, c, d)
tmp4 = data.frame( item = "dbp", mean = a,median = b,sd = c,"IQR" = d)

# 第1.1題 描述性統計
des=rbind(tmp1, tmp2, tmp3, tmp4)
des

# 第1.2題 scatter plot散佈圖 SBP vs DBP
plot(cvd_r_1000$sbp,            # X軸的值
     cvd_r_1000$dbp,             # Y軸的值
     main="sbp vs dbp",   # 圖片名稱
     xlab="SBP",            # X軸名稱
     ylab="DBP") # Y軸名稱  


# 第1.3題 contingency table(cvd, gender) 列聯表
table(cvd_r_1000$cvd, cvd_r_1000$gender, 
      dnn=c("心血管疾病", "性別"))


#第2題 將age切三組 1~45 46~60 61~ , 命名age_1
age_1 = 1*(cvd_r_1000$age<45) + 
        2*(cvd_r_1000$age>=45&cvd_r_1000$age<60) +
        3*(cvd_r_1000$age>=60)
age_1

#age_1 = cut(cvd_r_1000$age, breaks = c(0, 45, 60, 
#                               max(cvd_r_1000$age)),
#                               labels = c(1, 2, 3))

# 第3題 比較不同性別下，腰圍平均數是否相同？
# 再比較中位數是否相同？
# PFM t test檢定平均數
t_test = t.test(waist~gender, data = cvd_r_1000, 
                alternative = "two.sided")
t_test# p-value < 2.2e-16，拒絕虛無假設兩者之間有顯著差異（平均數不相同）
# PFM wilcox檢定中位數
wilcox.test(waist~gender, data = cvd_r_1000)
# p-value < 2.2e-16，拒絕虛無假設兩者之間有顯著差異（中位數不相同）

# boxplot
boxplot(waist~gender, data = cvd_r_1000)
plot(waist~as.factor(gender), data = cvd_r_1000) # 只用plot就要轉factor

# 第4題 比較不同年齡(age_1)腰圍平均數是否相同？中位數是否也相同？
library(car) # ANOVA要用acr
cvd_r_1000$age_1 = age_1
head(cvd_r_1000)

aov_age4group = aov(cvd_r_1000$waist~cvd_r_1000$age_1)
summary(aov_age4group) 
# P<0.05 age_1各組彼此之間至少有一組有差異，要做事後檢定

# 比較中位數
kruskal.test(cvd_r_1000$waist~cvd_r_1000$age_1)
# P<0.05 有顯著 

# box plot
boxplot(cvd_r_1000$waist~cvd_r_1000$age_1)

# 事後檢定
TukeyHSD(aov_age4group)
# 年齡第2組的平均減去年齡第1組的平均差值為5.07
# P<0.05 兩組有差異（顯著）
# 年齡第3組的平均減去年齡第1組的平均差值為8.68
# P<0.05 兩組有差異（顯著）
# 年齡第3組的平均減去年齡第2組的平均差值為3.62
# P is 0.0006 < 0.05 兩組有差異（顯著）

# 第5題 檢定sbp & waist間的相關性（皮爾森斯&皮爾曼相關係數）
cor(cvd_r_1000$waist, cvd_r_1000$sbp, 
    use = "pairwise.complete.obs", # 兩兩皆有值 才計算 跳過NA
    method = "pearson") # 預設是皮爾森
cor.test(cvd_r_1000$waist, cvd_r_1000$sbp, 
    use = "pairwise.complete.obs", 
    method = "pearson")
# p-value < 2.2e-16 兩者之間相關係數不等於零 有相關性

cor(cvd_r_1000$waist, cvd_r_1000$sbp, 
    use = "pairwise.complete.obs", 
    method = "spearman")





# 作業p24大題
# cvd_all隨機抽1000筆 
head(cvd_r_1000)
#head(subset(cvd_r_1000, select = -c(抽菸量)))
# 設waist為依變項
# 第1題 抽菸量為解釋變數 建構迴歸模型 寫出迴歸模型
result = lm(cvd_r_1000$waist~as.factor(cvd_r_1000$抽菸量))
table(as.factor(cvd_r_1000$抽菸量))
contrasts(as.factor(cvd_r_1000$抽菸量)) # 看dummy var 
summary(result)
# 迴歸模型Y = 76.9395 + 4.7852 * X1抽菸量1 + 7.9923 * X2抽菸量2

# 第2題
# 抽菸量為1時，waist平均增加4.7852，抽菸量為2時，waist平均增加7.9923

# 第3題 用ANOVA方法
aov_smo3group = aov(cvd_r_1000$waist~as.factor(cvd_r_1000$抽菸量))
summary(aov_smo3group) 
# P小於0.05 各抽菸量之間有相關 需做事後檢定

# 第4題 做事後檢定
TukeyHSD(aov_smo3group)
# 抽菸量1與0之間p值小於0.05 彼此有相關
# 抽菸量2與0之間p值小於0.05 彼此有相關
# 抽菸量2與1之間p值大於0.05 沒有更多證據顯示彼此有相關
     