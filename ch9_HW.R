# 第1題 利用iris資料的前二個個變數(Sepal.Length, Sepal.Width)
# 畫出直方圖，在同一張A4。(寬:2:1,長:2:1)
nf <- layout(matrix(c(1,0,0,2), 2, 2, byrow = TRUE), 
             widths = c(2,1), heights = c(2, 1),
             respect = TRUE)
layout.show(nf)

data(iris)
head(iris) # first two var: Sepal.Length, Sepal.Width
hist(iris$Sepal.Length)
hist(iris$Sepal.Width)

# 第2題 加分題
nf <- layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE), 
             widths = c(1,1), heights = c(1, 1),
             respect = TRUE)
layout.show(nf)
df_color=iris$Species
df_color=as.data.frame(df_color)
df_color$code=4*(df_color[,1] == "setosa")+
  5*(df_color[,1] == "versicolor")+
  6*(df_color[,1] == "virginica")
df_color$word [df_color$df_color=="setosa"] <- "black"
df_color$word [df_color$df_color=="versicolor"] <- "red"
df_color$word [df_color$df_color=="virginica"] <- "green"

plot(iris$Sepal.Length,
     iris$Sepal.Width, 
     main="Scatter Plot",
     xlab = "Sepal.Length",
     ylab = "Sepal.Width",
     col = df_color$word,
     pch = 3)
hist(iris$Petal.Width,
     main="Histogram",
     xlab = "Petal.Width",
     col = "yellow")
boxplot(iris$Petal.Length~iris$Species,
        main="Box Plot",
        xlab = "Species",
        ylab = "Petal.Length",
        col=c(4,5,6))
count_species = table(iris$Species)
pie(count_species,
    main="Pie Chart")
