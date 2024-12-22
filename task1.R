df = read.csv("D:/masters/statics/final/data.csv")
sum(complete.cases(df))
sum(colSums(df))

# Count the number of non-NA values for each column (x1, x2, ..., x5)
non_na_counts <- colSums(!is.na(df[c("x1", "x2", "x3", "x4", "x5")]))
print(non_na_counts)
head(df)
summary(df)
colnames(df)

library(ggplot2)
library(reshape2)

#plot of x1
ggplot(df, aes(x = seq_along(x1), y = x1)) +
  geom_line(color = "blue", size = 1) +
  ggtitle("Time Series: x1") +
  xlab("Index") +
  ylab("x1") +
  theme_minimal()

#plot of x2
ggplot(df, aes(x = seq_along(x2), y = x2)) +
  geom_line(color = "green", size =1) +
  ggtitle("Time Series: x2")+
  xlab("Index")+
  ylab("x2")+
  theme_minimal()

#plot of x3
ggplot(df, aes(x = seq_along(x3), y=x3)) +
  geom_line(color="red", size=1)+
  ggtitle("Time Series: x3")+
  xlab("Index")+
  ylab("x3")+
  theme_minimal()

#plot of x4
ggplot(df, aes(x = seq_along(x4), y=x4)) +
  geom_line(color="black", size=1)+
  ggtitle("Time Series: x4")+
  xlab("Index")+
  ylab("x4")+
  theme_minimal()

#plot of x5
ggplot(df, aes(x = seq_along(x5), y=x5)) +
  geom_line(color="brown", size=1)+
  ggtitle("Time Series: x5")+
  xlab("Index")+
  ylab("x5")+
  theme_minimal()

#all together plotted in one frame
library(tidyverse)

df_long <- df %>%
  Index = 1:6
ggplot(df, aes(x=Index))+
  geom_line(aes(y=x1, color="x1"), size = 1)+
  geom_line(aes(y=x2, color="x2"), size=1)+
  geom_line(aes(y = x3, color = "x3"), size = 1) +
  geom_line(aes(y = x4, color = "x4"), size = 1) +
  geom_line(aes(y = x5, color = "x5"), size = 1) +
  ggtitle("Time series plot of : x1, x2, x3, x4, x5")+
  xlab("Index")+
  ylab("Values")+
  scale_color_manual(
    name = "Variables", 
    values = c("x1" = "red", "x2" = "blue", "x3" = "green", "x4" = "orange", "x5" = "purple")
  ) +
  theme_minimal()

library(tidyverse)

# Sample data (assuming this structure for the dataframe df)
df <- data.frame(
  Index = 1:200,
  x1 = rnorm(200),
  x2 = rnorm(200),
  x3 = rnorm(200),
  x4 = rnorm(200),
  x5 = rnorm(200)
)

# Reshape data to long format using pivot_longer
df_long <- df %>%
  pivot_longer(cols = starts_with("x"), 
               names_to = "Variable", 
               values_to = "Value")

# Now create the plot
ggplot(df_long, aes(x = Index, y = Value, color = Variable)) +
  geom_line(size = 0.5) +
  ggtitle("Time series plot of : x1, x2, x3, x4, x5") +
  xlab("Index") +
  ylab("Values") +
  scale_color_manual(
    name = "Variables", 
    values = c("x1" = "red", "x2" = "blue", "x3" = "green", "x4" = "orange", "x5" = "purple")
  ) +
  theme_minimal()