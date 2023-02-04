install.packages("ggplot2")

df <- data.frame( 'A' = rnorm(100 , mean = 10 , sd = 2), 
                  'B' = rnorm(100 , mean = 34 , sd = 10) , 
                  'C' = rnorm(100 , mean = 45 , sd = 15))
#Normal daðýlým tablosu oluþturuldu.
df
View(df)  #tablo halinde görünüm 



df$A

df$A < 8
which(df$A < 8)


idx <- which(df$A < 8)

# A'nýn 8 den küçük olan alt kümesi
df[idx,'A']

df[idx, c('B' , 'C')]

df2 <- df[idx, c('B' , 'C')]

df2
class(df2)

df

mean(df$B)

df$B < mean(df$B)
idx2 <- which(df$B < mean(df$B))

df[idx2 ,]
df[idx2 , c('A' , 'C')]


View(df[idx2 , c('A' , 'C')])

unique(df)  #Tekrarlayan veri varsa silecektir
df_spB <- sd(df$B) 
df_sp
df_spC <- sd(df$C)
df_spC
#Data Frame oluþtururken parametre olarak girdiðimiz standart sapmalar test edildi.

df_varA <- var(df$A)
df_sdA <- sd(df$A)
sqrt(df_varA)==df_sdA
#Eþitlikler test edildi
hist(
  df$C,
  main = "Histogram Grafiði",
  xlab = "Deðiþken Deðerleri",
  ylab = "Frekans Deðerleri",
  breaks = 30,
  xlim = c(0,100),
  ylim = c(0,30)
)
#Normallik için histograma bakýldý, normal daðýlýma uymakta lakin aykýrý deðerler mevcut.

#Regresyon Analizi


#C baðýmlý, B baðýmsýz olarak düþünürsek
r_model <- lm(C ~ B, data = df)

# Betimsel Ýstatistik 
summary(r_model)

#Grafik için
library(ggplot2)
ggplot(df, aes(x = B, y = C)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(x = "x", y = "y", title = "Regresyon Analizi") +
  theme_classic()




