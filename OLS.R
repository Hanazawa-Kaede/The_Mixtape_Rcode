#OLSの実行・可視化
library(tidyverse)

set.seed(2023)
n <- 10000
slope <- 5.5 
tb <- tibble(
  x = rnorm(n),
  u = rnorm(n),
  y = slope*x+12*u
)

reg_tb <- tb %>% lm(y ~ x, .) %>% print()
coef(reg_tb)
beta0_hat <- coef(reg_tb)[1] %>% print()
beta1_hat <- coef(reg_tb)[2] %>% print()


#tbに変数を追加する
tb <- tb %>%
  mutate(
    yhat1 = predict(reg_tb),
    yhat2 = beta0_hat + beta1_hat*x,
    uhat1 = residuals(reg_tb),
    uhat2 = y - yhat2
  )

#yhat1, yhat2, uhat1, uhat2の新しく作った4つのsummaryを取り出す．
summary(tb[-1:-3])

reg_tb %>% ggplot(aes(x=x,y=y)) +
  ggtitle("OLS Regression Line") +
  geom_point(size=0.05,color = "black",alpha=1)+
  geom_smooth(method = "lm",color="black")+
  #注釈を加える
  annotate("text",x=-1.5,y=30,color="red",label=paste("Intercept = ",beta0_hat)) +
  annotate("text",x=1.5,y=-30,color="blue",label=paste("Slope = ",beta1_hat))
