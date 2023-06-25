#回帰解剖定理（regression anatomy theorem）について実行・可視化

library(tidyverse)
library(haven) #haven: Import and Export 'SPSS', 'Stata' and 'SAS' Files

read_data <- function(df){
  #paste0()：複数の要素を連結して1つの文字列を作成するために使用される関数
  full_path <- paste0("https://github.com/scunning1975/mixtape/raw/master/",df)
  haven::read_dta(full_path) #stataのファイルを読み込む
}

auto <-
  read_data("auto.dta") %>%
  mutate(length = length - mean(length))

lm1 <- auto %>% lm(price ~ length, .) %>% print()
lm2 <- auto %>% lm(price ~ length + weight + headroom + mpg, .) %>% print()
lm_aux <- auto %>% lm(length ~ weight + headroom + mpg, .) %>% print()


auto <- auto %>% 
  mutate(length_resid = residuals(lm_aux))

#残差から回帰する
lm2_alt <- auto %>% lm(price ~ length_resid, .) %>% print()

coef_lm1 <- coef(lm1)
coef_lm2_alt <- coef(lm2_alt)
resid_lm2 <- residuals(lm2)

#回帰解剖からの，残差との単回帰モデル
y_single <- tibble(price = coef_lm2_alt[1]+coef_lm1[2]*auto$length_resid,
                   length_resid = auto$length_resid) %>% print()
#重回帰モデル
y_multi <- tibble(price = coef_lm2_alt[1]+coef_lm2_alt[2]*auto$length_resid,
                  length_resid = auto$length_resid) %>% print()

auto %>% ggplot(aes(x=length_resid, y=price))+
  geom_point()+
  geom_smooth(data = y_multi, color="blue")+
  geom_smooth(data = y_single, color="red")
summary(lm1)
