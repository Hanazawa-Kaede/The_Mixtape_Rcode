install.packages("pacman")
pacman::p_load(arm, mvtnorm, lme4,multiwayvcov, clusterSEs, ggplot2, dplyr, haven)

#クラスタ頑健標準誤差について
#データがクラスタ化されている場合のOLS推定値を求める
# y_i ~ N(\beta_0+\beta_1x_i, \rho^2) の線形回帰モデルを考えている．
#\rho = 1のとき，クラスタ内のすべてのunitは同一とみなされ，有効標本サイズはクラスタの数に削減される．
#\rho = 0のとき，クラスタ内のunitの相関はなく，すべての観測値は互いに独立であるとみなされる

#クラスタ化されたデータを生成する関数
geom_cluster <- function(param = c(.1,.5), n = 1000, n_cluster = 50, rho = 0.5){
  #個人レベルの誤差共分散行列とサンプリング
  #2x2の行列を作成 共分散は0
  Sigma_i <- matrix(c(1,0,0,1-rho), ncol=2)
  values_i <- 
}