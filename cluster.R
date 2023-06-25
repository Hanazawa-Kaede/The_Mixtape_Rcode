install.packages("pacman")
pacman::p_load(arm, mvtnorm, lme4,multiwayvcov, clusterSEs, ggplot2, dplyr, haven)

#クラスタ頑健標準誤差について
#データがクラスタ化されている場合のOLS推定値を求める
# y_i ~ N(\beta_0+\beta_1x_i, \rho^2) の線形回帰モデルを考えている．
#\rho = 1のとき，クラスタ内のすべてのunitは同一とみなされ，有効標本サイズはクラスタの数に削減される．
#\rho = 0のとき，クラスタ内のunitの相関はなく，すべての観測値は互いに独立であるとみなされる

#クラスタ化されたデータを生成する関数
gen_cluster <- function(param = c(.1,.5), n = 1000, n_cluster = 50, rho = 0.5){
  
  #個人レベルの誤差共分散行列とサンプリング
  
  #2x2の行列を作成 共分散は0
  Sigma_i <- matrix(c(1,0,0,1-rho), ncol=2)
  
  #多変量正規分布からランダムな値を生成
  values_i <- rmvnorm(n = n, sigma = Sigma_i)
  
  #クラスタレベル誤差共分散行列とサンプリング
  cluster_name <- rep(1:n_cluster, each = n/n_cluster)
  Sigma_cl <- matrix(c(1, 0, 0, rho), ncol = 2)
  values_cl <- rmvnorm(n = n_cluster, sigma = Sigma_cl)
  
  #個人レベル，クラスタレベルの説明変数
  #values_i[, 1] の各要素に，values_cl[, 1] というベクトルの要素を n/n_cluster 回繰り返し加える．
  x <- values_i[ , 1] + rep(values_cl[ , 1], each = n / n_cluster)
  
  #個人レベル，クラスタレベルの誤差
  error <- values_i[ , 2] + rep(values_cl[ , 2], each = n / n_cluster)
  
  #データ生成プロセス
  y <- param[1] + param[2]*x + error
  
  df <- data.frame(x, y, cluster = cluster_name)
  
  return(df)
}

#クラスタ化されているデータにおけるシミュレーション
#cluster_robust = TRUE　で，クラスタ頑健標準誤差を計算
cluster_sim <- function(param = c(.1,.5), n=1000, n_cluster = 50, rho = .5, cluster_robust = FALSE){
  df <- gen_cluster(param = param, n = n, n_cluster = n_cluster, rho = rho)
  fit <- df %>% lm(y ~ x, .)
  b1 <- coef(fit)[2]
  #クラスタ頑健，ではないなら
  if(!cluster_robust){
    Sigma <- vcov(fit) #分散共分散行列を計算
    se <- sqrt(diag(Sigma)[2]) #分散共分散行列から，xの分散のsqrt,つまり標準誤差を抽出
    #回帰モデルの値について95%信頼区間の計算後，2行目（xの行）について取り出す．
    b1_ci95 <- confint(fit)[2,]
  }else{ #クラスタ頑健標準誤差の計算
    Sigma <- cluster.vcov(fit, ~cluster)
    se <- sqrt(diag(Sigma)[2])
    #t分布の上側パーセンタイルを計算，自由度はサンプルサイズ-2する．
    t_critical <- qt(.025, df = n-2, lower.tail = FALSE)
    lower <- b1 - t_critical*se
    upper <- b1 + t_critical*se
    b1_ci95 <- c(lower, upper)
  }
  return(c(b1, se, b1_ci95))
}

#シミュレーションを繰り返す関数
run_cluster_sim <- function(n_sims = 1000, param = c(.1, .5), n = 1000, n_cluster = 50,
                            rho = .5, cluster_robust = FALSE){
  df <- replicate(n_sims, cluster_sim(param = param, n = n, rho = rho,
                                      n_cluster = n_cluster, cluster_robust = cluster_robust))
  #dfを転置する
  df <- as.data.frame(t(df))
  #列名を指定
  names(df) <- c("b1", "se_b1", "ci95_lower", "ci95_upper")
  df <- df %>% mutate(
    id = 1:n(),
    param_caught = ci95_lower <= param[2] & ci95_upper >= param[2]
  )
  return(df)
}

#\beta_0 = 0.4, \beta_1 = 0　とし，\rho = 0でクラスターでない場合をsimulationする
#推定量の分布と信頼区間
sim_params <- c(.4, 0)
sim_nocluster <- run_cluster_sim(n_sims = 10000, param = sim_params, rho = 0)
hist_nocluster <- ggplot(sim_nocluster, aes(b1)) +
  geom_histogram(color = "black") +
  geom_vline(xintercept = sim_params[2], color = "red") 

print(hist_nocluster)

ci95_nocluster <- ggplot(sample_n(sim_nocluster, 100), #データフレームから指定された数のランダムな行を抽出する
                         aes(x = reorder(id, b1), y = b1, ymin = ci95_lower, ymax = ci95_upper,
                             color = param_caught))+
  geom_hline(yintercept = sim_params[2], linetype = "dashed") +
  geom_pointrange()+
  labs(x = "sim ID", y = "b1", title = "Randomly Chosen 100 95% CIs") + 
  scale_color_discrete(name = "True param value", labels = c("missed", "hit"))+
  coord_flip() #グラフの座標軸を縦軸と横軸を入れ替える

print(ci95_nocluster)
#第一種の過誤
sim_nocluster %>% summarize(type1_error = 1 - sum(param_caught)/n())


#次に，クラスタ化されたデータでOLS推定をすることの問題点を確かめる．
#クラスタ化（rhoが0でない）されているために，観測値が互いに独立ではない．
sim_cluster_ols <- run_cluster_sim(n_sims = 10000, param= sim_params)

#二つのオブジェクトを結合
hist_cluster_ols <- hist_nocluster %+% sim_cluster_ols
print(hist_cluster_ols)  

#信頼区間：データがクラスタ化されている場合，帰無仮説を棄却する推定値の数が多くなる．
ci95_cluster_ols <- ci95_nocluster %+% sample_n(sim_cluster_ols, 100) %>% print()
#第一種の過誤の確率が，0.4176と非常に高くなってしまっている
sim_cluster_ols %>% summarize(type1_error = 1 - sum(param_caught)/n())
  
#クラスタに頑健な標準誤差を用いることで解決できる
#ここでは，cluster_robust = TRUE とする．
sim_cluster_robust <- run_cluster_sim(n_sims = 10000, param = sim_params, cluster_robust = TRUE)
hist_cluster_robust <- hist_nocluster %+% sim_cluster_ols
print(hist_cluster_robust)

ci95_cluster_robust <- ci95_nocluster %+% sample_n(sim_cluster_robust, 100)
print(ci95_cluster_robust)  

sim_cluster_robust %>% summarize(type1_error = 1 - sum(param_caught)/n())

















