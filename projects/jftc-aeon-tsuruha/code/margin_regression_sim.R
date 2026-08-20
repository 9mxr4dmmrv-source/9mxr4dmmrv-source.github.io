# ============================================================
#  R再現②: 粗利益率回帰の再現実験(店舗固定効果パネル回帰)
#  公取委「イオン×ツルハ 経済分析報告書」のモデルを合成データで再現。
#   margin_it = α_i + β・競合グループ数_it + 隣接業態 + 需要シフター
#               + 年効果 + 月効果 + ε_it     (真のβ = -0.16)
#  論点: (1)固定効果でβを回収できるか (2)競合数の「内生性」で推定がどう歪むか
#        (3)観測できる需要シフターを入れても、未観測の地域要因が残ると
#           依然バイアスが残る = 報告書が警告した内生性の本質。
#  ※base Rのみ・追加パッケージ不要。固定効果は within 変換で実装。
# ============================================================
set.seed(2026)
N <- 250; years <- 5; T <- years*12            # 250店 × 60期
beta_true <- -0.16
th_sm <- -0.03; th_hc <- -0.02; th_ds <- -0.05

# DGP: x=観測シフター(人口・高齢化率等), u=未観測の地域需要要因
make_panel <- function(endogenous=FALSE){
  store<-rep(1:N,each=T); period<-rep(1:T,times=N)
  yr<-((period-1)%/%12)+1; mo<-((period-1)%%12)+1
  alpha<-rnorm(N,20,1.5)[store]
  x <- rnorm(N,0,1)[store] + rnorm(N*T,0,0.5)   # 観測シフター(回帰に入れられる)
  u <- rnorm(N,0,1)[store] + rnorm(N*T,0,0.5)   # 未観測の地域需要(入れられない)
  gamma_y<-rnorm(years,0,0.5)[yr]; delta_m<-sin(2*pi*mo/12)*0.4
  if(!endogenous){
    ncomp<-rpois(N*T,3)                          # 外生
  } else {                                       # 内生: 需要・店舗質が高いほど参入
    ncomp<-rpois(N*T, pmax(0.2, 3 + 1.0*x + 1.0*u + 0.05*(alpha-20)))
  }
  SM<-rpois(N*T,5); HC<-rpois(N*T,1); DS<-rpois(N*T,2)
  margin<-alpha + beta_true*ncomp + th_sm*SM + th_hc*HC + th_ds*DS +
          2*x + 2*u + gamma_y + delta_m + rnorm(N*T,0,0.8)
  data.frame(store,period,margin,ncomp,SM,HC,DS,x,u)
}

# two-way within 変換 (均衡パネルで厳密): ỹ = y - 店舗平均 - 期平均 + 全体平均
demean2<-function(v,s,p) v-ave(v,s)-ave(v,p)+mean(v)
# 固定効果回帰 → ncomp 係数とロバスト(HC1風)SE
fe_fit<-function(d,xcols){
  yt<-demean2(d$margin,d$store,d$period)
  Xt<-as.matrix(sapply(xcols,function(c) demean2(d[[c]],d$store,d$period)))
  fit<-lm.fit(Xt,yt); b<-fit$coefficients; e<-fit$residuals
  XtXi<-solve(crossprod(Xt)); meat<-t(Xt)%*%(Xt*e^2)
  dof<-length(yt)-ncol(Xt)-((N-1)+(T-1))
  se<-sqrt(diag(XtXi%*%meat%*%XtXi*(length(yt)/dof)))
  c(beta=b[1], se=se[1])
}

## --- 4つの定式化を 外生/内生 で比較 ---
compare<-function(endo){
  d<-make_panel(endo)
  s1<-summary(lm(margin~ncomp,data=d))$coefficients["ncomp",1:2]   # プールドOLS
  s2<-fe_fit(d,c("ncomp","SM","HC","DS"))                          # FE
  s3<-fe_fit(d,c("ncomp","SM","HC","DS","x"))                      # FE+観測シフター(報告書の定式化)
  s4<-fe_fit(d,c("ncomp","SM","HC","DS","x","u"))                  # FE+観測+未観測(infeasibleなオラクル)
  m<-rbind(s1,s2,s3,s4); rownames(m)<-c(
    "Spec1 プールドOLS","Spec2 FE","Spec3 FE+観測シフター(報告書型)","Spec4 FE+未観測も統制(理想/不可能)")
  colnames(m)<-c("beta_hat","SE"); round(m,3)
}
cat("真のβ =",beta_true,"\n\n=== 競合数が【外生】の世界 ===\n"); print(compare(FALSE))
cat("\n=== 競合数が【内生】の世界(高需要・高利益率の市場ほど競合が参入) ===\n"); print(compare(TRUE))

## --- ダミー変数アプローチ(独占商圏=0社 基準の段階効果) ---
true_steps<-c("0"=0,"1"=-0.10,"2"=-0.30,"3"=-0.44,"4plus"=-0.67)  # 報告書2km商圏の値
N2<-N
dd<-local({
  store<-rep(1:N2,each=T); period<-rep(1:T,times=N2)
  alpha<-rnorm(N2,20,1.5)[store]
  cn<-sample(0:4,N2*T,TRUE,c(.15,.25,.25,.2,.15))
  lab<-factor(ifelse(cn>=4,"4plus",as.character(cn)),levels=names(true_steps))
  margin<-alpha+true_steps[as.character(lab)]+rnorm(N2*T,0,0.8)
  data.frame(store,period,margin,lab)
})
ydm<-demean2(dd$margin,dd$store,dd$period)
mmd<-apply(model.matrix(~lab,dd)[,-1,drop=FALSE],2,demean2,s=dd$store,p=dd$period)
est<-lm.fit(mmd,ydm)$coefficients
cat("\n=== ダミー変数アプローチ: 独占商圏(0社)基準の段階効果 ===\n")
print(data.frame(競合グループ数=c("1社","2社","3社","4社以上"),
                 真の効果=unname(true_steps[-1]),推定値=round(est,3)),row.names=FALSE)

## --- モンテカルロ: 報告書型Spec3のβ̂分布を 外生 vs 内生 で500回 ---
R<-500
mc<-function(endo) replicate(R, fe_fit(make_panel(endo),c("ncomp","SM","HC","DS","x"))["beta.ncomp"])
b_exo<-mc(FALSE); b_end<-mc(TRUE)
cat(sprintf("\n[モンテカルロ %d回, Spec3] 外生 平均β̂=%.3f / 内生 平均β̂=%.3f (真値=%.2f)\n",
            R,mean(b_exo),mean(b_end),beta_true))
png("margin_montecarlo.png",width=1000,height=600,res=120)
xr<-range(c(b_exo,b_end,beta_true)); h1<-hist(b_exo,30,plot=FALSE); h2<-hist(b_end,30,plot=FALSE)
plot(h1,col=rgb(0,0.4,0.7,0.5),xlim=xr,ylim=c(0,max(h1$counts,h2$counts)),
     main="Monte Carlo: estimated beta (Spec3 = FE + observed shifter)",
     xlab="estimated beta",ylab="frequency")
plot(h2,col=rgb(0.85,0.2,0.1,0.5),add=TRUE); abline(v=beta_true,lwd=2,lty=2)
legend("topright",c("competitors exogenous","competitors endogenous","true beta = -0.16"),
       fill=c(rgb(0,0.4,0.7,0.5),rgb(0.85,0.2,0.1,0.5),NA),border=c(1,1,NA),
       lty=c(NA,NA,2),lwd=c(NA,NA,2),bg="white",cex=0.85)
dev.off(); cat("図を margin_montecarlo.png に出力\n")
