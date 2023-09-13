# experiment 3
# early and long RTW against a late RTW (80 vs 51)

source("experiments/utils.R")

## dependent variable

tmp = dat[,c(27, 29)]

vY = rep(0, nrow(tmp))
vY[tmp[,1] == 1 & (tmp[,2]==1)] = 1 # early RTW, 92 stk

pick = vY
pick = pick | (tmp[,1] != 1 & (tmp[,2]==1)) # late RTW 51

tmp = getData(dat,vY, pick)

# pick the data
vY = tmp$vY[tmp$pick]
#vW = tmp$vW[pick]
mX = tmp$mX[tmp$pick,]

#change packyear
mX[mX[,11] > 0,11] = 1

tab = NULL

## example university etc
for(iter in 1:12){
  ret = causal_analysis(iter, vY, mX)
  tab = rbind(tab, c(sum(mX[,iter]),sum(vY[!!mX[,iter]]),sum(!mX[,iter]),sum(vY[!mX[,iter]]),t(ret[,c(1,4)])))
}


tab = tab[, 1:6]; tab[, 5:6] = tab[, 5:6]
#tab = tab[, 1:6]; tab[, 5:6] = tab[, 5:6]*100
colnames(tab) = c("1","11","0","01","ACEe", "ACEp")
actions = c("UniEdu", "Spouse", "House", "WhiteC", "PinkC", "Oropharynx", "Oral", "Larynx",
            "LateStage", "MulModality","Smoking","Age")
tmp = data.frame(Variables = actions)
tab = data.frame(tmp, round(tab,4))
#tab = data.frame(tmp, round(tab,2))

tab = data.frame(tab$Variables,
                 paste0(round(tab$X11/tab$X1*100,2),"% (", tab$X1,")"),
                 paste0(round(tab$X01/tab$X0*100,2),"% (", tab$X0,")"),
                 tab$ACEe, tab$ACEp)
colnames(tab) = c("Var", "X1", "X0", "ACEe", "ACEp")

tab = flextable(tab)  %>%
  add_header_row(values = c("IRTW3m", sum(vY), sum(!vY), rep("",2)),top=F) %>%
  set_caption(caption = "The results for IRTW3m")