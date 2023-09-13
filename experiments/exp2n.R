# experiment 2 new
# remove the retired patients, and include the unemployed
# returned to work within 3 months after the medical treatment
# and kept working up to 12 months or until retirement

source("experiments/utils.R")

## dependent variable

tmp = dat[,c(27, 29)]
ttmp = (tmp[,1] == 6 | tmp[,1] == 99) & (tmp[,2] == 6 | tmp[,2] == 99)
ttmp = ttmp | (tmp[,1]==4 | tmp[,1]==5) | (tmp[,2]==4 | tmp[,2]==5)

vY = rep(0, nrow(tmp))
vY[tmp[,1] == 1 & (tmp[,2]==1 | tmp[,2]==4 | tmp[,2]==5)] = 1 # 3m

pick = !ttmp

tmp = getData(dat,vY, pick)

# pick the data
vY = tmp$vY[tmp$pick]
#vW = tmp$vW[pick]
mX = tmp$mX[tmp$pick,]

#change packyear
mX[mX[,11] > 0,11] = 1

tab = NULL

## example university etc
for(iter in 1:11){
  ret = causal_analysis(iter, vY, mX)
  tab = rbind(tab, c(sum(mX[,iter]),sum(vY[!!mX[,iter]]),sum(!mX[,iter]),sum(vY[!mX[,iter]]),t(ret[,c(1,4)])))
}


tab = tab[, 1:6]; tab[, 5:6] = tab[, 5:6]
#tab = tab[, 1:6]; tab[, 5:6] = tab[, 5:6]*100
colnames(tab) = c("1","11","0","01","ACEe", "ACEp")
actions = c("UniEdu", "Spouse", "House", "WhiteC", "PinkC", "Oropharynx", "Oral", "Larynx",
            "LateStage", "MulModality","Smoking")
tmp = data.frame(Variables = actions)
tab = data.frame(tmp, round(tab,4))
#tab = data.frame(tmp, round(tab,2))

tab = flextable(tab)  %>%
  add_header_row(values = c("IRTW", sum(vY), "", sum(!vY), rep("",3)),top=F) %>%
  set_caption(caption = "The results for IRTW-P1P2")
