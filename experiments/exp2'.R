# experiment 2
# returned to work within 3 months after the medical treatment
# but could not keep working afterwards

source("experiments/utils.R")

## dependent variable

tmp = dat[,c(27, 29)]
#nrow(tmp); sum(tmp[,1]==1 & (tmp[,2]==1 | tmp[,2]==4 | tmp[,2]==5))
ttmp = (tmp[,1] >= 6 & tmp[,2] >= 6) | (tmp[,1]==4 | tmp[,1]==5)

vY = rep(0, nrow(tmp))
vY[tmp[,1] == 1 & (tmp[,2]==0 | tmp[,2]==2 | tmp[,2]==3)] = 1 # 3m

pick = !ttmp


#pick = tmp[,1] == 1 & (tmp[,2]==1 | tmp[,2]==4 | tmp[,2]==5)
#pick = pick | (tmp[,1] == 1 & (tmp[,2]==0 | tmp[,2]==2 | tmp[,2]==3))

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


colnames(tab) = c("1","11","0","01","ACEe", "ACEp", "ACAe", "ACAp", "ACCe", "ACCp")
actions = c("UniEdu", "Spouse", "House", "WhiteC", "PinkC", "Oropharynx", "Oral", "Larynx",
            "LateStage", "MulModality","Smoking")
tmp = data.frame(Variables = actions)
tab = data.frame(tmp, round(tab,4))

tab = flextable(tab)  %>%
  add_header_row(values = c("IRTW3m'", sum(vY), "", sum(!vY), rep("",7)),top=F) %>%
  set_caption(caption = "The results for IRTW3m'")
