# utils.R

# merge the table
merge_custom <- function(ft, x, columns){
  z <- rle(x)
  rows_at <- cumsum(z$lengths) - z$lengths + 1
  
  for(i in seq_along(rows_at)){
    for(j in columns)
      ft <- merge_at(x = ft, i = seq( rows_at[i], rows_at[i] + z$lengths[i] - 1), j = j)
  }
  
  return(ft)
}

# build mX and return a list
# may change pick
# but doesn't change vY
getData <- function(dat, vY, pick){
  
  ## mX, covariates
  mX = NULL
  
  # education, UnderUni(0) och Uni(1)
  tmp = (dat[,6] == 4) | (dat[,6] == 5)
  mX = cbind(mX, tmp)
  
  # Marital Status, “with support from the spouse all the time”(1) and “without …”(0)
  tmp = (dat[,4] == 1)
  mX = cbind(mX, tmp)
  
  # Accommodation, “eget hus”(1) and “lägenhet eller hyr”(0)
  tmp = (dat[,5] == 1)
  mX = cbind(mX, tmp)
  
  # Occupation, “blue collar”, “white collar” and “healthcare”
  # white collar
  tmp = (dat[,7] == 2) | (dat[,7] == 4) # merge student to white collar
  mX = cbind(mX, tmp)
  # healthcare
  tmp = (dat[,7] == 3)
  mX = cbind(mX, tmp)
  
  # Lokalisation: “Oropharynx”, “Oral Cavity”, “Larynx” and “the others”
  # “Oropharynx”
  tmp = (dat[,13] == 1)
  mX = cbind(mX, tmp)
  # “Oral Cavity”
  tmp = (dat[,13] == 2)
  mX = cbind(mX, tmp)
  # “Larynx”
  tmp = (dat[,13] == 3)
  mX = cbind(mX, tmp)
  
  # Stadium: “I and II (early stage)”, “III and IV (late stage)”(1)
  tmp = rep(TRUE, nrow(dat)); tmp[(dat[,14]=="1") | (dat[,14]=="2")] = FALSE
  mX = cbind(mX, tmp)
  # pick = pick & (dat[,14] != 98) # remove the ones with unknown stadium
  
  # medical treatment: “Single modality (Surgery + Radio)” and “Multiple modality (the others)”(1)
  tmp = (dat[,19] != 1) & (dat[,19] != 3)
  mX = cbind(mX, tmp)
  
  # smoking packyears
  # tmp = dat[,10] * dat[,11] / 20; tmp[is.na(tmp)] = 0
  tmp = as.integer(dat[,9]>1)
  mX = cbind(mX, tmp)
  # pick = pick & !((dat[,9] != 1) & (tmp==0)) # remove the ones with unknown pack-year
  
  # age
  # tmp = dat[,3] >= 60
  # tmp = dat[,3] >= 55.1
  tmp = dat[,3] >= 58
  mX = cbind(mX, tmp)
  
  colnames(mX) = c("uni", "spouse", "hus", "white", "health", "oropharynx", "oral", "larynx",
                   "late", "mulmod", "packyear", "age")
  
  return(list(vY = vY, mX = mX, pick = pick))
}


causal_analysis <- function(treat, vY, mX){
  vw = mX[,treat]
  vy = vY; mx = mX[,-treat]
  mx = data.frame(mx)
  
  # estimate propensity score
  fomul = paste("vw ~", paste(names(mx), collapse=" + "))
  cat(fomul,"\n")
  fomul = formula(fomul)
  glm1 = glm(fomul,family=binomial, data=mx)
  
  ate = Match(Y=vy,Tr=vw, X=glm1$fitted, estimand="ATE")
  summary(ate) # ATE, average treatment effect
  att = Match(Y=vy,Tr=vw, X=glm1$fitted, estimand="ATT")
  summary(att) # ATT, average treatment for the treated
  atc = Match(Y=vy,Tr=vw, X=glm1$fitted, estimand="ATC")
  summary(atc) # ATC, average treatment for the control
  
  ret = matrix(0, 3, 4)
  
  ret[1,1] = ate$est
  ret[1,2] = ate$se
  ret[1,3] = c(ate$est/ate$se)
  ret[1,4] = 1-pchisq(c(ate$est/ate$se)**2,1)
  
  ret[2,1] = att$est
  ret[2,2] = att$se
  ret[2,3] = c(att$est/att$se)
  ret[2,4] = 1-pchisq(c(att$est/att$se)**2,1)
  
  ret[3,1] = atc$est
  ret[3,2] = atc$se
  ret[3,3] = c(atc$est/atc$se)
  ret[3,4] = 1-pchisq(c(atc$est/atc$se)**2,1)
  
  colnames(ret) = c("est","se","tval","pval")
  rownames(ret) = c("ATE","ATT","ATC")
  return(ret)
}

### Functions for Causation Analysis

## test considering covariates but only the intercept

# with covariates
Randomize <- function(seed, vY, mX){
  set.seed(seed)
  iN = length(vY)
  tmp = sample.int(iN, ceiling(iN/2))
  vWp = rep(FALSE, iN); vWp[tmp] = TRUE
  
  mXX = cbind(1, mX, vWp)
  beta = chol2inv(chol(crossprod(mXX))) %*% crossprod(mXX, vY)
  
  return(beta[length(beta)])
}

# consider covariates but only the intercept
RE_Test <- function(vY, mX, vW, iM, seed){
  mXX = cbind(1, mX, vW)
  beta = chol2inv(chol(crossprod(mXX))) %*% crossprod(mXX, vY)
  test = abs(beta[length(beta)])
  
  tmp = sapply(seed:(seed+iM-1), Randomize, vY=vY, mX=mX)
  return(list(test = test, pval = sum(abs(tmp) >= test)/length(tmp)))
}

#RE_Test(vY, NULL, vW, iM, seed)


## test considering covariates, non robust standard errors

# new new covariates
# look at the differences between the estimates and then Hotelling

RandomizeX <- function(seed, vY, mX){
  set.seed(seed)
  iN = length(vY)
  tmp = sample.int(iN, ceiling(iN/2))
  vWp = rep(FALSE, iN); vWp[tmp] = TRUE
  
  mX1 = cbind(1, mX[vWp,]); mX0 = cbind(1, mX[!vWp,])
  vY1 = vY[vWp]; vY0 = vY[!vWp]
  
  XX1i = chol2inv(chol(crossprod(mX1)))
  XX0i = chol2inv(chol(crossprod(mX0)))
  b1 = XX1i %*% crossprod(mX1, vY1)
  b0 = XX0i %*% crossprod(mX0, vY0)
  
  ve1 = c(vY1 - mX1%*%b1)
  ve0 = c(vY0 - mX0%*%b0)
  S1 = XX1i * c(crossprod(ve1)) / length(vY1)
  S0 = XX0i * c(crossprod(ve0)) / length(vY0)
  
  return(c(t(b1 - b0) %*% chol2inv(chol(S1+S0)) %*% (b1 - b0)))
}

REX_Test <- function(vY, mX, vW, iM, seed){
  mXX = cbind(1, mX)
  mX1 = mXX[vW,]; mX0 = mXX[!vW,]
  vY1 = vY[vW]; vY0 = vY[!vW]
  
  XX1i = chol2inv(chol(crossprod(mX1)))
  XX0i = chol2inv(chol(crossprod(mX0)))
  b1 = XX1i %*% crossprod(mX1, vY1)
  b0 = XX0i %*% crossprod(mX0, vY0)
  
  ve1 = c(vY1 - mX1%*%b1)
  ve0 = c(vY0 - mX0%*%b0)
  S1 = XX1i * c(crossprod(ve1)) / length(vY1)
  S0 = XX0i * c(crossprod(ve0)) / length(vY0)
  
  test = c(t(b1 - b0) %*% chol2inv(chol(S1+S0)) %*% (b1 - b0))
  
  tmp = sapply(seed:(seed+iM-1), RandomizeX, vY=vY, mX=mX)
  return(list(test = test, pval = sum(tmp >= test)/length(tmp)))
}

#REX_Test(vY, matrix(vX,length(vX),1), vW, iM, seed)


## test considering covariates, non robust standard errors

# look at the differences between the estimates and then Hotelling
# can choose which parameters to be varying

RandomizeXX <- function(seed, vY, mX, iNN, vPar, vv){
  set.seed(seed)
  iN = length(vY)
  tmp = sample.int(iN, iNN)
  vWp = rep(FALSE, iN); vWp[tmp] = TRUE
  
  if(is.null(mX)){
    mXX=matrix(1, length(vY), 1)
    if(0 %in% vPar) mXX = cbind(mXX, vWp)
  }else{
    mXX = cbind(1, mX)
    if(0 %in% vPar) mXX = cbind(mXX, vWp)
    mXX = cbind(mXX, (mX*vWp)[,vPar])
  }
  
  XXi = chol2inv(chol(crossprod(mXX)))
  beta = XXi %*% crossprod(mXX, vY)
  
  ve = c(vY - mXX%*%beta)
  mS = XXi * c(crossprod(ve)) / length(vY)
  
  test = vv %*% beta
  test = c(t(test) %*% chol2inv(chol(vv%*%mS%*%t(vv))) %*% test)
  
  return(test)
}

# vPar is a vector indicating which parameters to vary
# it should include zero, but if it does not, intercept shall be removed in the varying part
# 0 stands for intercept, and other numbers are the column numbers of mX
REXX_Test <- function(vY, mX=NULL, vW, iM, seed, vPar=0){
  if(is.null(mX)){
    mXX=matrix(1, length(vY), 1)
    if(0 %in% vPar) mXX = cbind(mXX, vW)
    ncx = 0
  }else{
    mXX = cbind(1, mX)
    if(0 %in% vPar) mXX = cbind(mXX, vW)
    mXX = cbind(mXX, (mX*vW)[,vPar])
    ncx = ncol(mX)
  }
  
  inn = sum(vW)
  
  XXi = chol2inv(chol(crossprod(mXX)))
  beta = XXi %*% crossprod(mXX, vY)
  
  ve = c(vY - mXX%*%beta)
  mS = XXi * c(crossprod(ve)) / length(vY)
  
  vv = matrix(0, length(vPar), ncx+1)
  vv = cbind(vv, diag(length(vPar)))
  
  test = vv %*% beta
  test = c(t(test) %*% chol2inv(chol(vv%*%mS%*%t(vv))) %*% test)
  
  tmp = sapply(seed:(seed+iM-1), RandomizeXX, vY=vY, mX=mX, iNN=inn, vPar=vPar, vv=vv)
  
  return(list(test = test, pval = sum(tmp >= test)/length(tmp), beta=beta, ve=ve))
}

#REXX_Test(vY, matrix(vX,length(vX),1), vW, iM, seed, 0:1)
#RE_Test(vY=vY, mX=matrix(vX,length(vX),1), vW=vW, iM=iM, seed=seed)

#REXX_Test(vY=vY, mX=matrix(vX,length(vX),1), vW=vW, iM=iM, seed=seed, vPar=0)
