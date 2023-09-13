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
