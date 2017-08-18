fd[!is.na(fd$age_2010 - fd$age_2005) & !(fd$age_2010 - fd$age_2005) %in% c(4,5), c(1,4,29)]

colnames(fd)
fd[abs(fd$age_2015 - fd$age_2010) > 1,]
