library(readr)
dat1 <- read_csv("C:/Users/Hafsa/Desktop/Corona-Project-master/Kaggle data/novel-corona-virus-2019-dataset2/COVID19_line_list_data.csv")
dim(dat1)

#dat2 <- read_csv("C:/Users/Hafsa/Desktop/Corona-Project-master/Kaggle data/novel-corona-virus-2019-dataset/COVID19_open_line_list.csv")
#dim(dat2)


str(dat1)


table(dat1$death)

no_outcome_yet <- "0"
'%!in%' <- function(x,y)!('%in%'(x,y))
death_dat <- dat1[dat1$death %!in% no_outcome_yet,]
recover_dat <- dat1[dat1$recovered %!in% no_outcome_yet,]

dim(death_dat)
dim(recover_dat)


dat <- rbind(recover_dat,death_dat)

dat$case_outcome <- ifelse(dat$death == 0, dat$recovered,
                           ifelse(dat$recovered == 0, dat$death,NA))

dat$dead_or_alive <- ifelse(dat$death == 0,"Alive",
                           ifelse(dat$recovered == 0,"Dead",NA))

View(dat)

write.csv(dat,"C:/Users/Hafsa/Desktop/Corona-Project-master/Kaggle data/novel-corona-virus-2019-dataset2/dead_or_alive.csv")
