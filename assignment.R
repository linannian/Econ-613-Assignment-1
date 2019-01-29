## Assignment 1 Ethan Sicong Lu
getwd()
setwd('Documents/R programming/Econ 613/assignment1/')

## Exercise 1
student <- read.csv('datstu.csv')
district <- read.csv('datjss.csv')
school <- read.csv('datsss.csv')

summary(student)
str(student)
## number of student = 340,823

summary(school)
length(unique(school$schoolcode))
## number of school = 898

levels(student$choicepgm1)
levels(student$choicepgm2)
levels(student$choicepgm3)
levels(student$choicepgm4)
levels(student$choicepgm5)
# number of program = 32

num_of_choice <- student[,5:16]
num_of_choice[num_of_choice==''] <- NA
num_of_choice <- as.matrix(num_of_choice)
unique1 <- matrix(c(NA,NA),nrow=1)
for (i in 1:6){
  unique1 <- rbind(unique1,unique(num_of_choice[,c(i,i+6)]))
}
omi_unique <- na.omit(unique1)
length(unique(omi_unique)[,1])
# number of choices = 2,773

test_score <- as.matrix(student$score)
test_score[test_score==''] <- NA
sum(is.na(test_score))
# number of missing test score = 179,887

school_code <- student[,5:10]
school_code[school_code==''] <- NA
school_code <- as.matrix(school_code)
n <- nrow(school_code)
count <- 0
for (i in 1:n){
  l1 <- length(as.numeric(na.omit(school_code[i,1:6])))
  l2 <- length(unique(as.numeric(na.omit(school_code[i,1:6]))))
  if (l1 > l2){
    count <- count+1
  }
}
# number of people applied to same school = 120,071

program <- as.matrix(student[,11:16])
program[program==''] <- NA
nrow(program[!complete.cases(program),])
# number of student applied less than 6 program  = 20,988

## Exercise 2
admitted <- student[complete.cases(student$rankplace),]
admitted <- admitted[admitted$rankplace<7,]

summary(admitted)
str(admitted)

admitted_rank_1 <- admitted[admitted$rankplace == 1,]
admitted_rank_2 <- admitted[admitted$rankplace == 2,]
admitted_rank_3 <- admitted[admitted$rankplace == 3,]
admitted_rank_4 <- admitted[admitted$rankplace == 4,]
admitted_rank_5 <- admitted[admitted$rankplace == 5,]
admitted_rank_6 <- admitted[admitted$rankplace == 6,]

student_info_1 <- admitted_rank_1[,c(-6,-7,-8,-9,-10,-12,-13,-14,-15,-16)]
student_info_2 <- admitted_rank_2[,c(-5,-7,-8,-9,-10,-11,-13,-14,-15,-16)]
student_info_3 <- admitted_rank_3[,c(-5,-6,-8,-9,-10,-11,-12,-14,-15,-16)]
student_info_4 <- admitted_rank_4[,c(-5,-6,-7,-9,-10,-11,-12,-13,-15,-16)]
student_info_5 <- admitted_rank_5[,c(-5,-6,-7,-8,-10,-11,-12,-13,-14,-16)]
student_info_6 <- admitted_rank_6[,c(-5,-6,-7,-8,-9,-11,-12,-13,-14,-15)]

colnames(student_info_1) <- c("X","score","agey","male","schoolcode","choicepgm","jssdistrict","rankplace")
colnames(student_info_2) <- c("X","score","agey","male","schoolcode","choicepgm","jssdistrict","rankplace")
colnames(student_info_3) <- c("X","score","agey","male","schoolcode","choicepgm","jssdistrict","rankplace")
colnames(student_info_4) <- c("X","score","agey","male","schoolcode","choicepgm","jssdistrict","rankplace")
colnames(student_info_5) <- c("X","score","agey","male","schoolcode","choicepgm","jssdistrict","rankplace")
colnames(student_info_6) <- c("X","score","agey","male","schoolcode","choicepgm","jssdistrict","rankplace")

student_info_refined <- rbind(student_info_1,student_info_2,student_info_3,student_info_4,student_info_5,student_info_6)
summary(student_info_refined)
str(student_info_refined)

school_list <- unique(student_info_refined[,5:6])
str(school_list)

m1 <- student$jssdistrict[1:2300] ## just put a vector of factors here, will change it later
m2 <- matrix(0,nrow = 2300,ncol = 5)
school_list <- cbind.data.frame(school_list,m1,m2)
colnames(school_list) <- c("schoolcode","choicepgm","district","latitude","longtitude","cutoff","quality","size")
str(school_list)
n <- nrow(school_list)
for (i in 1:n){
  holder <- student_info_refined[student_info_refined$schoolcode == school_list[i,1] & student_info_refined$choicepgm == school_list[i,2],]
  school_list[i,6] <- min(holder$score)
  school_list[i,7] <- mean(holder$score)
  school_list[i,8] <- nrow(holder)
}

for (i in 1:n){
  holder2 <- school[school$schoolcode == school_list[i,1],]
  school_list[i,3] <- holder2[1,4]
  school_list[i,4] <- holder2[1,6]
  school_list[i,5] <- holder2[1,5]
}
# The answer for Exercise 2 is: school_list
View(school_list)
write.csv(school_list,file = 'school_list.csv')

## Exercise 3
m3 <- matrix(0, nrow = nrow(student),ncol=8)
colnames(m3) <- c('jsslong','jsslat','d1','d2','d3','d4','d5','d6')
student_w_dist <- cbind.data.frame(student,m3)
remove(m3)

distance <- function(ssslong,ssslat,jsslong,jsslat){
  dist_calc <- sqrt((69.172*(ssslong - jsslong)*cos(jsslat/57.3))^2+(69.172*(ssslat-jsslat))^2)
  return(dist_calc)
}

str(student_info_refined2)
str(district)
# district and jssdistrict of student has different levels, change levels first
dist_test <- district
dist_test[,2] <- factor(dist_test[,2],levels = levels(student$jssdistrict))

n <- nrow(student)
for (i in 1:n){
  holder3 <- dist_test[dist_test$jssdistrict == student_w_dist[i,17],]
  student_w_dist[i,19] <- holder3[1,3]
  student_w_dist[i,20] <- holder3[1,4]
}

school_refined <- school[complete.cases(school[,5:6]),]

# this loop is very long, takes a hour
for (i in 1:n){
  holder_school1 <- school_refined[school_refined$schoolcode == student_w_dist[i,5],]
  holder_school2 <- school_refined[school_refined$schoolcode == student_w_dist[i,6],]
  holder_school3 <- school_refined[school_refined$schoolcode == student_w_dist[i,7],]
  holder_school4 <- school_refined[school_refined$schoolcode == student_w_dist[i,8],]
  holder_school5 <- school_refined[school_refined$schoolcode == student_w_dist[i,9],]
  holder_school6 <- school_refined[school_refined$schoolcode == student_w_dist[i,10],]
  student_w_dist[i,21] <- distance(holder_school1[1,5],holder_school1[1,6],student_w_dist[i,19],student_w_dist[i,20])
  student_w_dist[i,22] <- distance(holder_school2[1,5],holder_school2[1,6],student_w_dist[i,19],student_w_dist[i,20])
  student_w_dist[i,23] <- distance(holder_school3[1,5],holder_school3[1,6],student_w_dist[i,19],student_w_dist[i,20])
  student_w_dist[i,24] <- distance(holder_school4[1,5],holder_school4[1,6],student_w_dist[i,19],student_w_dist[i,20])
  student_w_dist[i,25] <- distance(holder_school5[1,5],holder_school5[1,6],student_w_dist[i,19],student_w_dist[i,20])
  student_w_dist[i,26] <- distance(holder_school6[1,5],holder_school6[1,6],student_w_dist[i,19],student_w_dist[i,20])
}

# answer for Exercise 3 is incluede in student_w_dist
View(student_w_dist)
write.csv(student_w_dist,'student_with_distance.csv')

# saving space version, since github doesn't accept document more than 25MB
student_w_dist_shorter <- student_w_dist[,c(1,21,22,23,24,25,26)]
write.csv(student_w_dist_shorter,'Exercise_3_output.csv')

## Exercise 4
cutoff_and_quality <- matrix(0, nrow = nrow(student_w_dist), ncol = 12)
colnames(cutoff_and_quality) <- c('C1Cut','C1Qua','C2Cut','C2Qua','C3Cut','C3Qua',
                                  'C4Cut','C4Qua','C5Cut','C5Qua','C6Cut','C6Qua')
student_comprehensive <- cbind.data.frame(student_w_dist,cutoff_and_quality)
remove(cutoff_and_quality)

str(school_list)
str(student_comprehensive)

# change levels first
for (i in 11:16) {
  student_comprehensive[,i] <- factor(student_comprehensive[,i],levels = levels(school_list$choicepgm))
}

str(school_list)
str(student_comprehensive)

## this loop is very long, takes about 3hrs
for (i in 1:n){
  holder_program1 <- school_list[school_list$schoolcode == student_comprehensive[i,5] & school_list$choicepgm == student_comprehensive[i,11],]
  holder_program2 <- school_list[school_list$schoolcode == student_comprehensive[i,6] & school_list$choicepgm == student_comprehensive[i,12],]
  holder_program3 <- school_list[school_list$schoolcode == student_comprehensive[i,7] & school_list$choicepgm == student_comprehensive[i,13],]
  holder_program4 <- school_list[school_list$schoolcode == student_comprehensive[i,8] & school_list$choicepgm == student_comprehensive[i,14],]
  holder_program5 <- school_list[school_list$schoolcode == student_comprehensive[i,9] & school_list$choicepgm == student_comprehensive[i,15],]
  holder_program6 <- school_list[school_list$schoolcode == student_comprehensive[i,10] & school_list$choicepgm == student_comprehensive[i,16],]
  student_comprehensive[i,27] <- holder_program1[1,6]
  student_comprehensive[i,28] <- holder_program1[1,7]
  student_comprehensive[i,29] <- holder_program2[1,6]
  student_comprehensive[i,30] <- holder_program2[1,7]
  student_comprehensive[i,31] <- holder_program3[1,6]
  student_comprehensive[i,32] <- holder_program3[1,7]
  student_comprehensive[i,33] <- holder_program4[1,6]
  student_comprehensive[i,34] <- holder_program4[1,7]
  student_comprehensive[i,35] <- holder_program5[1,6]
  student_comprehensive[i,36] <- holder_program5[1,7]
  student_comprehensive[i,37] <- holder_program6[1,6]
  student_comprehensive[i,38] <- holder_program6[1,7]
}
write.csv(student_comprehensive,'student_comprehensive.csv')

stat_E4 <- matrix(0,ncol=6,nrow = 30)
colnames(stat_E4) <- c('cutoff Avg','cutoff Sd','quality Avg','quality Sd','distance Avg','distance Sd')
rownames(stat_E4) <- c('rank1','rank2','rank3','rank4','rank5','rank6',
                       'Q1rank1','Q1rank2','Q1rank3','Q1rank4','Q1rank5','Q1rank6',
                       'Q2rank1','Q2rank2','Q2rank3','Q2rank4','Q2rank5','Q2rank6',
                       'Q3rank1','Q3rank2','Q3rank3','Q3rank4','Q3rank5','Q3rank6',
                       'Q4rank1','Q4rank2','Q4rank3','Q4rank4','Q4rank5','Q4rank6')

stat_E4[1,1] <- mean(student_comprehensive$C1Cut,na.rm = TRUE); stat_E4[1,2] <- sd(student_comprehensive$C1Cut,na.rm = TRUE)
stat_E4[2,1] <- mean(student_comprehensive$C2Cut,na.rm = TRUE); stat_E4[2,2] <- sd(student_comprehensive$C2Cut,na.rm = TRUE)
stat_E4[3,1] <- mean(student_comprehensive$C3Cut,na.rm = TRUE); stat_E4[3,2] <- sd(student_comprehensive$C3Cut,na.rm = TRUE)
stat_E4[4,1] <- mean(student_comprehensive$C4Cut,na.rm = TRUE); stat_E4[4,2] <- sd(student_comprehensive$C4Cut,na.rm = TRUE)
stat_E4[5,1] <- mean(student_comprehensive$C5Cut,na.rm = TRUE); stat_E4[5,2] <- sd(student_comprehensive$C5Cut,na.rm = TRUE)
stat_E4[6,1] <- mean(student_comprehensive$C6Cut,na.rm = TRUE); stat_E4[6,2] <- sd(student_comprehensive$C6Cut,na.rm = TRUE)

stat_E4[1,3] <- mean(student_comprehensive$C1Qua,na.rm = TRUE); stat_E4[1,4] <- sd(student_comprehensive$C1Qua,na.rm = TRUE)
stat_E4[2,3] <- mean(student_comprehensive$C2Qua,na.rm = TRUE); stat_E4[2,4] <- sd(student_comprehensive$C2Qua,na.rm = TRUE)
stat_E4[3,3] <- mean(student_comprehensive$C3Qua,na.rm = TRUE); stat_E4[3,4] <- sd(student_comprehensive$C3Qua,na.rm = TRUE)
stat_E4[4,3] <- mean(student_comprehensive$C4Qua,na.rm = TRUE); stat_E4[4,4] <- sd(student_comprehensive$C4Qua,na.rm = TRUE)
stat_E4[5,3] <- mean(student_comprehensive$C5Qua,na.rm = TRUE); stat_E4[5,4] <- sd(student_comprehensive$C5Qua,na.rm = TRUE)
stat_E4[6,3] <- mean(student_comprehensive$C6Qua,na.rm = TRUE); stat_E4[6,4] <- sd(student_comprehensive$C6Qua,na.rm = TRUE)

stat_E4[1,5] <- mean(student_comprehensive$d1,na.rm = TRUE); stat_E4[1,6] <- sd(student_comprehensive$d1,na.rm = TRUE)
stat_E4[2,5] <- mean(student_comprehensive$d2,na.rm = TRUE); stat_E4[2,6] <- sd(student_comprehensive$d2,na.rm = TRUE)
stat_E4[3,5] <- mean(student_comprehensive$d3,na.rm = TRUE); stat_E4[3,6] <- sd(student_comprehensive$d3,na.rm = TRUE)
stat_E4[4,5] <- mean(student_comprehensive$d4,na.rm = TRUE); stat_E4[4,6] <- sd(student_comprehensive$d4,na.rm = TRUE)
stat_E4[5,5] <- mean(student_comprehensive$d5,na.rm = TRUE); stat_E4[5,6] <- sd(student_comprehensive$d5,na.rm = TRUE)
stat_E4[6,5] <- mean(student_comprehensive$d6,na.rm = TRUE); stat_E4[6,6] <- sd(student_comprehensive$d6,na.rm = TRUE)

k <- quantile(student_comprehensive_refined$score,probs = seq(0,1,0.25),na.rm = TRUE)
# the quartile line are 252,283,324,469

# Given the fact that people without score are more than 1/4 of the total population, 
# I can't seperate total data frame into 4 pieces acoording to their test score
# I can only partition the table for people with score into 4 pieces according to their scores.
student_comprehensive_refined <- student_comprehensive[complete.cases(student_comprehensive$score),]
student_comprehensive_refined <- student_comprehensive_refined[order(student_comprehensive_refined$score),]

q1 <- student_comprehensive_refined[student_comprehensive_refined$score <= 252,]
q2 <- student_comprehensive_refined[student_comprehensive_refined$score > 252 & student_comprehensive_refined$score<=283,]
q3 <- student_comprehensive_refined[student_comprehensive_refined$score > 283 & student_comprehensive_refined$score<=324,]
q4 <- student_comprehensive_refined[student_comprehensive_refined$score > 324,]

# q1 stats
stat_E4[7,1] <- mean(q1$C1Cut,na.rm = TRUE); stat_E4[7,2] <- sd(q1$C1Cut,na.rm = TRUE)
stat_E4[8,1] <- mean(q1$C2Cut,na.rm = TRUE); stat_E4[8,2] <- sd(q1$C2Cut,na.rm = TRUE)
stat_E4[9,1] <- mean(q1$C3Cut,na.rm = TRUE); stat_E4[9,2] <- sd(q1$C3Cut,na.rm = TRUE)
stat_E4[10,1] <- mean(q1$C4Cut,na.rm = TRUE); stat_E4[10,2] <- sd(q1$C4Cut,na.rm = TRUE)
stat_E4[11,1] <- mean(q1$C5Cut,na.rm = TRUE); stat_E4[11,2] <- sd(q1$C5Cut,na.rm = TRUE)
stat_E4[12,1] <- mean(q1$C6Cut,na.rm = TRUE); stat_E4[12,2] <- sd(q1$C6Cut,na.rm = TRUE)

stat_E4[7,3] <- mean(q1$C1Qua,na.rm = TRUE); stat_E4[7,4] <- sd(q1$C1Qua,na.rm = TRUE)
stat_E4[8,3] <- mean(q1$C2Qua,na.rm = TRUE); stat_E4[8,4] <- sd(q1$C2Qua,na.rm = TRUE)
stat_E4[9,3] <- mean(q1$C3Qua,na.rm = TRUE); stat_E4[9,4] <- sd(q1$C3Qua,na.rm = TRUE)
stat_E4[10,3] <- mean(q1$C4Qua,na.rm = TRUE); stat_E4[10,4] <- sd(q1$C4Qua,na.rm = TRUE)
stat_E4[11,3] <- mean(q1$C5Qua,na.rm = TRUE); stat_E4[11,4] <- sd(q1$C5Qua,na.rm = TRUE)
stat_E4[12,3] <- mean(q1$C6Qua,na.rm = TRUE); stat_E4[12,4] <- sd(q1$C6Qua,na.rm = TRUE)

stat_E4[7,5] <- mean(q1$d1,na.rm = TRUE); stat_E4[7,6] <- sd(q1$d1,na.rm = TRUE)
stat_E4[8,5] <- mean(q1$d2,na.rm = TRUE); stat_E4[8,6] <- sd(q1$d2,na.rm = TRUE)
stat_E4[9,5] <- mean(q1$d3,na.rm = TRUE); stat_E4[9,6] <- sd(q1$d3,na.rm = TRUE)
stat_E4[10,5] <- mean(q1$d4,na.rm = TRUE); stat_E4[10,6] <- sd(q1$d4,na.rm = TRUE)
stat_E4[11,5] <- mean(q1$d5,na.rm = TRUE); stat_E4[11,6] <- sd(q1$d5,na.rm = TRUE)
stat_E4[12,5] <- mean(q1$d6,na.rm = TRUE); stat_E4[12,6] <- sd(q1$d6,na.rm = TRUE)

# q2 stats
stat_E4[13,1] <- mean(q2$C1Cut,na.rm = TRUE); stat_E4[13,2] <- sd(q2$C1Cut,na.rm = TRUE)
stat_E4[14,1] <- mean(q2$C2Cut,na.rm = TRUE); stat_E4[14,2] <- sd(q2$C2Cut,na.rm = TRUE)
stat_E4[15,1] <- mean(q2$C3Cut,na.rm = TRUE); stat_E4[15,2] <- sd(q2$C3Cut,na.rm = TRUE)
stat_E4[16,1] <- mean(q2$C4Cut,na.rm = TRUE); stat_E4[16,2] <- sd(q2$C4Cut,na.rm = TRUE)
stat_E4[17,1] <- mean(q2$C5Cut,na.rm = TRUE); stat_E4[17,2] <- sd(q2$C5Cut,na.rm = TRUE)
stat_E4[18,1] <- mean(q2$C6Cut,na.rm = TRUE); stat_E4[18,2] <- sd(q2$C6Cut,na.rm = TRUE)

stat_E4[13,3] <- mean(q2$C1Qua,na.rm = TRUE); stat_E4[13,4] <- sd(q2$C1Qua,na.rm = TRUE)
stat_E4[14,3] <- mean(q2$C2Qua,na.rm = TRUE); stat_E4[14,4] <- sd(q2$C2Qua,na.rm = TRUE)
stat_E4[15,3] <- mean(q2$C3Qua,na.rm = TRUE); stat_E4[15,4] <- sd(q2$C3Qua,na.rm = TRUE)
stat_E4[16,3] <- mean(q2$C4Qua,na.rm = TRUE); stat_E4[16,4] <- sd(q2$C4Qua,na.rm = TRUE)
stat_E4[17,3] <- mean(q2$C5Qua,na.rm = TRUE); stat_E4[17,4] <- sd(q2$C5Qua,na.rm = TRUE)
stat_E4[18,3] <- mean(q2$C6Qua,na.rm = TRUE); stat_E4[18,4] <- sd(q2$C6Qua,na.rm = TRUE)

stat_E4[13,5] <- mean(q2$d1,na.rm = TRUE); stat_E4[13,6] <- sd(q2$d1,na.rm = TRUE)
stat_E4[14,5] <- mean(q2$d2,na.rm = TRUE); stat_E4[14,6] <- sd(q2$d2,na.rm = TRUE)
stat_E4[15,5] <- mean(q2$d3,na.rm = TRUE); stat_E4[15,6] <- sd(q2$d3,na.rm = TRUE)
stat_E4[16,5] <- mean(q2$d4,na.rm = TRUE); stat_E4[16,6] <- sd(q2$d4,na.rm = TRUE)
stat_E4[17,5] <- mean(q2$d5,na.rm = TRUE); stat_E4[17,6] <- sd(q2$d5,na.rm = TRUE)
stat_E4[18,5] <- mean(q2$d6,na.rm = TRUE); stat_E4[18,6] <- sd(q2$d6,na.rm = TRUE)

# q3 stats
stat_E4[19,1] <- mean(q3$C1Cut,na.rm = TRUE); stat_E4[19,2] <- sd(q3$C1Cut,na.rm = TRUE)
stat_E4[20,1] <- mean(q3$C2Cut,na.rm = TRUE); stat_E4[20,2] <- sd(q3$C2Cut,na.rm = TRUE)
stat_E4[21,1] <- mean(q3$C3Cut,na.rm = TRUE); stat_E4[21,2] <- sd(q3$C3Cut,na.rm = TRUE)
stat_E4[22,1] <- mean(q3$C4Cut,na.rm = TRUE); stat_E4[22,2] <- sd(q3$C4Cut,na.rm = TRUE)
stat_E4[23,1] <- mean(q3$C5Cut,na.rm = TRUE); stat_E4[23,2] <- sd(q3$C5Cut,na.rm = TRUE)
stat_E4[24,1] <- mean(q3$C6Cut,na.rm = TRUE); stat_E4[24,2] <- sd(q3$C6Cut,na.rm = TRUE)

stat_E4[19,3] <- mean(q3$C1Qua,na.rm = TRUE); stat_E4[19,4] <- sd(q3$C1Qua,na.rm = TRUE)
stat_E4[20,3] <- mean(q3$C2Qua,na.rm = TRUE); stat_E4[20,4] <- sd(q3$C2Qua,na.rm = TRUE)
stat_E4[21,3] <- mean(q3$C3Qua,na.rm = TRUE); stat_E4[21,4] <- sd(q3$C3Qua,na.rm = TRUE)
stat_E4[22,3] <- mean(q3$C4Qua,na.rm = TRUE); stat_E4[22,4] <- sd(q3$C4Qua,na.rm = TRUE)
stat_E4[23,3] <- mean(q3$C5Qua,na.rm = TRUE); stat_E4[23,4] <- sd(q3$C5Qua,na.rm = TRUE)
stat_E4[24,3] <- mean(q3$C6Qua,na.rm = TRUE); stat_E4[24,4] <- sd(q3$C6Qua,na.rm = TRUE)

stat_E4[19,5] <- mean(q3$d1,na.rm = TRUE); stat_E4[19,6] <- sd(q3$d1,na.rm = TRUE)
stat_E4[20,5] <- mean(q3$d2,na.rm = TRUE); stat_E4[20,6] <- sd(q3$d2,na.rm = TRUE)
stat_E4[21,5] <- mean(q3$d3,na.rm = TRUE); stat_E4[21,6] <- sd(q3$d3,na.rm = TRUE)
stat_E4[22,5] <- mean(q3$d4,na.rm = TRUE); stat_E4[22,6] <- sd(q3$d4,na.rm = TRUE)
stat_E4[23,5] <- mean(q3$d5,na.rm = TRUE); stat_E4[23,6] <- sd(q3$d5,na.rm = TRUE)
stat_E4[24,5] <- mean(q3$d6,na.rm = TRUE); stat_E4[24,6] <- sd(q3$d6,na.rm = TRUE)

# q4 stats
stat_E4[25,1] <- mean(q4$C1Cut,na.rm = TRUE); stat_E4[25,2] <- sd(q4$C1Cut,na.rm = TRUE)
stat_E4[26,1] <- mean(q4$C2Cut,na.rm = TRUE); stat_E4[26,2] <- sd(q4$C2Cut,na.rm = TRUE)
stat_E4[27,1] <- mean(q4$C3Cut,na.rm = TRUE); stat_E4[27,2] <- sd(q4$C3Cut,na.rm = TRUE)
stat_E4[28,1] <- mean(q4$C4Cut,na.rm = TRUE); stat_E4[28,2] <- sd(q4$C4Cut,na.rm = TRUE)
stat_E4[29,1] <- mean(q4$C5Cut,na.rm = TRUE); stat_E4[29,2] <- sd(q4$C5Cut,na.rm = TRUE)
stat_E4[30,1] <- mean(q4$C6Cut,na.rm = TRUE); stat_E4[30,2] <- sd(q4$C6Cut,na.rm = TRUE)

stat_E4[25,3] <- mean(q4$C1Qua,na.rm = TRUE); stat_E4[25,4] <- sd(q4$C1Qua,na.rm = TRUE)
stat_E4[26,3] <- mean(q4$C2Qua,na.rm = TRUE); stat_E4[26,4] <- sd(q4$C2Qua,na.rm = TRUE)
stat_E4[27,3] <- mean(q4$C3Qua,na.rm = TRUE); stat_E4[27,4] <- sd(q4$C3Qua,na.rm = TRUE)
stat_E4[28,3] <- mean(q4$C4Qua,na.rm = TRUE); stat_E4[28,4] <- sd(q4$C4Qua,na.rm = TRUE)
stat_E4[29,3] <- mean(q4$C5Qua,na.rm = TRUE); stat_E4[29,4] <- sd(q4$C5Qua,na.rm = TRUE)
stat_E4[30,3] <- mean(q4$C6Qua,na.rm = TRUE); stat_E4[30,4] <- sd(q4$C6Qua,na.rm = TRUE)

stat_E4[25,5] <- mean(q4$d1,na.rm = TRUE); stat_E4[25,6] <- sd(q4$d1,na.rm = TRUE)
stat_E4[26,5] <- mean(q4$d2,na.rm = TRUE); stat_E4[26,6] <- sd(q4$d2,na.rm = TRUE)
stat_E4[27,5] <- mean(q4$d3,na.rm = TRUE); stat_E4[27,6] <- sd(q4$d3,na.rm = TRUE)
stat_E4[28,5] <- mean(q4$d4,na.rm = TRUE); stat_E4[28,6] <- sd(q4$d4,na.rm = TRUE)
stat_E4[29,5] <- mean(q4$d5,na.rm = TRUE); stat_E4[29,6] <- sd(q4$d5,na.rm = TRUE)
stat_E4[30,5] <- mean(q4$d6,na.rm = TRUE); stat_E4[30,6] <- sd(q4$d6,na.rm = TRUE)

# answer for Exercise 4 is in stat_E4
View(stat_E4)
write.csv(stat_E4,'stat_for_exercise4.csv')

## Exercise 5
m4 <- matrix(0, nrow = nrow(school_list),ncol = 1)
school_list_selectivity <- cbind(school_list,m4)
remove(m4)

k1 <- quantile(school_list_selectivity$cutoff,probs = seq(0,1,0.1))
# decile line are 207,212,218,226,240,256,275,298,335,433
colnames(school_list_selectivity) <- c(colnames(school_list),'selectivity_c')

l <- nrow(school_list_selectivity)
for (i in 1:l){
  if (school_list_selectivity[i,6] <= 207) {
    school_list_selectivity[i,9] = 1
  } else if (school_list_selectivity[i,6] > 207 & school_list_selectivity[i,6] <= 212){
    school_list_selectivity[i,9] = 2
  } else if (school_list_selectivity[i,6] > 212 & school_list_selectivity[i,6] <= 218){
    school_list_selectivity[i,9] = 3
  } else if (school_list_selectivity[i,6] > 218 & school_list_selectivity[i,6] <= 226){
    school_list_selectivity[i,9] = 4
  } else if (school_list_selectivity[i,6] > 226 & school_list_selectivity[i,6] <= 240){
    school_list_selectivity[i,9] = 5
  } else if (school_list_selectivity[i,6] > 240 & school_list_selectivity[i,6] <= 256){
    school_list_selectivity[i,9] = 6
  } else if (school_list_selectivity[i,6] > 256 & school_list_selectivity[i,6] <= 275){
    school_list_selectivity[i,9] = 7
  } else if (school_list_selectivity[i,6] > 275 & school_list_selectivity[i,6] <= 298){
    school_list_selectivity[i,9] = 8
  } else if (school_list_selectivity[i,6] > 298 & school_list_selectivity[i,6] <= 335){
    school_list_selectivity[i,9] = 9
  } else {
    school_list_selectivity[i,9] = 10
  }
}

k2 <- quantile(school_list_selectivity$quality,probs = seq(0,1,0.1))
# decile line are 237.8141,245.2906,251.8420,258.6540,268.4597,281.5867,299.2483,319.7500,353.6483,444.9769 
m4 <- matrix(0, nrow = nrow(school_list),ncol = 1)
school_list_selectivity <- cbind(school_list_selectivity,m4)
remove(m4)
colnames(school_list_selectivity)[10] <- 'selectivity_q'

for (i in 1:l){
  if (school_list_selectivity[i,7] <= 237.8141) {
    school_list_selectivity[i,10] = 1
  } else if (school_list_selectivity[i,7] > 237.8141 & school_list_selectivity[i,7] <= 245.2906){
    school_list_selectivity[i,10] = 2
  } else if (school_list_selectivity[i,7] > 245.2906 & school_list_selectivity[i,7] <= 251.8420){
    school_list_selectivity[i,10] = 3
  } else if (school_list_selectivity[i,7] > 251.8420 & school_list_selectivity[i,7] <= 258.6540){
    school_list_selectivity[i,10] = 4
  } else if (school_list_selectivity[i,7] > 258.6540 & school_list_selectivity[i,7] <= 268.4597){
    school_list_selectivity[i,10] = 5
  } else if (school_list_selectivity[i,7] > 268.4597 & school_list_selectivity[i,7] <= 281.5867){
    school_list_selectivity[i,10] = 6
  } else if (school_list_selectivity[i,7] > 281.5867 & school_list_selectivity[i,7] <= 299.2483){
    school_list_selectivity[i,10] = 7
  } else if (school_list_selectivity[i,7] > 299.2483 & school_list_selectivity[i,7] <= 319.7500){
    school_list_selectivity[i,10] = 8
  } else if (school_list_selectivity[i,7] > 319.7500 & school_list_selectivity[i,7] <= 353.6483){
    school_list_selectivity[i,10] = 9
  } else {
    school_list_selectivity[i,10] = 10
  }
}

m5 <- matrix(0,nrow = nrow(student),ncol = 14)
colnames(m5) <- c('r1groupC','r2groupC','r3groupC','r4groupC','r5groupC','r6groupC','num_groups_by_Cutoff',
                  'r1groupQ','r2groupQ','r3groupQ','r4groupQ','r5groupQ','r6groupQ','num_groups_by_Quality')
student_selectivity <- cbind.data.frame(student,m5)

str(school_list_selectivity)
str(student_selectivity)

# change levels first
for (i in 11:16) {
  student_selectivity[,i] <- factor(student_selectivity[,i],levels = levels(school_list_selectivity$choicepgm))
}

str(school_list_selectivity)
str(student_selectivity)

for (i in 1:n){
  holder_selectivity1 <- school_list_selectivity[school_list_selectivity$schoolcode == student_selectivity[i,5] & school_list_selectivity$choicepgm == student_selectivity[i,11],]
  holder_selectivity2 <- school_list_selectivity[school_list_selectivity$schoolcode == student_selectivity[i,6] & school_list_selectivity$choicepgm == student_selectivity[i,12],]
  holder_selectivity3 <- school_list_selectivity[school_list_selectivity$schoolcode == student_selectivity[i,7] & school_list_selectivity$choicepgm == student_selectivity[i,13],]
  holder_selectivity4 <- school_list_selectivity[school_list_selectivity$schoolcode == student_selectivity[i,8] & school_list_selectivity$choicepgm == student_selectivity[i,14],]
  holder_selectivity5 <- school_list_selectivity[school_list_selectivity$schoolcode == student_selectivity[i,9] & school_list_selectivity$choicepgm == student_selectivity[i,15],]
  holder_selectivity6 <- school_list_selectivity[school_list_selectivity$schoolcode == student_selectivity[i,10] & school_list_selectivity$choicepgm == student_selectivity[i,16],]
  student_selectivity[i,19] <- holder_selectivity1[1,9];student_selectivity[i,26] <- holder_selectivity1[1,10]
  student_selectivity[i,20] <- holder_selectivity2[1,9];student_selectivity[i,27] <- holder_selectivity2[1,10]
  student_selectivity[i,21] <- holder_selectivity3[1,9];student_selectivity[i,28] <- holder_selectivity3[1,10]
  student_selectivity[i,22] <- holder_selectivity4[1,9];student_selectivity[i,29] <- holder_selectivity4[1,10]
  student_selectivity[i,23] <- holder_selectivity5[1,9];student_selectivity[i,30] <- holder_selectivity5[1,10]
  student_selectivity[i,24] <- holder_selectivity6[1,9];student_selectivity[i,31] <- holder_selectivity6[1,10]
  student_selectivity[i,25] <- length(unique(na.omit(as.numeric(student_selectivity[i,19:24]))))
  student_selectivity[i,32] <- length(unique(na.omit(as.numeric(student_selectivity[i,26:31]))))
}

# answer for exercise 5 is in student_selectivity
View(student_selectivity)
write.csv(student_selectivity,'student_selectivity.csv')

# saving space version, since github doesn't accept document more than 25MB
student_selectivity_shorter <- student_selectivity[,c(1,25,32)]
write.csv(student_selectivity_shorter,'Exercise_5_output.csv')
