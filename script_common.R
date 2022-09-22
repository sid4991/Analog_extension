B <- data.frame(State = c(1), Score = c(1), num = c(1), denom = c(1))
A <- data.frame(State = c(1), Score = c(1), num = c(1), denom = c(1))
F_V_County_680 <- read.csv("~/GitHub/Data/F_V_County_680.csv")
County <- unique(F_V_County_680$State_County)

for (i in County) {
  data <- dplyr::filter(DataRCP85_NonLinearComparision_temp, Target == i)
  list_analog <- data$Analog
  print(length(list_analog))
  data1 <- dplyr::filter(DataRCP85_NonLinearComparision_derived, Target == i)
  list_analog1 <- data1$Analog
  print(length(list_analog1))
  list <- c(list_analog, list_analog1)
  num <- length(intersect(list_analog, list_analog1))
  denom <- length(unique(list))
  Score <- (round(num / denom, digits = 2))
  A$State <- i
  A$Score <- Score
  A$num <- num
  A$denom <- denom
  B <- rbind(A, B)

  B$Score <- ifelse(B$num == 0, 0, B$Score)

}
B <- B[-c(681),]

B <- dplyr::filter(B,B$denom>0)
