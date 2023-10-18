myData2 <- data.frame(
  person = c("Stan", "Francine", "Steve", "Roger", "Heyley", "Klaus"),
  sex = factor(c("M","F","M","M","F","M")),
  funny = factor(c("High","Med","Low","High","Med","Med"))
)

newRecord2 <- data.frame(
  age = c(41,41,15,1600,21,60)
)

myData2 <- cbind(myData2,newRecord2)
