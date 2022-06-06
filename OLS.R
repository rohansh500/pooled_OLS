# Loading Libraries

library(readxl)

# Creating Dataframe

columns <- c("term","sat","tothrs","cumgpa","season","frstsem","crsgpa","verbmath","trmgpa","hssize","hsrank","id","spring","female","black","white","ctrmgpa","ctothrs","ccrsgpa","ccrspop","cseason","hsperc","football")
D <- read_excel("C:/Users/ROHAN SHARMA/Downloads/GPA3.xls",col_names=FALSE)
colnames(D) <- columns

# Part a
#Defing functions

function1 <- trmgpa~spring+sat+hsperc+female+black+white+frstsem+tothrs+crsgpa+season

# Extracting summary

model1=lm(formula=function1,data=D)
summary(model1)

#Part b
#For the fall semester
D2 = subset(D,term == 1)
model2 = lm(formula = function1,data = D2)
summary(model2)

#For the spring semester
D3 = subset(D,term == 2)
model3 = lm(formula = function1,data = D3)
summary(model3)


#Part c
#Defining functions

deltatrmgpa <- diff(D$trmgpa)
deltafrstsem <- diff(D$frstsem)
deltatothrs <- diff(D$tothrs)
deltacrsgpa <- diff(D$crsgpa)
deltaseason <- diff(D$season)

deltatrmgpa <- c(deltatrmgpa[seq(length(deltafrstsem))%%2 == 1])
deltafrstsem <- c(deltafrstsem[seq(length(deltafrstsem))%%2 == 1])
deltatothrs <- c(deltatothrs[seq(length(deltafrstsem))%%2 == 1])
deltacrsgpa <- c(deltacrsgpa[seq(length(deltafrstsem))%%2 == 1])
deltaseason <- c(deltaseason[seq(length(deltafrstsem))%%2 == 1])

function2 <- deltatrmgpa~deltatothrs+deltacrsgpa+deltafrstsem+deltaseason
D4 = data.frame(deltatrmgpa,deltafrstsem,deltatothrs,deltacrsgpa,deltaseason)

#Extracting summary

model4 = lm(formula = function2,data = D4)
summary(model4)
