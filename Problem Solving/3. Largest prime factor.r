# The prime factors of 13195 are 5, 7, 13 and 29.
# 
# What is the largest prime factor of the number 600851475143 ?

lpf = 1
N = 600851475143
y = N

for (i in 2:as.integer(sqrt(N))+ 1){
  while(y %% i == 0){
    y = y / i
    lpf = i
  }
if(y == 1)
  break
}
print(paste('Answer #3: ', lpf))

#or this way
print('---or in this easier way---')
install.packages('schoolmath')
library(schoolmath)
max(prime.factor(N))

