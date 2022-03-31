# the sum of the squares of the first ten natural numbers is,
# 1^2 + 2^2 + ..... + 10^2 = 385
# The square of the sum of the first ten natural numbers is,
# (1+2+....+10)^2 = 3025
# Hence the difference between the sum of the squares of the first ten natural
# numbers and the square of the sum is .
# 3025-385 = 2640
# Find the difference between the sum of the squares of the first one hundred
# natural numbers and the square of the sum.

sum0 = 0
sum1 = 0

for(i in 1:100){
  sum0 = sum0 + i^2
  sum1 = sum1 + i
}
print(paste('Answer #6: ', sum1^2 - sum0))