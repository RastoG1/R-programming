# 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 
# without any remainder.
# What is the smallest positive number that is evenly divisible by all of the numbers
# from 1 to 20?

prime_nums_u20 = c(2, 3, 5, 7, 11, 13, 17, 19)
nums = 2:20

a = 1
z = 1

for(i in prime_nums_u20){
  while(a < max(nums)){
    a = a*i
  }
  a = a / i
  print(a)
  z = z*a
  a = 1
}

print(paste('Answer #5: ', z))






# 232792560
