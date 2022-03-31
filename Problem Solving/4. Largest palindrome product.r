# A palindromic number reads the same both ways. 
#The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
# Find the largest palindrome made from the product of two 3-digit numbers.
install.packages('stringi')
library(stringi)

palindrome = function(x) stri_reverse(x)==x

p_values = c()

for (i in 500:999) {
  for(j in 500:999){
    z = toString(i*j)
    if(palindrome(z)){
      p_values = append(p_values, z)
    }
  }
}
p = strtoi(p_values)
print(paste('Answer #4: ', max(p)))
