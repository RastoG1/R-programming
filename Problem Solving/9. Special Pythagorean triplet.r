# A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
# 
# a^2 + b^2 = c^2
# For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
# 
# There exists exactly one Pythagorean triplet for which a + b + c = 1000.
# Find the product abc.

check.integer <- function(N){
  !grepl("[^[:digit:]]", format(N,  digits = 20, scientific = FALSE))
}

a = c(1:999)
b = c(1:999)
i = 1
is_1000 = FALSE

while(!is_1000){
  for(j in b){
    if(check.integer(sqrt(a[i]^2 + b[j]^2))){
      c = sqrt(a[i]^2 + b[j]^2)
      if(a[i] + b[j] + c == 1000){
        is_1000 = TRUE
        prod = a[i]*b[j]*c
      }
    }
  }
  i = i + 1
}
print(paste('Answer #9: ', prod))












