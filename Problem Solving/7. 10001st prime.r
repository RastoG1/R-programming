# By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see 
#that the 6th prime is 13.
# What is the 10 001st prime number?


primes = c(2)
i = primes
while (length(primes) < 10001) {
  i <- i + 1
  if (all(i %% primes != 0))
    primes = append(primes, i)
   
}

print(paste('Answer 7: ', primes[10001]))

