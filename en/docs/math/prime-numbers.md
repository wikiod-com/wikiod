---
title: "prime numbers"
slug: "prime-numbers"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Prime factorization
Example implementation of a prime factorization algorithm. A prime factorization algorithm will find for a given number `n` a list of primes, such that if you multiply those primes you get `n`. The below implementation will add `-1` to the list of prime factors in case `n < 0`. Note that there exists no prime factorization of `0`, therefore the method below returns an empty list.

    List<Integer> primeFactors(int n) {
        List<Integer> factors = new ArrayList<>();
        if (n < 0) {
            factors.add(-1);
            n *= -1;
        }
        for (int i = 2; i <= n / i; ++i) {
            while (n % i == 0) {
                factors.add(i);
                n /= i ;
            }
        }
        if (n > 1) {
            factors.add(n);
        }
        return factors ;
    }

## Prime checking
Please note that by definition `1` is not a prime number. To check if a number `n` is a prime we should try to find a divisor `i` of `n`.  If we cannot, then `n` is a prime. We have found a divisor when `n % i == 0` evaluates to true. We only need to try odd numbers, since there's only one even prime, namely `2`, which we'll treat as a special case. Additionally, only the numbers up to and including `sqrt(n)` are possible divisors, because when `n = a * b` then at least one of the factors is at most `sqrt(n)`.

To check whether or not a number is a prime number the following algorithm can be used:

    boolean isPrime (int n) {
        if (n < 2) {
            return false;
        }
        if (n % 2 == 0) {
            return n == 2;
        }
        for (int i = 3; i*i <= n; i += 2) {
            if (n % i == 0) {
                return false;
            }
        }
        return true ;
    }

## Prime sieve

The Sieve of Eratosthenes generates all the primes from 2 to a given number *n*.

 1. Assume that all numbers (from 2 to *n*) are prime. 
 2. Then take the first prime number and removes all of its multiples. 
 3. Iterate the step 2 for next prime. Continue until all numbers till *n* have been marked.


**Pseudocode:**

    Input: integer n > 1
     
    Let A be an array of Boolean values, indexed by integers 1 to n,
    initially all set to true.
     
    for i = 2, 3, 4, ..., not exceeding sqrt(n):
        if A[i] is true:
            for j = 2i, 3i, 4i, 5i, ..., not exceeding n :
                A[j] := false
     
    Output: all i such that A[i] is true.

---

**C code**:


    void PrimeSieve(int n)
    {
        int *prime;
        prime = malloc(n * sizeof(int));
            int i;
            for (i = 2; i <= n; i++)
                prime[i] = 1;    
        
           
        int p;
        for (p = 2; p * p <= n; p++)
        {    
            if (prime[p] == 1) // a Prime found
            {
                // mark all multiples of p as not prime.
                for (int i = p * 2; i <= n; i += p)
                    prime[i] = 0;
            }
        }
     
        // print prime numbers
        for (p = 2; p <= n; p++)
            if (prime[p] == 1)
                printf("%d ", p);
    }


