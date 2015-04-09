import math

def log2(n):
  return math.log(n) / math.log(2)

def GenerateModulos(n, all_primes, TwoToPs):
  logn = int(log2(n))
  N = (1 << n) - 1
  M = 1l
  for i in range(logn):
    M *= TwoToPs[i]
  j = 0
  for i in range(logn, len(TwoToPs)):
    M *= TwoToPs[i] 
    if M % TwoToPs[j] == 0:
      M = M / TwoToPs[j]
      j += 1
    else:
      assert(False)
#    print M, all_primes[i]
    if N < M:
      return [n, logn, i, all_primes[i], TwoToPs[i], N, M]
  print n, logn, TwoToPs[i], all_primes[i], N, M
  return []


def GeneratePrimes(m):
  L = [True] * (m + 1);
  L[0] = False 
  L[1] = False 
  L[2] = True
  L[3] = True
  for i in range(2, m + 1):
    if L[i]:
      j = 2 * i
      while j <= m:
        L[j] = False
        j += i
    elif m < i * i:
      break

  result = []
  for i in range(2, m + 1):
    if L[i]:
      result.append(i)

  return result

all_primes = GeneratePrimes(100000) 
TwoToPs = []
for p in all_primes:
  TwoToPs.append((1 << p) - 1)
#print all_primes
n = int(raw_input())
#while True:
L = GenerateModulos(n, all_primes, TwoToPs)
if L == []:
  pass
else:
  print L
n += 10
