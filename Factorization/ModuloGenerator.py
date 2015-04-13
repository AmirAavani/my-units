import math

def log2(n):
  return math.log(n) / math.log(2)

def GenerateModulos3(n, all_primes, TwoToPs):
  logn = int(log2(n) + 0.9999)
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

def GenerateModulos4(n, all_primes, TwoToPs):
  logn = int(log2(n) + 0.9999)
  N = (1 << n) - 1

  top = n / 2
  bot = 1
  result = []
  MinP = []

  while bot <= top:
    M = 1l
    mid = (bot + top) / 2

    P = []
    for i in range(logn):
      p = all_primes[i]
      if  mid <= p:
        print "-mid = ", mid
        bot = mid + 1
        break
      
      while p * all_primes[i] < mid:
        p *= all_primes[i]
      P.append(p)
      M *= ((1 << p) - 1)

    if N < M:
      print "+mid = ", mid, 'P = ', P
      result = [n, logn, p, mid, N, M]
      MinP = P
      top = mid - 1
    else:
      print "-mid = ", mid
      bot = mid + 1

  print 'n, logn, max_p^i, mid, N, M', result
  print 'MinP = ', MinP
  return result


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
L = GenerateModulos3(n, all_primes, TwoToPs)
if L == []:
  pass
else:
  print L

L = GenerateModulos4(n, all_primes, TwoToPs)
if L == []:
  pass
else:
  print L
n += 10
