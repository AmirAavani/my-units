import sys;
import fileinput

AnswerFile = open(sys.argv[1])
tmp = AnswerFile.read().split()
solution = [0] * (len(tmp) + 1)
for i in range(len(tmp)):
  solution[i + 1] = int(tmp[i])
#print solution
while True:
  S = raw_input()
  if S == "done":
      break
  S += ','
  S = S.split(',')
  n = 0l
  P2 = 1l
  for si in S:
      if si == '':
          continue
      t = ''
      sign = +1
      for c in si:
          if c == '(':
              continue
          if c == ')':
              continue
          if c == '~':
              sign = -1
              continue
          if c != 'x':
              t += c
      if t == '':
          break
      it = int(t)
      print 'si, sign, it, solution[it]'
      print si, sign, it, solution[it]
      if (0 < solution[it]) and (0 < sign):
        print 1,
        n += P2
      elif (solution[it] < 0) and (sign < 0):
        print 1,
        n += P2
      else:
        print 0,
      P2 *= 2
  print n
#  sindex = int(S[0])
#  if len(S) == 1:
#      S.append(S[0])
#  eindex = int(S[1])
#  n = 0
#  p2 = 1
#  for i in range(eindex - sindex + 1):
#      n *= 2;
#      print eindex - i,
#      if 0 < solution[eindex - i]:
#        n += 1 
#  print
#  print solution[sindex:eindex+1], n
  
  

