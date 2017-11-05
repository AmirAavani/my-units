import sys

def Eval(data, vector):
  result = []
  value = 0
  P2 =  1
  for vs in vector:
    negated = False
    if vs[0] == '~':
      negated = True
      vs = vs[1:]
    vs = vs[1:]
    v = int(vs)

    if (data[v - 1][0] == '-') ^ negated:
      result.append(0)
    else:
      result.append(1)
      value = value + P2
    P2 = P2 * 2

  return [value, result];

def main():
  ans_filename = sys.argv[1]
  data = ''
  with open (ans_filename, "r") as ans_file:
    data = ans_file.readlines()[1]
  data = data.split(' ')
 
  for i in range(len(sys.argv) - 2):
    print(i)
    vector = sys.argv[i + 2]
    vector = vector[1:-2]
    
    vector = vector.split(',')
#    print(vector)
    print(Eval(data, vector));

if  __name__ =='__main__':main()
