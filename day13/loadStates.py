import sys

with open(sys.argv[1], 'r') as f:
    txt = f.read()

def diff(a1, a2):
    return [(i, t) for i, t in enumerate(zip(a1, a2)) if t[0] != t[1]]

lines = txt.split('\n')[:-1]
pcs = [int(l.split(' ')[0]) for l in lines]
relBases = [int(l.split(' ')[1]) for l in lines]
mems = [eval(l.split(' ')[2]) for l in lines]
