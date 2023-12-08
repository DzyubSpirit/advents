import sys

with open(sys.argv[1]) as f:
    lines = f.readlines()

sum = 0
for line in lines:
    digits = [ch for ch in line if ch >= '0' and ch <= '9']
    # digits = []
    # for ch in line:
    #   if ch >= '0' and ch <= '9':
    #     digits.append(ch)
    sum += int(digits[0])*10 + int(digits[-1])
print(sum)
