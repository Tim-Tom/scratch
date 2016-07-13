import sys, time

while True:
    line = sys.stdin.readline()
    while line != "":
        sys.stdout.write(line)
        line = sys.stdin.readline()
        time.sleep(2)
