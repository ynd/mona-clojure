# Clojure: 10.55348
# Java: 2.58524 sans / 12.02628 avec color creation

import os, sys, commands, re
from itertools import repeat

def get_run_time(cmd):
  return float(re.findall(r"0m(.*?)s", commands.getoutput("time %s"% cmd))[0])

def average(l):
  return sum(l) / len(l)

def benchmark(cmd):
  return average(map(get_run_time, repeat(cmd, 25)))

os.chdir("clojure")
print "Clojure:", benchmark("java -cp ../../clojure.jar clojure.lang.Repl fitness.clj")

os.chdir("../java")
print "Java:", benchmark("java Fitness")