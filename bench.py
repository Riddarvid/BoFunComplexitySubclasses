# Python script for running measurements

import subprocess
import statistics
from numpy import percentile
import numpy as np

def singleRandomBenchmark(alg, funType, arity):
  result = subprocess.run(['stack', 'exec', 'exploring-level-p-exe', alg, funType, str(arity)], stdout=subprocess.PIPE)
  result.check_returncode()
  return float(result.stdout.decode('utf-8'))

def multipleRandomBenchmark(alg, fun, arity, sampleSize):
  results = []
  for i in range(sampleSize):
    result = singleRandomBenchmark(alg, fun, arity)
    results.append(result)
    print(result)
  return results

def singleSpecificBenchmark(alg, funType, fun):
  result = subprocess.run(['stack', 'exec', 'exploring-level-p-exe', alg, funType, fun], stdout=subprocess.PIPE)
  result.check_returncode()
  return float(result.stdout.decode('utf-8'))

def multipleSpecificBenchmark(alg, funType, fun, sampleSize):
  print(f"Alg: {alg}")
  print(f"Function type: {funType}")
  print(f"Function: {fun}")
  print(f"Sample size: {sampleSize}")
  results = []
  for i in range(sampleSize):
    result = singleSpecificBenchmark(alg, funType, fun)
    results.append(result)
  return results

def medianSpecific(alg, funType, fun, sampleSize):
  return statistics.median(multipleSpecificBenchmark(alg, funType, fun, sampleSize))

def measureSample(alg, funType, funStr):
  result = subprocess.run(['stack', 'exec', 'exploring-level-p-exe', alg, funType, funStr], stdout=subprocess.PIPE)
  result.check_returncode()
  return float(result.stdout.decode('utf-8'))

def measureCompleteSample(filePath, alg, funType, arity, sampleSize):
  sample = readSample(filePath, funType, arity, sampleSize)
  results = []
  for fun in sample:
    result = measureSample(alg, funType, fun)
    results.append(result)
  return results

def readSample(filePath, funType, arity, sampleSize):
  with open(filePath) as f:
    sample = f.readlines()
  i = 0
  while i < len(sample) and not sample[i].startswith(f"{funType}, {arity}"):
    i += 1
  sample = sample[i+1:i+1+sampleSize]
  return sample

def fiveValueFromSample(filePath, alg, funType, arity, sampleSize):
  results = np.array(measureCompleteSample(filePath, alg, funType, arity, sampleSize))
  quartiles = percentile(results, [25, 50, 75])
  resultMin = results.min()
  resultMax = results.max()
  return (resultMin.item(), quartiles[0].item(), quartiles[1].item(), quartiles[2].item(), resultMax.item())

def measureThresholdFun(nt, nf):
  result = subprocess.run(['stack', 'exec', 'exploring-level-p-exe', str(nt), str(nf)], stdout=subprocess.PIPE)
  result.check_returncode()
  return float(result.stdout.decode('utf-8'))

def measureNbitThresholdfun(n):
  for nt in range(n + 1):
    nf = n + 1 - nt
    result = measureThresholdFun(nt, nf)
    print(f"{nt} {result}")

flatFunTypes = ["GenFun", "CanonicalGenFun", "NormalizedGenFun", "BothGenFun", "ThresholdFun", "IterThresholdFun", "SymmetricFun", "IterSymmetricFun"]
iterFunTypes = ["GenFun", "CanonicalGenFun", "NormalizedGenFun", "BothGenFun", "IterThresholdFun", "IterSymmetricFun"]
hashableTypes = ["GenFun", "CanonicalGenFun", "NormalizedGenFun", "BothGenFun"]

#for ft in hashableTypes:
#  print(medianSpecific('explicitComplexity', ft, "iterMaj_3_2", 5))
#  print()

# majNValsSuperSlow = [1, 3, 5, 7, 9, 11]
# majNValsSlow = majNValsSuperSlow + [13, 15, 17]
# majNValsMedium = majNValsSlow + [51, 101]
# majNValsFast = majNValsMedium + [151, 201, 251, 301]

# superSlowTypes = ["GenFun", "CanonicalGenFun"]
# slowTypes = ["NormalizedGenFun", "BothGenFun"]
# mediumTypes = ["IterThresholdFun", "IterSymmetricFun"]
# fastTypes = ["SymmetricFun", "ThresholdFun"]

# print(fiveValueFromSample('./data/samples.dat', 'genAlg', 'GenFun', 5, 100))

# for alg in ["complexity"]:
#   for ft in ["GenFun", "CanonicalGenFun", "NormalizedGenFun", "BothGenFun"]:
#     for arity in [5]:
#       print(f"Alg: {alg}")
#       print(f"Function type: {ft}")
#       print(f"Arity: {arity}")
#       print(fiveValueFromSample('./data/samples.dat', alg, ft, arity, 100))
#       print()
#   for ft in ["ThresholdFun", "SymmetricFun"]:
#     for arity in [10, 15, 150]:
#       print(f"Alg: {alg}")
#       print(f"Function type: {ft}")
#       print(f"Arity: {arity}")
#       print(fiveValueFromSample('./data/samples.dat', alg, ft, arity, 100))
#       print()
#   for ft in ["IterThresholdFun"]:
#     for arity in [10, 15]:
#       print(f"Alg: {alg}")
#       print(f"Function type: {ft}")
#       print(f"Arity: {arity}")
#       print(fiveValueFromSample('./data/samples.dat', alg, ft, arity, 100))
#       print()
#   for ft in ["IterSymmetricFun"]:
#     for arity in [5]:
#       print(f"Alg: {alg}")
#       print(f"Function type: {ft}")
#       print(f"Arity: {arity}")
#       print(fiveValueFromSample('./data/samples.dat', alg, ft, arity, 100))
#       print()

# for alg in ["explicitComplexity"]:
#   for ft in ["GenFun", "CanonicalGenFun", "NormalizedGenFun", "BothGenFun"]:
#     for arity in [5]:
#       print(f"Alg: {alg}")
#       print(f"Function type: {ft}")
#       print(f"Arity: {arity}")
#       print(fiveValueFromSample('./data/samples.dat', alg, ft, arity, 100))
#       print()

measureNbitThresholdfun(200)