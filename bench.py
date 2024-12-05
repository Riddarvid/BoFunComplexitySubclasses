import subprocess
import statistics

def singleRandomBenchmark(alg, funType, arity):
  result = subprocess.run(['stack', 'exec', 'exploring-level-p-exe', alg, funType, str(arity)], stdout=subprocess.PIPE)
  result.check_returncode()
  return result.stdout.decode('utf-8')

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
  return result.stdout.decode('utf-8')

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

flatFunTypes = ["GenFun", "CanonicalGenFun", "NormalizedGenFun", "BothGenFun", "ThresholdFun", "IterThresholdFun", "SymmetricFun", "IterSymmetricFun"]
iterFunTypes = ["GenFun", "CanonicalGenFun", "NormalizedGenFun", "BothGenFun", "IterThresholdFun", "IterSymmetricFun"]
hashableTypes = ["GenFun", "CanonicalGenFun", "NormalizedGenFun", "BothGenFun"]

#for ft in hashableTypes:
#  print(medianSpecific('explicitComplexity', ft, "iterMaj_3_2", 5))
#  print()

majNValsSuperSlow = [1, 3, 5, 7, 9, 11]
majNValsSlow = majNValsSuperSlow + [13, 15, 17]
majNValsMedium = majNValsSlow + [51, 101]
majNValsFast = majNValsMedium + [151, 201, 251, 301]

superSlowTypes = ["GenFun", "CanonicalGenFun"]
slowTypes = ["NormalizedGenFun", "BothGenFun"]
mediumTypes = ["IterThresholdFun", "IterSymmetricFun"]
fastTypes = ["SymmetricFun", "ThresholdFun"]


#print(medianSpecific('genAlg', "NormalizedGenFun", "maj_17", 5))

for alg in ["explicitComplexity"]:
  for n in majNValsSuperSlow:
    for ft in superSlowTypes:
      print(medianSpecific(alg, ft, f"maj_{n}", 5))
      print()

  for n in majNValsSlow:
    for ft in slowTypes:
      print(medianSpecific(alg, ft, f"maj_{n}", 5))
      print()

  # for n in majNValsMedium:
  #   for ft in mediumTypes:
  #     print(medianSpecific(alg, ft, f"maj_{n}", 5))
  #     print()

  # for n in majNValsFast:
  #   for ft in fastTypes:
  #     print(medianSpecific(alg, ft, f"maj_{n}", 5))
  #     print()