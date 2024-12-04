import subprocess
import statistics

def singleRandomBenchmark(alg, funType, arity):
  result = subprocess.run(['stack', 'exec', 'exploring-level-p-exe', alg, funType, str(arity)], stdout=subprocess.PIPE)
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
    #print(result)
  return results

def medianSpecific(alg, funType, fun, sampleSize):
  return statistics.median(multipleSpecificBenchmark(alg, funType, fun, sampleSize))

flatFunTypes = ["GenFun", "CanonicalGenFun", "NormalizedGenFun", "BothGenFun", "ThresholdFun", "IterThresholdFun", "SymmetricFun", "IterSymmetricFun"]
iterFunTypes = ["GenFun", "CanonicalGenFun", "NormalizedGenFun", "BothGenFun", "IterThresholdFun", "IterSymmetricFun"]

for ft in iterFunTypes:
  print(medianSpecific('genAlg', ft, "iterMaj_3_2", 5))
  print()