#!/usr/bin/env python3
from __future__ import print_function

import asyncio

from glob import glob
from binaries import *
from os import path
import csv
import time
import re

numSamples = int(os.getenv("NUM_SAMPLES", 15))
defaultVectorWidth = 8

def printTestLineOptions():
  text = """
--- Test case options --
The test case specified on the first line of the test case file. Syntax:
  \\\\ Key: Value, Key2: Value2
-- Supported Keys --
LaunchCode: <launcherName>
Shapes: <ArgumentShapes>[r<ReturnValueShape] // WFV only
Width: <vectorizationFactor>
ULPMathPrec: <ULPError*10> // ULP error bound on math functions (in 10*ULP)
VarShape[<GlobalVariable>]=<Shape> // Assign shape <Shape> to value <GlobalVariable>
"""
  print(text)

class TestCase:
  def getFilename(self, filetype):
    primaryName = self.baseName.split(".")[0]
    if filetype == 'base':
      return self.baseName
    elif filetype == 'primary':
      return primaryName
    elif filetype == 'csv':
      return  primaryName.split("-")[0] + ".csv"
    elif filetype == 'scalarLL':
      return "build/" + primaryName + ".ll"
    elif filetype == 'wfvLL':
      return "build/" + primaryName + ".wfv.ll"
    elif filetype == 'wfvLogPrefix':
      return "logs/" + primaryName + ".wfv"
    elif filetype == 'loopLL':
      return "build/" + primaryName + ".loopvec.ll"
    elif filetype == 'loopLogPrefix':
      return "logs/" + primaryName + ".loopvec"

  def __init__(self, testfile):
    self.srcFile = testfile
    self.baseName = path.basename(testfile)
    self.mode = self.getFilename('primary').split("-")[-1]
    self.options = {}

  def parseOptions(self):
    with open("suite/" + self.baseName, 'r') as f:
      srcOptions = f.readline().strip("//").strip("\n").strip()
    sigInfo = srcOptions.split(",")
    self.options['launchCode'] = None
    self.options['shapes'] = None
    self.options['ulp_math_prec'] = 10

    self.options['extraShapes'] = dict()

    # default outer loop stencil
    self.options['width'] = 8 if self.mode == 'loop' else None
    self.options['loopHint'] = 0

    for option in sigInfo:
      opSplit = option.split(":")
      if not opSplit or len(opSplit) != 2:
        print("(ill-formed header option entry: {})".format(option))
        return None, None

      lhsPart = opSplit[0].strip()
      rhsPart = opSplit[1].strip()

      if lhsPart == "LaunchCode":
        self.options['launchCode'] = rhsPart
      if lhsPart == "LoopHint":
        self.options['loopHint'] = rhsPart
      elif lhsPart == "Shapes":
        self.options['shapes'] = rhsPart
      elif lhsPart == "Width":
        self.options['width'] = int(rhsPart)
      elif lhsPart == "ULPMathPrec":
        self.options['ulp_math_prec'] = int(rhsPart)
      else:
        namedMatch = re.search("\[(.*)\]", option)
        if not namedMatch is None:
          mStart = namedMatch.span()[0]
          optName = option[:mStart]
          keyName = namedMatch.groups()[0]
          self.options['extraShapes'][keyName] = rhsPart

  def requestLauncher(self, prefix, profileMode):
    launcherCpp = "launcher/" + prefix + "_" + self.options['launchCode'] + ".cpp"
    return (launcherCpp, "-Ilauncher/include")


def runOuterLoopTester(scaLauncherBin, vecLauncherBin, profileMode):
  scalarSuccess, rawScalarRes = runForOutput(scaLauncherBin)
  scalarRes = rawScalarRes.decode('utf-8') if rawScalarRes else ""

  if not scalarSuccess:
    raise TestFailure("scalar launcher crashed! Output:\n{}".format(scalarRes), None)

  vecSuccess, rawVecRes = runForOutput(vecLauncherBin)

  vecRes = rawVecRes.decode('utf-8') if rawVecRes else ""

  if not vecSuccess or vecRes is None:
    raise TestFailure("vector launcher crashed! Output:\n{}".format(vecRes), None)

  if profileMode:
    return float(vecRes), float(scalarRes)
  else:
    return scalarRes == vecRes


def runWFVTester(launcherBin, profileMode):
  success, rawResult = runForOutput(launcherBin)

  result = rawResult.decode('utf-8') if rawResult else ""

  if not success:
    raise TestFailure("wfv launcher crashed! Output:\n{}".format(result), None)

  if profileMode:
    speedup = float(result)
    return 1.0, speedup # because code below expects runtimes and (spedup / 1.0 == speedup)
  else:
    return success

### test case failures ###
rvToolReason="failed in rvTool"
launcherReason="could not build launcher"
rvTestGenReason="failed in rvTestGen"

# silent failure if a certain feature is not supported by the target (toolchain)
class Unsupported(Exception):
    def __init__(self, reason):
      self.reason = reason

    def __str__(self):
      return "Unsupported: {}".format(self.reason)

class TestFailure(Exception):
    def __init__(self, reason, logPrefix):
      self.reason = reason
      self.logPrefix = logPrefix

    def __str__(self):
      if self.logPrefix:
        return "{} (logs {})".format(self.reason, self.logPrefix)
      else:
        return self.reason


class Toolchain(object):

    # @returns a runner for this test case (eg lambda function that returns a result on invocation)
    async def buildTestRunner(self, testCase, profileMode):
        return None


# pure clang toolchain for the host target
class HostClangToolchain(Toolchain):
    def __init__(self):
      self.clang = LLVMTools("-march=native -Iinclude -Wno-unused-command-line-argument")

    async def buildWFVTester(self, testCase, profileMode):
      destFile = testCase.getFilename('wfvLL')
      logPrefix = testCase.getFilename('wfvLogPrefix') + ".rvTool"
      scalarName = "foo"
      ret = await rvToolWFV(testCase.getFilename('scalarLL'), destFile,
          scalarName, testCase.options, logPrefix)
      if ret != 0:
          raise TestFailure(rvToolReason, logPrefix)

      testBC = testCase.getFilename('wfvLL') 
      if testBC is None:
        raise TestFailure("testBC", "")
    
      prefix = "profile" if profileMode else "verify"
      launcherCpp, launcherCXXFlags = testCase.requestLauncher(prefix, profileMode)

      caseName = primaryName(testBC)
      launcherBin = "./build/" + prefix + "_" + caseName + ".bin"
      ok = await self.clang.compileCPP(launcherBin, [testBC, launcherCpp], launcherCXXFlags) 
      if not ok:
        raise TestFailure(launcherReason, None)

      return lambda: runWFVTester(launcherBin, profileMode)


    async def buildOuterLoopTester(self, testCase, profileMode):
      prefix = "loopprofile" if profileMode else "loopverify"

      scalarLL = testCase.getFilename('scalarLL')
      vectorizedLL =  testCase.getFilename('loopLL')
      logPrefix = testCase.getFilename('loopLogPrefix') + ".rvTool"
      scalarName = "foo"
      ret = await rvToolOuterLoop(scalarLL, vectorizedLL, scalarName, testCase.options, logPrefix)
      if 0 != ret: raise TestFailure(rvToolReason, logPrefix)
    
      optScalarLL = scalarLL[:-2] + "opt.ll"
      ret = await self.clang.optimizeIR(optScalarLL, scalarLL, "")
      if 0 != ret: raise TestFailure("optimizeIR failed", None)
    
      launcherCpp, launcherCXXFlags = testCase.requestLauncher(prefix, profileMode)

      caseName = primaryName(testCase.srcFile)

      # build launcher binaries
      vecLauncherBin = "./build/" + prefix + "_" + caseName + ".rv.bin"
      ok = await self.clang.compileCPP(vecLauncherBin, [vectorizedLL, launcherCpp], launcherCXXFlags) # clangLine + " " + testBC + " " + launcherLL + " -o " + launcherBin)
      if not ok:
          raise TestFailure("compileCPP for vectorizedLL+launcher", None)

      scaLauncherBin = "./build/" + prefix + "_" + caseName + ".scalar.bin"
      ok = await self.clang.compileCPP(scaLauncherBin, [scalarLL, launcherCpp], launcherCXXFlags) # clangLine + " " + testBC + " " + launcherLL + " -o " + launcherBin)
      if not ok:
          raise TestFailure("compileCPP for scalarLL+launcher", None)

      # create runner
      return lambda: runOuterLoopTester(scaLauncherBin, vecLauncherBin, profileMode)
    

    async def buildTestRunner(self, testCase, profileMode):
      scalarLL = testCase.getFilename('scalarLL')
      await self.clang.compileToIR(testCase.srcFile, scalarLL)

      if testCase.mode == "wfv":
        return await self.buildWFVTester(testCase, profileMode)
      elif testCase.mode == "loop":
        return await self.buildOuterLoopTester(testCase, profileMode)




# rv+clang toolchain with nc++ for kernel compilation


################## TOOL CHAIN SELECTION #################
def printToolChains():
  text = """\
Supported toolchains (default is ve):
 clang           rvTool+Clang tool chain for the host machine (-march=native).
"""
  print(text)

def selectToolChain(toolChainName):
  if toolChainName == "clang":
    return HostClangToolchain()
  else:
    raise Unsupported("Invalid toolchain: {}".format(toolChainName))




################## COMMAND LINE PARSING ################

def printBanner():
  print("---------------------- RV integrated tester ----------------------")

def printRule():
  print("------------------------------------------------------------------")

def printHelp():
  text = """\
  ./test_rv [-p][-t <toolChain>][-h] ...test case patterns...

Options:
<none>            run all tests in suite/ in verification mode.
 -h               print help text and exit.
 -t <toolchain>   run test cases with the selected toolchain.
 -p               run in profile mode.

Environment variables:
 RVT_DEBUG=1      dump all shell commands before they are run.
 NUM_SAMPLES=<n>  take median of <n> samples when in profile mode.

"""
  print(text)
  printToolChains()
  printTestLineOptions()

# rv integrated tester banner
printBanner()

# parse command line args
toolChainName="clang"
patterns=None
profileMode=False
startArg = 1
while startArg < len(sys.argv):
    if sys.argv[startArg] == "-p":
      profileMode = True
      startArg += 1

    elif sys.argv[startArg] == "-t":
      if startArg + 1 >= len(sys.argv):
        print("Expected -t <toolChainName>")
        raise SystemExit(-1)
      toolChainName = sys.argv[startArg + 1]
      startArg += 2

    elif sys.argv[startArg] == "-h":
      printHelp()
      raise SystemExit

    else:
      break

# parse test case nameas
patterns = sys.argv[startArg:]

if patterns is None or len(patterns) == 0:
  patterns = ["suite/*.c*"]



def profileTest(numSamples, func):
  samples = []
  for i in range(numSamples):
    s = func()
    if s is None:
      return None
    samples.append(s)

  if None in samples:
    return None

  resList = sorted(samples)
  return resList[len(resList) // 2]




# configure tool chain
try:
  toolchain = selectToolChain(toolChainName)
except Unsupported as err:
  print(err)
  raise SystemExit(-1)


# print run info
print("Toolchain: {}".format(toolChainName))
if profileMode:
  print("Profile mode ({} samples)".format(numSamples))
else:
  print("Test mode".format(numSamples))

printRule()



# run stuff
AllPassed = True

async def build_test(test, profileMode):
  try:
    runner = await toolchain.buildTestRunner(test, profileMode)
    print("{:60}".format("- {}".format(test.baseName)), end="")
    print("built")
    return runner
  except TestFailure as err:
    print("{:60}".format("- {}".format(test.baseName)), end="")
    print("ERROR: {}".format(err))
    return None
  except Unsupported as err:
    print("{:60}".format("- {}".format(test.baseName)), end="")
    print("({})".format(err))
    return None

async def build_tests():
  test_runners_promise = []

  for test in test_cases:
    runner = build_test(test, profileMode)
    test_runners_promise.append(runner)

  runners_awaited = await asyncio.gather(*test_runners_promise)

  for runner in runners_awaited:
    test_runners.append(runner)

for pattern in patterns:
  tests = [testCase for testCase in glob(pattern)]
  tests.sort()

  test_cases = [TestCase(test) for test in tests]
  test_runners = []

  results = [["Test", "Speedup"]]
  for test in test_cases:
    # parse test case line ("// A: x, B: y .." line in test source file)
    test.parseOptions()

  asyncio.run(build_tests())

  num_tests = len(test_cases)
  num_run_tests = 0
  num_success_tests = 0

  for test, runner in zip(test_cases, test_runners):
    if runner is None:
        continue

    if profileMode:
      os.environ["NAT_STAT_DUMP"] = test.getFilename('csv')

    print("{:60}".format("- {}".format(test.baseName)), end="")

    num_run_tests += 1

    # run the test
    try:
      if profileMode:
        rvTime, defTime = profileTest(numSamples, runner)
        success = not (rvTime is None or defTime is None)
        if success:
            num_success_tests += 1
        print("{:5.3f}".format(float(defTime) / rvTime) if success else "failed!")
        results.append([test.baseName, "{:5.3f}".format(defTime / rvTime) if success else "0"])

      else:
        success = runner()
        if success:
            num_success_tests += 1
        print("passed" if success else "failed! <-")

    except TestFailure as testFail:
      AllPassed = False
      print("failed! {}".format(testFail))

  print("{} of {} tests run; {} ok, {} failed".format(num_run_tests, num_tests, num_success_tests, num_run_tests - num_success_tests))


# flush out results numbers
if profileMode:
  with open(time.strftime("%Y-%m-%d-%H:%M:%S") + ".csv", 'w') as f:
    writer = csv.writer(f)
    writer.writerows(results) 

# Goodbye
printRule()
if AllPassed:
  raise SystemExit(0)
else:
  raise SystemExit(1)
