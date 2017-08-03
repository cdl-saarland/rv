#!/usr/bin/env python3
#
#===- test_rv.py ---------------------------------------------------===//
#
#                     The Region Vectorizer
#
# This file is distributed under the University of Illinois Open Source
# License. See LICENSE.TXT for details.
#
# @authors simon
#



from glob import glob
from binaries import *
from os import path
import csv
import time

numSamples = 15

patterns=None
profileMode=False
if len(sys.argv) > 1:
  if sys.argv[1] == "-p":
    profileMode=True
    startArg = 2
  else:
    startArg = 1
  patterns = sys.argv[startArg:]

if patterns is None or len(patterns) == 0:
  patterns = ["suite/*.c*"]


def profileTest(profileMode, numSamples, func):
  if not profileMode:
    return func()

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


def wholeFunctionVectorize(srcFile, argMappings, width):
  baseName = path.basename(srcFile).split(".")[0]
  destFile = "build/" + baseName + ".wfv.ll"
  logPrefix =  "logs/"  + baseName + ".wfv"
  scalarName = "foo"
  ret = runWFV(srcFile, destFile, scalarName, argMappings, width, logPrefix)
  return destFile if ret == 0 else None

def outerLoopVectorize(srcFile, loopDesc, width):
  baseName = path.basename(srcFile).split(".")[0]
  destFile = "build/" + baseName + ".loopvec.ll"
  logPrefix =  "logs/"  + baseName + ".loopvec"
  scalarName = "foo"
  ret = runOuterLoopVec(srcFile, destFile, scalarName, loopDesc, logPrefix, width)
  return destFile if ret == 0 else None

def executeWFVTest(scalarLL, options, profileMode):
  sigInfo = options.split(",")

  width = 8
  for option in sigInfo:
    opSplit = option.split(":")
    if opSplit[0].strip() == "LaunchCode":
      launchCode = opSplit[1].strip()
    elif opSplit[0].strip() == "Shapes":
      shapes = opSplit[1].strip()
    elif opSplit[0].strip() == "Width":
      width = int(opSplit[1].strip())

  testBC = wholeFunctionVectorize(scalarLL, shapes, width)
  if testBC:
    result = profileTest(profileMode, numSamples, lambda: runWFVTest(testBC, launchCode, profileMode))
    if profileMode:
      speedup = result
      return 1.0, speedup # because code below expects runtimes and (spedup / 1.0 == speedup)
    else:
      return result
  else:
    return None, None if profileMode else False

def executeOuterLoopTest(scalarLL, options, profileMode):
  sigInfo = options.split(",")
  # launchCode = options.split("-k")[1].split("-")[0].strip()

  width = 8

  for option in sigInfo:
    opSplit = option.split(":")
    if opSplit[0].strip() == "LaunchCode":
      launchCode = opSplit[1].strip()
    elif opSplit[0].strip() == "LoopHint":
      loopHint = opSplit[1].strip()
    elif opSplit[0].strip() == "Width":
      width = int(opSplit[1].strip())

  # create RV-vectorizer version
  vectorIR = outerLoopVectorize(scalarLL, loopHint, width)
  if vectorIR is None:
    return (None, None) if profileMode else False

  # create OR reference version
  optLL = scalarLL[:-2] + "opt.ll"
  ret = optimizeIR(optLL, scalarLL)
  assert ret == 0

  scalarRes = profileTest(profileMode, numSamples, lambda: runOuterLoopTest(optLL, launchCode, "scalar", profileMode))
  vectorRes = profileTest(profileMode, numSamples, lambda: runOuterLoopTest(vectorIR, launchCode, "loopvec", profileMode))

  if scalarRes is None or vectorRes is None:
    return (None, None) if profileMode else False 

  if profileMode:
    return vectorRes, scalarRes
  else:
    return scalarRes == vectorRes


print("-- RV tester {}--".format("(profile mode) " if profileMode else ""))

for pattern in patterns:
  tests = [testCase for testCase in glob(pattern)]
  tests.sort()
  results = [["Test", "Speedup"]]
  for testCase in tests:
    baseName = path.basename(testCase)

    if profileMode:
      os.environ["NAT_STAT_DUMP"] = baseName.split(".")[0].split("-")[0] + ".csv"

    with open("suite/" + baseName, 'r') as f:
      options = f.readline().strip("//").strip("\n").strip()

    print("{:60}".format("- {}".format(baseName)), end="")
    parts = baseName.split(".")[0].split("-")
    mode = parts[-1]
    rest = "-".join(parts[0:-1])

    scalarLL = buildScalarIR(testCase)

    if mode == "wfv":
      result = executeWFVTest(scalarLL, options, profileMode)
    elif mode == "loop":
      result = executeOuterLoopTest(scalarLL, options, profileMode)

    if profileMode:
      rvTime, defTime = result
      success = not (rvTime is None or defTime is None)
      print("{:5.3f}".format(defTime / rvTime) if success else "failed!")
      results.append([baseName, "{:5.3f}".format(defTime / rvTime) if success else "0"])

    else:
      success = result
      print("passed" if success else "failed! <-")

  if profileMode:
    with open(time.strftime("%Y-%m-%d-%H:%M:%S") + ".csv", 'w') as f:
      writer = csv.writer(f)
      writer.writerows(results)
