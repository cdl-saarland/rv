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

if len(sys.argv) > 1:
  patterns = sys.argv[1:]
else:
  patterns = ["suite/*.c*"]

def wholeFunctionVectorize(srcFile, argMappings):
  baseName = path.basename(srcFile)
  destFile = "build/" + baseName + ".wfv.ll"
  logPrefix =  "logs/"  + baseName + ".wfv"
  scalarName = "foo"
  ret = runWFV(srcFile, destFile, scalarName, argMappings, logPrefix)
  return destFile if ret == 0 else None

def outerLoopVectorize(srcFile, loopDesc):
  baseName = path.basename(srcFile)
  destFile = "build/" + baseName + ".loopvec.ll"
  logPrefix =  "logs/"  + baseName + ".loopvec"
  scalarName = "foo"
  ret = runOuterLoopVec(srcFile, destFile, scalarName, loopDesc, logPrefix)
  return destFile if ret == 0 else None

def executeWFVTest(scalarLL, options):
  sigInfo = options.split(",")

  for option in sigInfo:
    opSplit = option.split(":")
    if opSplit[0].strip() == "LaunchCode":
      launchCode = opSplit[1].strip()
    elif opSplit[0].strip() == "Shapes":
      shapes = opSplit[1].strip()

  testBC = wholeFunctionVectorize(scalarLL, shapes)
  return runWFVTest(testBC, launchCode) if testBC else False

def executeOuterLoopTest(scalarLL, options):
  sigInfo = options.split(",")
  # launchCode = options.split("-k")[1].split("-")[0].strip()

  for option in sigInfo:
    opSplit = option.split(":")
    if opSplit[0].strip() == "LaunchCode":
      launchCode = opSplit[1].strip()
    elif opSplit[0].strip() == "LoopHint":
      loopHint = opSplit[1].strip()

  vectorIR = outerLoopVectorize(scalarLL, loopHint)
  if vectorIR is None:
    return False

  scalarRes = runOuterLoopTest(scalarLL, launchCode, "scalar")
  vectorRes = runOuterLoopTest(vectorIR, launchCode, "loopvec")

  if scalarRes is None or vectorRes is None:
    return False

  return scalarRes == vectorRes


print("-- RV tester --")
for pattern in patterns:
  tests = [testCase for testCase in glob(pattern)]
  tests.sort()
  for testCase in tests:
    baseName = path.basename(testCase)

    with open("suite/" + baseName, 'r') as f:
      options = f.readline().strip("//").strip("\n").strip()

    print("{:60}".format("- {}".format(baseName)), end="")
    parts = baseName.split(".")[0].split("-")
    mode = parts[-1]
    rest = "-".join(parts[0:-1])

    scalarLL = buildScalarIR(testCase)

    if mode == "wfv":
      success = executeWFVTest(scalarLL, options)
    elif mode == "loop":
      success = executeOuterLoopTest(scalarLL, options)
    print("passed!" if success else "failed!")



