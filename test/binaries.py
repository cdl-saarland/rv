#===- binaries.py ---------------------------------------------------===//
#
#                     The Region Vectorizer
#
# This file is distributed under the University of Illinois Open Source
# License. See LICENSE.TXT for details.
#
# @authors simon
#

import os
from os import path
import shlex, subprocess, sys, errno

# set-up workspace
Debug=int(os.getenv("RVT_DEBUG", 0)) != 0

libRV="../lib/libRV.so"

def assureDir(path):
  try:
    os.makedirs(path)
  except OSError as exception:
    if exception.errno != errno.EEXIST:
      raise

assureDir("logs")
assureDir("build")

def shellCmd(cmdText, envModifier=None, logPrefix=None):
    if Debug:
      print("CMD {}".format(cmdText))
    processEnv=os.environ
    if envModifier:
        processEnv=dict(os.environ, **envModifier)
    cmd = shlex.split(cmdText)
    if logPrefix is None:
      # stdout
      proc = subprocess.Popen(cmd, env=processEnv)
    else:
      # write to log streams
      with open(logPrefix + ".out", "w") as fOut:
        with open(logPrefix + ".err", "w") as fErr:   
          proc = subprocess.Popen(cmd, stdout=fOut, stderr=fErr, env=processEnv)
    retCode=proc.wait()
    if retCode != 0:
        print("")
        print(cmdText)
    return retCode

def runForOutput(cmdText):
    if Debug:
      print("CMD {}".format(cmdText))
    cmd = shlex.split(cmdText)
    try:
        return True, subprocess.check_output(cmd, stderr=subprocess.STDOUT)
    except CalledProcessError as err:
        return False, err.output

optClangLine="clang -march=native -O3 -c -emit-llvm -S  " # -fno-slp-vectorize"

clangLine="clang++ -std=c++14 -march=native -m64 -O2 -fno-vectorize" # -fno-slp-vectorize"
cClangLine="clang -march=native -m64 -O2 -fno-vectorize -fno-slp-vectorize"

rvToolLine="rvTool"

def rvClang(clangArgs):
   return shellCmd(clangLine + " -Xclang -load -Xclang " + libRV + " -O3 " + clangArgs)


def plainName(fileName):
    return os.path.basename(fileName).split(".")[0]

def buildScalarIR(srcFile):
    baseName = plainName(srcFile)
    scalarLL = "build/" + baseName + ".ll"
    compileToIR(srcFile, scalarLL)
    return scalarLL

def runOuterLoopVec(scalarLL, destFile, scalarName = "foo", loopDesc=None, logPrefix=None, width=None):
    baseName = plainName(scalarLL)
    cmd = rvToolLine + " -loopvec -i " + scalarLL
    if destFile:
      cmd = cmd + " -o " + destFile
    if scalarName:
      cmd = cmd + " -k " + scalarName
    if loopDesc:
      cmd = cmd + " -l " + loopDesc
    if width:
      cmd = cmd + " -w " + str(width)

    return shellCmd(cmd,  None, logPrefix)

def runWFV(scalarLL, destFile, scalarName = "foo", shapes=None, width=None, logPrefix=None, extraShapes=dict()):
    cmd = rvToolLine + " -wfv -lower -i " + scalarLL
    if destFile:
      cmd = cmd + " -o " + destFile
    if scalarName:
      cmd = cmd + " -k " + scalarName
    if shapes:
      cmd = cmd + " -s " + shapes
    if width:
      cmd = cmd + " -w " + str(width)
    if 0 < len(extraShapes.items()):
      cmd = cmd + " -x " + ",".join("{}={}".format(k,v) for k,v in extraShapes.items())

    return shellCmd(cmd,  None, logPrefix)

launcherCache = set()

def requestLauncher(launchCode, prefix):
    launcherCpp = "launcher/" + prefix + "_" + launchCode + ".cpp"
    if "profile" in prefix: # profileMode
      launcherLL= "build/" + prefix + "_" + launchCode + ".ll"
    else:
      launcherLL= "build/" + prefix + "_" + launchCode + ".ll"

    if launcherLL in launcherCache:
      return launcherLL
    else:
      success = compileToIR(launcherCpp, launcherLL)
      if not success:
        return None
      launcherCache.add(launcherLL)
    return launcherLL

def runWFVTest(testBC, launchCode, profileMode):
    shellCmd(clangLine + " " + testBC + " -c -S -o " + testBC + ".s")
    try:
      caseName = plainName(testBC)
      if profileMode:
        launcherLL = requestLauncher(launchCode, "profile")
        if launcherLL is None:
          print("(could not build launcher {}) ".format(launchCode), end = "")
          return None
        launcherBin = "./build/profile_" + caseName + ".bin"
        shellCmd(clangLine + " " + testBC + " " + launcherLL + " -o " + launcherBin)
        success, result = runForOutput(launcherBin)
        if not success:
          return None
        else:
          return float(result)
      else:
        launcherLL = requestLauncher(launchCode, "verify")
        launcherBin = "./build/verify_" + caseName + ".bin"
        shellCmd(clangLine + " " + testBC + " " + launcherLL + " -o " + launcherBin)
        retCode = shellCmd(launcherBin)
        return retCode == 0
    except:
      return False

def runOuterLoopTest(testBC, launchCode, suffix, profileMode=False):
  modeText = "profile" if profileMode else "verify"
  try:
    caseName = plainName(testBC)
    launcherLL = requestLauncher(launchCode, "loop" + modeText)
    launcherBin = "./build/" + modeText + "_" + caseName + "." + suffix + ".bin"
    runForOutput(clangLine + " " + testBC + " " + launcherLL + " -o " + launcherBin)

    success, result = runForOutput(launcherBin)

    if profileMode:
      return float(result) if success else None
    else:
      return result if success else None

  except:
      return None

def optimizeIR(destFile, srcFile):
  return shellCmd(optClangLine + " " + srcFile + " -S -o " + destFile)

def compileToIR(srcFile, destFile):
  if not path.exists(srcFile):
    return False

  if srcFile[-2:] == ".c":
    retCode = shellCmd(cClangLine + " " + srcFile + " -fno-unroll-loops -S -emit-llvm -c -o " + destFile)
  else:
    retCode = shellCmd(clangLine + " " + srcFile + " -fno-unroll-loops -S -emit-llvm -c -o " + destFile)
  return retCode == 0

def disassemble(bcFile,suffix):
    return shellCmd("llvm-dis " + bcFile, "logs/dis_" + suffix) == 0
    
def build_launcher(launcherBin, launcherLL, fooFile, suffix):
    return shellCmd(clangLine + " -fno-slp-vectorize -o " + launcherBin + " " + launcherLL + " " + fooFile, "logs/clang-launcher_" + suffix) == 0

def assemble(fooFile, destAsm, suffix):
    return shellCmd(clangLine + " -fno-slp-vectorize -c -S -o " + destAsm + " " + fooFile, "logs/clang-asm_" + suffix) == 0
