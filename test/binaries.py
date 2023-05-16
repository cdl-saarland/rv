from __future__ import print_function

import asyncio

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
    print("CMD {}".format(cmdText))
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

async def shellCmdAsync(cmdText, envModifier=None, logPrefix=None):
    if Debug:
      print("CMD {}".format(cmdText))
    processEnv=os.environ
    if envModifier:
        processEnv=dict(os.environ, **envModifier)
    cmd = shlex.split(cmdText)
    if logPrefix is None:
      # stdout
      proc = await asyncio.create_subprocess_shell(" ".join(cmd), env=processEnv)
    else:
      # write to log streams
      with open(logPrefix + ".out", "w") as fOut:
        with open(logPrefix + ".err", "w") as fErr:
          proc = await asyncio.create_subprocess_shell(" ".join(cmd), stdout=fOut, stderr=fErr, env=processEnv)

    retCode = await proc.wait()

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
    except subprocess.CalledProcessError as err:
        return False, err.output

rvToolLine="rvTool"

# use a 1.0 ULP error bound for SLEEF math
testULPBound = 10

def rvClang(clangArgs):
   return shellCmd(clangLine + " -fplugin=" + libRV + " -O3 " + clangArgs)


def primaryName(fileName):
    return path.basename(fileName).split(".")[0]

async def rvToolOuterLoop(scalarLL, destFile, scalarName = "foo", options = {}, logPrefix=None):
    baseName = primaryName(scalarLL)
    cmd = rvToolLine + " -loopvec -i " + scalarLL
    if destFile:
      cmd = cmd + " -o " + destFile
    if scalarName:
      cmd = cmd + " -k " + scalarName
    if options['loopHint']:
      cmd = cmd + " -l " + options['loopHint']
    if options['width']:
      cmd = cmd + " -w " + str(options['width'])
    if options["ulp_math_prec"]:
      cmd += " --math-prec {}".format(options["ulp_math_prec"])
    if 0 < len(options['extraShapes'].items()):
      cmd = cmd + " -x " + ",".join("{}={}".format(k,v) for k,v in options['extraShapes'].items())

    return await shellCmdAsync(cmd,  None, logPrefix)

async def rvToolWFV(scalarLL, destFile, scalarName = "foo", options = {}, logPrefix=None):
    cmd = rvToolLine + " -wfv -lower -i " + scalarLL
    if destFile:
      cmd = cmd + " -o " + destFile
    if scalarName:
      cmd = cmd + " -k " + scalarName + " -t " + scalarName + "_SIMD"
    if options['shapes']:
      cmd = cmd + " -s " + options['shapes']
    if options['width']:
      cmd = cmd + " -w " + str(options['width'])
    if options["ulp_math_prec"]:
      cmd += " --math-prec {}".format(options["ulp_math_prec"])
    if 0 < len(options['extraShapes'].items()):
      cmd = cmd + " -x " + ",".join("{}={}".format(k,v) for k,v in options['extraShapes'].items())

    cmd += " --math-prec {}".format(testULPBound)

    return await shellCmdAsync(cmd,  None, logPrefix)




############ Clang / LLVM tooling ############
class LLVMTools(object):
  def __init__(self, commonFlags=""):
    self.optcClangLine="clang -O3 -c -emit-llvm -S " + commonFlags
    self.optClangLine="clang++ -std=c++14 -O3 -c -emit-llvm -S "  + commonFlags
    self.clangLine="clang++ -std=c++14 -m64 -O2 -fno-vectorize -fno-slp-vectorize " + commonFlags
    self.cClangLine="clang -m64 -O2 -fno-vectorize -fno-slp-vectorize " + commonFlags


  def compileC(self, destFile, srcFiles, extraFlags=""):
    return 0 == shellCmd(self.cClangLine + " " + (" ".join(srcFiles)) + " " + extraFlags + " -o " + destFile)
  
  async def compileCPP(self, destFile, srcFiles, extraFlags=""):
    return 0 == await shellCmdAsync(self.clangLine + " " + (" ".join(srcFiles)) + " " + extraFlags + " -o " + destFile)
  
  
  async def optimizeIR(self, destFile, srcFile, extraFlags=""):
    return await shellCmdAsync(self.optClangLine + " " + srcFile + " -S -o " + destFile)
  
  def compileOptimized(self, srcFile, destFile, extraFlags=""):
    if not path.exists(srcFile):
      return False
  
    if srcFile[-2:] == ".c":
      retCode = shellCmd(self.optcClangLine + " " + srcFile + " " + extraFlags + " -o " + destFile)
    else:
      retCode = shellCmd(self.optClangLine + " " + srcFile + " " + extraFlags + " -o " + destFile)
    return retCode == 0

  def compileOptimized(self, srcFile, destFile, extraFlags=""):
    if not path.exists(srcFile):
      return False
  
    if srcFile[-2:] == ".c":
      retCode = shellCmd(self.optcClangLine + " " + srcFile + " " + extraFlags + " -o " + destFile)
    else:
      retCode = shellCmd(self.optClangLine + " " + srcFile + " " + extraFlags + " -o " + destFile)
    return retCode == 0
  
  
  
  async def compileToIR(self, srcFile, destFile, extraFlags=""):
    if not path.exists(srcFile):
      return False
  
    if srcFile[-2:] == ".c":
      retCode = await shellCmdAsync(self.cClangLine + " " + srcFile + " -fno-unroll-loops -S -emit-llvm -c " + extraFlags + " -o " + destFile)
    else:
      retCode = await shellCmdAsync(self.clangLine + " " + srcFile + " -fno-unroll-loops -S -emit-llvm -c " + extraFlags + "-o " + destFile)
    return retCode == 0
  
  def disassemble(self, bcFile, suffix):
      return shellCmd("llvm-dis " + bcFile, "logs/dis_" + suffix) == 0
      
  def build_launcher(self, launcherBin, launcherLL, fooFile, suffix):
      return shellCmd(self.clangLine + " -fno-slp-vectorize -o " + launcherBin + " " + launcherLL + " " + fooFile, "logs/clang-launcher_" + suffix) == 0
  
  def assemble(self, fooFile, destAsm, suffix):
      return shellCmd(self.clangLine + " -fno-slp-vectorize -c -S -o " + destAsm + " " + fooFile, "logs/clang-asm_" + suffix) == 0
