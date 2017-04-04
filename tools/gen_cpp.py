#! /usr/bin/python3
import sys

cppFileName = sys.argv[1]
bcFile = sys.argv[2]

stream = open(bcFile, 'br')
with open(cppFileName, 'w') as out:
  # prologue
  out.write("const unsigned char * buffer = (unsigned char[]){") 

  # transcode file
  later = False

  while True:
    byte = stream.read(1)
    if not byte:
      raise SystemExit

    if later:
      out.write(',')

    later = True
    out.write("{}".format(byte[0]))

  # epilogue
  out.write("};\n")
