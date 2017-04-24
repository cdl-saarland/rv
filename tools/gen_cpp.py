#! /usr/bin/env python3
import sys

cppFileName = sys.argv[1]
bufferName = sys.argv[2]
bcFile = sys.argv[3]


def encodeByte(byte):
  return "{}".format(byte)

  if byte == '\\':
    return "\\\\"
  elif byte == '\"':
    return "\\\""
  elif byte == '\n':
    return "\\n"
  elif byte == '\t':
    return "\\t"
  else:
    return byte


stream = open(bcFile, 'br')
with open(cppFileName, 'w') as out:
  # prologue
  out.write("#include<string>\nextern const unsigned char {}_Buffer[] = {}".format(bufferName, "{")) 

  # transcode file
  later = False

  while True:
    byte = stream.read(1)
    if not byte:
      break

    if later:
      out.write(',')

    later = True
    out.write("0x{:02X}".format(byte[0]))

  # epilogue
  out.write("{0};\nextern const size_t {1}_BufferLen = sizeof({1}_Buffer);\n".format("}", bufferName))
