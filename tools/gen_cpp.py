
import sys

cppFileName, bufferName, bcFile = sys.argv[1:4]

if (sys.version_info > (3, 0)):
    cbyte = lambda x: x
else:
    cbyte = lambda x: ord(x)

def bytes_from_file(filename, chunksize=8192):
    with open(filename, "rb") as f:
        while True:
            chunk = f.read(chunksize)
            if chunk:
                for b in chunk:
                    yield cbyte(b)
            else:
                break

with open(cppFileName, 'w') as out:
    # prologue
    out.write('#include<string>\n')
    out.write('extern "C" const unsigned char %s_Buffer[] = {' % bufferName)

    for idx, byte in enumerate(bytes_from_file(bcFile)):
        # transcode file
        if idx > 0:
            out.write(',')
        if idx % 16 == 0:
            out.write('\n')

        out.write("0x{:02X}".format(byte))

    # epilogue
    out.write("\n")
    out.write("};\n")
    out.write('extern "C" const size_t %s_BufferLen = sizeof(%s_Buffer);\n' % (bufferName, bufferName))
