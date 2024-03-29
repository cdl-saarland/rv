# -*- Python -*-

# Add local python packages
import site

# Configuration file for the 'lit' test runner.

import os
import subprocess

# name: The name of this test suite.
config.name = 'RV-Functional'

# suffixes: A list of file extensions to treat as test files.
config.suffixes = []

# is_early; Request to run this suite early.
config.is_early = True

# Increase max test time for test execution.
# FIXME: Debug builds will need more time
config.maxIndividualTestTime = 30

# test_source_root: The root path where tests are located.
# test_exec_root: The root path where tests should be run.
config.test_exec_root = os.path.join(config.rv_src_root, 'test')
config.test_source_root = config.test_exec_root

# Load and instantiate custom test format.
site.addsitedir(config.test_source_root)
import rv
config.test_format = rv.rvtest.RVTest('suite')

# Propagate the temp directory. Windows requires this because it uses \Windows\
# if none of these are present.
if 'TMP' in os.environ:
    config.environment['TMP'] = os.environ['TMP']
if 'TEMP' in os.environ:
    config.environment['TEMP'] = os.environ['TEMP']

# Propagate HOME as it can be used to override incorrect homedir in passwd
# that causes the tests to fail.
if 'HOME' in os.environ:
    config.environment['HOME'] = os.environ['HOME']

# Propagate path to symbolizer for ASan/MSan.
for symbolizer in ['ASAN_SYMBOLIZER_PATH', 'MSAN_SYMBOLIZER_PATH']:
    if symbolizer in os.environ:
        config.environment[symbolizer] = os.environ[symbolizer]

# Win32 seeks DLLs along %PATH%.
if sys.platform in ['win32', 'cygwin'] and os.path.isdir(config.shlibdir):
    config.environment['PATH'] = os.path.pathsep.join((
            config.shlibdir, config.environment['PATH']))

# Win32 may use %SYSTEMDRIVE% during file system shell operations, so propogate.
if sys.platform == 'win32' and 'SYSTEMDRIVE' in os.environ:
    config.environment['SYSTEMDRIVE'] = os.environ['SYSTEMDRIVE']
