from __future__ import absolute_import
import os
import shlex
import subprocess
import sys
import glob

import lit.Test
import lit.TestRunner
import lit.util
from lit.formats.base import TestFormat

kIsWindows = sys.platform in ['win32', 'cygwin']

class RVTest(TestFormat):
    def __init__(self, suite_dir):
        self.suite_dir = suite_dir

    def getAbsoluteTestDir(self, localConfig):
        return os.path.join(localConfig.rv_src_root, "test")

    def getAbsoluteSuiteDir(self, localConfig):
        rv_test_root = self.getAbsoluteTestDir(localConfig)
        return os.path.join(rv_test_root, self.suite_dir)

    def getTestCases(self, litConfig, localConfig):
        rv_test_root = os.path.join(localConfig.rv_src_root, "test")
        abs_suite_dir = self.getAbsoluteSuiteDir(localConfig)

        # Collect relative CPP file names.
        #  TODO: Reuse lit functions here:
        #     for fn in lit.util.listdir_files(dir_path,
        #                                      suffixes=self.test_suffixes):
        for abs_case_path in glob.glob("{}/*.cpp".format(abs_suite_dir)):
          rel_case_path = os.path.relpath(abs_case_path, abs_suite_dir)
          yield rel_case_path

    # LIT interface function.
    def getTestsInDirectory(self, testSuite, path_in_suite,
                            litConfig, localConfig):
        # TODO: Find a better way to skip subfolders
        if len(path_in_suite) != 0:
          return
        source_path = testSuite.getSourcePath(path_in_suite)
        for rv_test_case in self.getTestCases(litConfig, localConfig):
          test_path = path_in_suite + (self.suite_dir, rv_test_case)
          yield lit.Test.Test(testSuite, test_path, localConfig,
                              file_path=test_path)

    # LIT interface function.
    def execute(self, test, litConfig):
        abs_suite_dir = self.getAbsoluteSuiteDir(test.config)
        rel_case_path = os.path.relpath(test.getSourcePath(), abs_suite_dir)

        cmd = ["python3", "./test_rv.py", os.path.join(self.suite_dir, rel_case_path)]

        if litConfig.noExecute:
            return lit.Test.PASS, ''

        header = f"Script:\n--\n{' '.join(cmd)}\n--\n"

        llvm_bin_dir = os.path.join(test.config.llvm_obj_root, 'bin')
        exec_env = test.config.environment
        exec_env['PATH'] = os.path.pathsep.join((llvm_bin_dir, exec_env['PATH']))

        try:
            out, err, exitCode = lit.util.executeCommand(
                cmd, env=exec_env,
                cwd=self.getAbsoluteTestDir(test.config),
                timeout=litConfig.maxIndividualTestTime)
        except lit.util.ExecuteCommandTimeoutException:
            return (lit.Test.TIMEOUT,
                    f'{header}Reached timeout of '
                    f'{litConfig.maxIndividualTestTime} seconds')

        if exitCode:
            return lit.Test.FAIL, header + out + err
        passing_test_line = 'passed'
        if passing_test_line not in out:
            return (lit.Test.UNRESOLVED,
                    f'{header}Unable to find {passing_test_line} '
                    f'in ./test_rv output:\n\n{out}{err}')
  
        return lit.Test.PASS,''
