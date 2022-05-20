#!/usr/bin/env python3
import tempfile as tf
import shutil as sh
import os.path as path
import os
import unittest as ut


ACG = 'MAPL_GridCompSpecs_ACG.py'
spec_file = 'TestSpecs.rc'
CONTROL = '.CONTROL'
outputs = { '-i': 'IMPORT', '-x': 'EXPORT', '-p': 'INTERNAL', '-g': 'GET_POINTERS', '-d': 'DECLARE_POINTERS' }
controls = dict(map(lambda s: (s, outputs[s] + CONTROL), outputs))

#class TestMAPL_ACG(ut.TestCase):
#
#    def setUp(self):
#        
#    def test_Test(self):
#        self.assertTrue(True)

if __name__ == '__main__':
    with tf.TemporaryDirectory() as testdirname:
        for f in [ACG, spec_file] + list(controls.values()):
            sh.copy(f, testdirname)
        os.chdir(testdirname)
#        os.system('./' + ACG)
        print(os.listdir())
#        ut.main()
