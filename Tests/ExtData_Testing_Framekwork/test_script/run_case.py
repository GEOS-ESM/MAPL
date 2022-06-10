#!/usr/bin/env python
  
import argparse, sys, os
import subprocess as sp
import glob
import shutil
import utils

class ExtDataCase():
    """
    """

    def __init__(self, case_name, comm_line_args):

        self.build_dir = comm_line_args['build_dir']
        self.case_dir = comm_line_args['case_dir']
        self.case_name = case_name
        self.case_path = self.case_dir+"/"+self.case_name.rstrip()

    def run(self,logfile):
        
        scrdir="ExtData_scratch"
        orig_dir = os.getcwd()
        if os.path.isdir(scrdir):
           shutil.rmtree(scrdir)
        os.mkdir(scrdir)
        rc_files = glob.glob(self.case_path+"/*.rc")
        for rc_file in rc_files:
            shutil.copy(rc_file,scrdir)
        yaml_files = glob.glob(self.case_path+"/*.yaml")
        for yaml_file in yaml_files:
            shutil.copy(yaml_file,scrdir)
        os.chdir(scrdir)

        g5_mod_path = self.build_dir+"/g5_modules"
        utils.source_g5_modules(g5_mod_path)
        success = os.path.isfile('nproc.rc')
        if success:
           fproc = open('nproc.rc',"r")
           nproc = fproc.readline()
           nproc = nproc.rstrip()
           fproc.close()
        else:
           nproc = "1"

        exec_path = "cat CAP1.rc " + self.case_dir+"/use_extdata2g.rc > temp.rc ;mv temp.rc CAP1.rc"
        sp.call(exec_path,stdout=logfile,stderr=logfile,shell=True)
        exec_path = "cat CAP2.rc " + self.case_dir+"/use_extdata2g.rc > temp.rc; mv temp.rc CAP2.rc"
        sp.call(exec_path,stdout=logfile,stderr=logfile,shell=True)

        exec_path = "mpirun -np "+nproc+" "+self.build_dir+"/ExtDataDriver.x "
        sp.call(exec_path,stdout=logfile,stderr=logfile,shell=True)
        sp.call("~/bin/Killall ExtDataDriver.x",stdout=logfile,stderr=logfile,shell=True)

        print("finished exec of "+self.case_name.rstrip())
        success = os.path.isfile('egress')
        os.chdir(orig_dir)
        shutil.rmtree(scrdir)
#
        if success:
           return True
        else:
           return False
        
