#!/usr/bin/env python
  
import argparse, sys, os
import subprocess as sp
from run_case import ExtDataCase

def parse_comm_args():

    p = argparse.ArgumentParser(description='Run ExtData tester script')

    # BAS_HOM_DIR, CUR_HOM_DIR, DIFF
    # ------------------------------
    p.add_argument("--builddir",  dest="build_dir",help='src directory for build')
    p.add_argument("--casedir",  dest="case_dir",help='where cases are located')
    p.add_argument("--cases",  dest="cases",help='list of cases')
    p.add_argument("--savelog",dest="save_log",default="false",help='save the log files for all')


    args = vars(p.parse_args()) # vars converts to dict

    # some checks on inputs
    # ---------------------
    if not os.path.isdir(args['build_dir']):
        raise Exception('build_dir [%s] does not exist' % args['bas'])
    if not os.path.isdir(args['case_dir']):
        raise Exception('case_dir [%s] does not exist' % args['bas'])

    # return opts
    # -----------
    return args


if __name__ == "__main__":

    comm_opts = parse_comm_args()
    build_dir = comm_opts['build_dir']
    case_dir = comm_opts['case_dir']
    case_path = comm_opts['cases']
    case_file = open(case_path,'r')
    lines =case_file.readlines()
    case_file.close()
    for case in lines:
       if '#' not in case:
          print("running "+case.rstrip())
          this_case = ExtDataCase(case,comm_opts)
          logfile=case.rstrip()+".log"
          log = open(logfile,'w')
          success = this_case.run(log)
          log.close()
          if success:
             print(case.rstrip()+" passed")
             if comm_opts['save_log'].lower() == "false":
                os.remove(logfile)
          else:
             print(case.rstrip()+" failed")

