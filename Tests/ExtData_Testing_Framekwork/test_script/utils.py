#!/usr/bin/env python

"""
# ------------------------------------------------------------------------------
# collection of useful functions:
#
#      writemsg
#      get_hostname
#      source_g5_modules
# ------------------------------------------------------------------------------
"""


import os
import sys
import glob
import time
import shutil
import errno
import fnmatch
import subprocess as sp
import filecmp
import shlex
import distutils.spawn
import subprocess
import re

def writemsg(str2write, fout=None, quiet=None):
    """
    # --------------------------------------------------------------------------
    # write message to fout
    #
    # Inputs:
    #    str2write: (obvious)
    #         fout: handle of (open) output file, if None, set to sys.stdout
    # --------------------------------------------------------------------------
    """    
    if not fout: fout = sys.stdout
    if not quiet: fout.write('%s' % str2write); fout.flush()



def get_hostname():
    """
    # --------------------------------------------------------------------------
    # Return the hostname (DISCOVER, PLEIADES)
    # --------------------------------------------------------------------------
    """

    node = os.uname()[1]
    if node[0:8]=='discover' or node[0:4]=='borg':
        HOST = 'DISCOVER'
    elif node[0:3]=='pfe' or node[0:4]=='maia' or (node[0]=='r' and node[4]=='i'):
        HOST = 'PLEIADES'
    elif node[-13:]=='gsfc.nasa.gov' or (node[:6]=='gs6101' 
            and (node[-12:]=='ndc.nasa.gov') or node[-5:]=='local'):
        HOST = 'DESKTOP'
        # MAT Note that the DESKTOP is a "failover" if it is gsfc
        #     we return DESKTOP if it matches nothing else
    else:
        HOST = 'DESKTOP'
        #raise Exception('could not get host name from node [%s]' % node)

    return HOST

def source_g5_modules(g5_modules, fout=None):
    """
    #---------------------------------------------------------------------------
    # def source_g5_modules(g5_modules, fout):
    #
    # source_g5_modules is a wrapper for the csh script g5_modules. It
    # queries the csh script for basedir, modules and modinit, adds basedir
    # to os.environ and loads library modules
    # 
    # Input:
    #    g5_modules: full path of g5_modules
    #          fout: handle of (open) log file, if None - set to sys.stdout
    #---------------------------------------------------------------------------
    """

    if not fout: fout = sys.stdout

    # check if g5_modules exists
    # --------------------------
    if not os.path.isfile(g5_modules):
        raise Exception('g5_modules does not exist')


    # part of the command to run
    # --------------------------
    cmd = ['/bin/csh', g5_modules]

    # query for basedir
    # -----------------
    run = sp.Popen(cmd+['basedir'], stdout=sp.PIPE, stderr=sp.PIPE)
    output = run.communicate()
    rtrnCode = run.wait()
    if rtrnCode != 0:
        print('0:'); print(output[0]); print('1:'); print(output[1])
        raise Exception('cant query g5_modules for basedir')
    #BASEDIR = output[0].strip()
    BASEDIR = output[0].split('\n')[0].strip()


    # query for modules to load
    # -------------------------
    run = sp.Popen(cmd+['modules'], stdout=sp.PIPE, stderr=sp.PIPE)
    output = run.communicate()
    rtrnCode = run.wait()
    if rtrnCode != 0:
        print('0:'); print(output[0]); print('1:'); print(output[1])
        raise Exception('cant query g5_modules for modules')
    #MODULES = output[0].strip().split()
    MODULES = output[0].split('\n')[0].strip().split()

    #print("MATMAT MODULES: ", MODULES)


    # query for modinit
    # -----------------
    run = sp.Popen(cmd+['modinit'], stdout=sp.PIPE, stderr=sp.PIPE)
    output = run.communicate()
    rtrnCode = run.wait()
    if rtrnCode != 0:
        print('0:'); print(output[0]); print('1:'); print(output[1])
        raise Exception('cant query g5_modules for modinit')
    # MODINIT = output[0].strip().replace('csh', 'python')
    # For Matt, modinit query results in '/usr/share/modules/init/csh\n/usr/..'
    tmpdir = output[0].split('\n')[0].strip()
    newdir = tmpdir.split('/')
    HOST = get_hostname()
    # MAT On anvil, at least, the modules has python.py
    if HOST=='PLEIADES' or HOST=='DESKTOP':
        newdir[-1] = 'python.py'
    else:
        newdir[-1] = 'python'
    MODINIT = '/'.join(newdir)

    # set BASEDIR
    # -----------
    ARCH = os.uname()[0]
    writemsg(' %s: Setting BASEDIR' % os.path.basename(g5_modules), fout)
    os.environ['BASEDIR'] = BASEDIR # this only modifies the local environment
    BASELIB = '%s/%s/lib' % (BASEDIR, ARCH)
    if 'LD_LIBRARY_PATH' in os.environ:
        os.environ['LD_LIBRARY_PATH'] += os.pathsep + BASELIB
    else:
        os.environ['LD_LIBRARY_PATH'] = BASELIB


    # load library modules
    # --------------------
    if (os.path.isfile(MODINIT)):
        writemsg(' and modules.\n', fout)

        exec(open(MODINIT).read())
        module('purge')
        for mod in MODULES:
            module('load',mod)

        # At NAS something weird is happening with python
        # if you force it to load this at the end, things work
        #if HOST=='PLEIADES':
            #module('load','python/2.7.15')
        #module('list')
    elif os.environ.get('LMOD_PKG') is not None:
        writemsg(' and modules.\n', fout)

        sys.path.insert(0,os.path.join(os.environ['LMOD_PKG'], "init"))
        from env_modules_python import module

        module('purge')
        for mod in MODULES:
            module('load',mod)

    else:
        raise Exception('could not load required modules')

    # set ESMA_FC to gfortran, if needed
    # ----------------------------------
    if BASEDIR.split(os.sep)[-1].split('_')[0]=='gfortran':
        writemsg(' Setting ESMA_FC to gfortran\n', fout)
        os.environ['ESMA_FC'] = 'gfortran'

    # set ESMA_FC to pgfortran, if needed
    # -----------------------------------
    if BASEDIR.split(os.sep)[-1].split('_')[0]=='pgfortran':
        writemsg(' Setting ESMA_FC to pgfortran\n', fout)
        os.environ['ESMA_FC'] = 'pgfortran'
        os.environ['PGI_LOCALRC'] = '/discover/swdev/mathomp4/PGILocalRC/linux86-64/17.10/bin/localrc.60300'
        writemsg(' Setting PGI_LOCALRC to %s\n' % os.environ['PGI_LOCALRC'], fout)
