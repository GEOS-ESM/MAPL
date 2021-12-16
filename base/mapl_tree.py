#!/usr/bin/env python

import os, sys, glob
import itertools, argparse

__VERSION__ = '1.0.0'

class MAPL_Tree():
    """
    #-----------------------------------------------------------------------
    # defines a dict gc_dir with key: comp_name, val: corresponding_dir
    # the method getDirFromComp returns takes a component name and returns
    # the corresponding dir name
    #-----------------------------------------------------------------------
    """

    def __init__(self, output_format='txt', output_color=False, add_link=False,
                 full_tree=False, trim=False, repo=False):
        """
        #-----------------------------------------------------------------------
        # defines the following:
        #
        # attributes:
        #   name
        #   dict gc_dir
        #   out_format
        #   out_color
        #   out_link
        #   full_tree
        #   space
        #
        # for output format mm, prints <map version="0.9.0">
        #-----------------------------------------------------------------------
        """

        # who am i
        # --------
        self.name = 'mapl_tree'

        # output format
        # -------------
        self.out_format = output_format

        # output color
        # color surface and children green
        # --------------------------------
        self.out_color = output_color

        # add external links
        # ------------------
        self.add_link = add_link

        # Option to display full tree
        # ---------------------------
        self.full_tree = full_tree

        # how much space
        # --------------
        self.space = '    '

        # Trim Grid Component names, etc
        # ------------------------------
        self.trim = trim

        # Meanty for repository hierarchy
        # -------------------------------
        self.repo = repo

        # header for mindmap output
        # -------------------------
        if self.out_format=='mm':
            print('<map version="0.9.0">')

        # the dictionary (used by non-maintained chname style)
        # ----------------------------------------------------
        self.gc_dir = dict()
        self.gc_dir['gcs'] = '@GEOSgcs_GridComp'
        self.gc_dir['gcm'] = '@GEOSgcm_GridComp'
        self.gc_dir['ana'] = None
        self.gc_dir['dataatm'] = None
        self.gc_dir['agcm'] = 'GEOSagcm_GridComp'
        self.gc_dir['aiau'] = 'GEOSmkiau_GridComp'
        self.gc_dir['adfi'] = None
        self.gc_dir['ogcm'] = 'GEOSogcm_GridComp'
        self.gc_dir['scmdynamics'] = None
        self.gc_dir['superdynamics'] = 'GEOSsuperdyn_GridComp'
        self.gc_dir['physics'] = 'GEOSphysics_GridComp'
        self.gc_dir['dyn'] = '@FVdycoreCubed_GridComp'
        self.gc_dir['gwd'] = 'GEOSgwd_GridComp'
        self.gc_dir['moist'] = 'GEOSmoist_GridComp'
        self.gc_dir['surface'] = 'GEOSsurface_GridComp'
        self.gc_dir['turbulence'] = 'GEOSturbulence_GridComp'
        self.gc_dir['chemistry'] = '@GEOSchem_GridComp'
        self.gc_dir['radiation'] = 'GEOSradiation_GridComp'
        self.gc_dir['saltwater'] = 'GEOSsaltwater_GridComp'
        self.gc_dir['lake'] = 'GEOSlake_GridComp'
        self.gc_dir['landice'] = 'GEOSlandice_GridComp'
        self.gc_dir['land'] = 'GEOSland_GridComp'
        self.gc_dir['vegdyn'] = 'GEOSvegdyn_GridComp'
        self.gc_dir['catch'] = 'GEOScatch_GridComp'
        self.gc_dir['chemenv'] = None
        self.gc_dir['pchem'] = 'GEOSpchem_GridComp'
        self.gc_dir['GOCART'] = '@GOCART'
        self.gc_dir['gocart'] = 'GOCART_GridComp'
        self.gc_dir['gocart2g'] = 'GOCART2G_GridComp'
        self.gc_dir['gaas'] = 'GAAS_GridComp'
        self.gc_dir['h2o'] = None
        self.gc_dir['stratchem'] = 'StratChem_GridComp'
        self.gc_dir['gmichem'] = 'GMIchem_GridComp'
        self.gc_dir['carma'] = 'CARMAchem_GridComp'
        self.gc_dir['geoschem'] = 'GEOSCHEMchem_GridComp'
        self.gc_dir['matrix'] = 'MATRIXchem_GridComp'
        self.gc_dir['mam'] = 'MAMchem_GridComp'
        self.gc_dir['solar'] = 'GEOSsolar_GridComp'
        self.gc_dir['irrad'] = 'GEOSirrad_GridComp'
        self.gc_dir['satsim'] = 'GEOSsatsim_GridComp'
        #self.gc_dir['obio'] = 'GEOS_OceanBioGeoChemGridComp'
        self.gc_dir['obio'] = None
        self.gc_dir['orad'] = 'GEOS_OradGridComp'
        self.gc_dir['seaice'] = 'GEOSseaice_GridComp'
        self.gc_dir['ocean'] = 'GEOS_OceanGridComp'

    def __del__(self):
        """
        #-----------------------------------------------------------------------
        # for output format mindmap, prints </map>
        #-----------------------------------------------------------------------
        """

        if self.out_format=='mm':
            print('</map>')

    def getDirFromComp(self, comp):
        """
        #-----------------------------------------------------------------------
        # returns the dir name corresponding to the gridcomp
        # returns None if gridcomp name is not in the list
        #-----------------------------------------------------------------------
        """

        comp = comp.lower()
        if comp in list(self.gc_dir.keys()):
            return self.gc_dir[comp]
        else:
            return None

    def get_link(self, compname):
        """
        #-----------------------------------------------------------------------
        # returns an (external) link corresponding to the component
        # if no link exists, returns None
        #-----------------------------------------------------------------------
        """

        if compname.lower() == 'agcm':
            return 'http://geos5.org/wiki/index.php?title=AGCM_Structure'
        elif compname.lower() == 'ogcm':
            return 'http://geos5.org/wiki/index.php?title=OGCM_Structure'
        elif compname.lower() == 'aana':
            return 'http://geos5.org/wiki/index.php?title=Atmospheric_Analysis'
        else:
            return None

    def get_color(self, compname):
        """
        #-----------------------------------------------------------------------
        # returns color of the component. may be None
        #-----------------------------------------------------------------------
        """

        if compname.lower() in ['surface', 'lake', 'landice',
                                'land', 'saltwater', 'vegdyn', 'catch']:
            return '#008000'
        else:
            return None

    def write_comp(self, compname_, level, isRoot=False):
        """
        #-----------------------------------------------------------------------
        # write gridcomp name to stdout in the format specified by self.out_format
        # On output return True/False if component name has been written/suppressed
        #-----------------------------------------------------------------------
        """

        # Adhoc name patches for readability (trim mode only)
        # ---------------------------------------------------
        Alias = dict ( Gcm        = 'GCM',
                       Mkiau      = 'MkIAU',
                       Superdyn   = 'SuperDyn',
                       Datmodyn   = 'DatmoDyn',
                       Dataatm    = 'DataAtm',
                       Satsim     = 'SatSim',
                       Seaice     = 'SeaIce',
                       Datasea    = 'DataSea',
                       Dataseaice = 'DataSeaIce',
                       Landice    = 'LandIce',
                       Saltwater  = 'SaltWater',
                       Gwd        = 'GWD',
                       Vegdyn     = 'VegDyn',
                       )

        if self.trim or self.repo:
            is_not_comp = not ( 'GridComp' in compname_ )
            is_not_repo = not ( isRoot or compname_[0]=='@' or compname_[-1]=='@' )
            if self.trim and is_not_comp:
                return False # not written
            elif self.repo and is_not_repo:
                return False # not written
            else:
                compname = compname_.replace('@','')
                if self.trim:
                    compname = compname.replace('GridComp','')\
                                       .replace('_','')\
                                       .replace('GEOS','')
                    compname = compname[0].upper() + compname[1:]
                if compname in Alias:
                    compname = Alias[compname]
        else:
            compname = compname_

        if self.add_link: MYLINK = self.get_link(compname)
        else: MYLINK = None
        if self.out_color: MYCOLOR = self.get_color(compname)
        else: MYCOLOR = None

        if self.out_format=='mm':
            txt2prnt = level*self.space + '<node TEXT="' + compname + '"'
            if MYLINK:
                txt2prnt += ' LINK="' + MYLINK + '"'
            if MYCOLOR:
                txt2prnt += ' COLOR="' + MYCOLOR + '"'
            print(txt2prnt +'>')

        elif self.out_format=='txt':
            print(level*('|'+self.space) + compname)
        else:
            raise Exception('output format [%s] not recognized' % self.out_format)

        return True # component has been written out

    def write_end(self, level):
        """
        #-----------------------------------------------------------------------
        # for mindmap (xml) output, we need a </node>
        #-----------------------------------------------------------------------
        """

        if self.out_format=='mm':
            print(level*self.space + '</node>')

    def traverse_chname(self, parentdir, parentcomp, level=0):
        """
        #-----------------------------------------------------------------------
        # recursive function that traverses the directory struture and prints
        # out either an ascii table or xml type tree structure of GEOS
        #
        # inputs:
        #    parentdir:  root GEOS directory
        #    parentcomp: name of the root component
        #    level:      counter for recursion (default 0)
        #
        # output:
        #    tree structure of GEOS component names (children)
        #-----------------------------------------------------------------------
        """

        parentdir = parentdir.rstrip(os.sep)

        # list of GridComp src files
        # --------------------------
        filelist = os.listdir(parentdir)
        src_files = glob.glob(parentdir+os.sep+'*GridComp.F90')

        # get children comp names from src files
        # ch_comps is a list of lists, and needs to be flattened
        # ------------------------------------------------------
        ch_comps = list()
        for file in src_files:
            ch_comps.append(self.parse_fort_src(file))
        ch_comps = list(itertools.chain(*ch_comps))

        # write comp name to stdout
        # -------------------------
        self.write_comp(parentcomp, level)

        # for each child comp, if a corresponding
        # directory exists, recurse. else print child name
        # ------------------------------------------------
        for child_comp in ch_comps:
            child_dir = self.getDirFromComp(child_comp)
            if child_dir:
                CH_DIR = parentdir + os.sep + child_dir
                self.traverse_chname(CH_DIR, child_comp, level+1)
            else:
                self.write_comp(child_comp, level+1)
                self.write_end(level+1)

        # write end to stdout (xml)
        # -------------------------
        self.write_end(level)

    def parse_fort_src(self, fortsrc):
        """
        #-----------------------------------------------------------------------
        # process gridcomp src file and return a list of children components
        #
        # inputs:
        #    fortsrc: Fortran src file
        #
        # output:
        #    list of children components
        #-----------------------------------------------------------------------
        """

        children = list()
        fin = open(fortsrc, 'r')
        for line in fin:
            if 'mapl_addchild' in line.lower():
                child = line.split(',')[1].split('=')[1].replace("'","").replace('"','').lower()
                children.append(child)
        fin.close()
        return children

    def traverse_dirname(self, parentdir, level=0, isRoot=False):
        """
        #-----------------------------------------------------------------------
        # recursive function that traverses the directory structure and prints
        # out either an ascii table or xml type tree structure of GEOS by
        # processing the dir names
        #
        # inputs:
        #    parentdir:  root GEOS directory
        #    level:      counter for recursion (default 0)
        #
        # output:
        #    tree structure of GEOS component names (directory names)
        #-----------------------------------------------------------------------
        """

        parentdir = parentdir.rstrip(os.sep)

        # write comp name to stdout
        # -------------------------
        NAM2WRT = parentdir.split('/')[-1]

        written = self.write_comp(NAM2WRT, level, isRoot=isRoot)

        if not written:
            level = level - 1 # writing may have been suppressed when trimming

        # Some of the external sub-repos in GEOS are very deep and
        # noisy. For most cases, we don't really care how deep they are
        # as we can't easily control that. So by default, we will "stop"
        # these directories from being traversed. You can use the --full
        # option if you wish to see them
        # --------------------------------------------------------------
        if not self.full_tree:
            dirs_to_stop_traversing = ['ecbuild','FMS','mom','mom6','geos-chem','HEMCO']
        else:
            dirs_to_stop_traversing = []

        # We strip off the commercial-at which might be prefixed or postfixed
        # -------------------------------------------------------------------
        if os.path.basename(parentdir).strip('@') not in dirs_to_stop_traversing:

            # get subdirectories of parentdir, consider
            # only those that end with _GridComp or ModPlug
            # ---------------------------------------------
            dirlist = os.listdir(parentdir)
            chdirs = list()

            # There are some directories we don't need to display with tree
            # -------------------------------------------------------------
            dirs_to_ignore = ['.git', '.github', '.circleci','.mepo','.codebuild']

            for subdir in dirlist:
                if os.path.isdir(parentdir + os.sep + subdir):

                    # We also want to ignore any build or install directories
                    # -------------------------------------------------------
                    rules_to_ignore = [subdir in dirs_to_ignore,
                            subdir.startswith('build'),
                            subdir.startswith('install')]

                    # This bypasses any subdir that matches our ignore rules
                    # ------------------------------------------------------
                    if not any(rules_to_ignore):
                        chdirs.append(parentdir + os.sep + subdir)

            # if chdirs is non-empty, recurse, else do nothing
            # ------------------------------------------------
            if chdirs:
                for chdir in chdirs:
                    self.traverse_dirname(chdir, level+1)

        # write end to stdout (xml)
        # -------------------------
        if written:
            self.write_end(level)

# end class MAPL_Tree

def main():
    """
    # --------------------------------------------------------------------------
    # main function
    # --------------------------------------------------------------------------
    """

    comm_opts = parse_args()
    ROOTDIR   = comm_opts['dir']
    ROOTCOMP  = None # comm_opts['rootcomp']
    OUT_TYPE  = 'dirname'  # comm_opts['outtype']
    OUT_FORM  = comm_opts['format']
    OUT_COLOR = comm_opts['color']
    ADD_LINK  = comm_opts['link']
    FULL_TREE = comm_opts['full']
    TRIM      = comm_opts['trim']
    REPO      = comm_opts['repo']

    if ( REPO ):
         FULL_TREE = True
         TRIM = False
    
    MT = MAPL_Tree(OUT_FORM, OUT_COLOR, ADD_LINK, FULL_TREE, TRIM, REPO)

    if OUT_TYPE=='chname':
        MT.traverse_chname(ROOTDIR, ROOTCOMP) # TO DO: remove this option
    elif OUT_TYPE=='dirname':
        MT.traverse_dirname(ROOTDIR,isRoot=True)
    else:
        raise Exception('output type [%s] not recognized' % OUT_TYPE)

def parse_args():
    """
    # --------------------------------------------------------------------------
    # parse command line arguments and return a dict of options
    # --------------------------------------------------------------------------
    """

    p = argparse.ArgumentParser(epilog= "Program to create a hierarchical "
                                "tree of GridComps. This is done either by reading "
                                "the _GridComp directory names or parsing the GridComp "
                                "source code for children names. Output is either an "
                                "ascii table or an xml type file readable by Freemind.")

    # top level directory, output type
    # --------------------------------
    p.add_argument('-c','--color', help='color nodes (edit MAPL_Tree::get_color)', action='store_true')
    p.add_argument('-d', '--dir', help='top level GridComp directory', default='.')
    p.add_argument('-f','--format', help='output format: ascii (txt) or mindmap (mm)',
                    choices=['txt','mm'], default='txt')
    p.add_argument('-F','--full', help='display full tree', action='store_true')
    p.add_argument('-l','--link', help='add external link to nodes (edit MAPL_Tree::get_link)', action='store_true')
    p.add_argument('-t','--trim', help='skip non GridComps, shorten names, use bult-in aliases',action='store_true')
    p.add_argument('-r','--repo', help='shows only the repository hierarchy',action='store_true')

    # Do not document this, it should be removed
    # ------------------------------------------
    # p.add_argument('-m', '--mode', help='output mode (dirname/chname)', choices=['dirname','chname'], default='dirname')
    # p.add_argument('-c','--comp', help='top level GridComp name', required=False)

    args = vars(p.parse_args()) # vars converts to dict

    # checks on input values
    # ----------------------
    if not os.path.isdir(args['dir']):
        raise Exception('root directory [%s] does not exist' % args['dir'])

    ### if args['mode'] == 'chname' and not args['rootcomp']:
    ###    p.error("--outtype=chname requires --rootcomp")

    return args

if __name__=="__main__":
    main()

