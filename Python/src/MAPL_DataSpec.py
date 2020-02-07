class MAPL_DataSpec:
    """Declare and manipulate an import/export/internal specs for a 
       MAPL Gridded component"""

    stringlike_options = ['SHORT_NAME', 'LONG_NAME', 'UNITS']
    literal_options = ['DIMS', 'VLOCATION', 'NUM_SUBTILES',
                       'REFRESH_INTERVAL', 'AVERAGING_INTERVAL', 'HALOWIDTH',
                       'PRECISION','DEFAULT','RESTART', 'UNGRIDDED_DIMS',
                       'FIELD_TYPE', 'STAGGERING', 'ROTATION']
    all_options = stringlike_options + literal_options


    def __init__(self, category, args, indent=3):
        self.category = category
        self.args = args
        self.indent  = indent

    def newline(self):
        return "\n" + " "*self.indent

    def continue_line(self):
        return "&" + self.newline() + "& "

    def emit_spec(self):
        return self.emit_header() + self.emit_args() + self.emit_trailer()

    def get_rank(self):
        ranks = {'MAPL_DimsHorzVert':3, 'MAPL_DimsHorzOnly':2, 'MAPL_DimsVertOnly':1}
        if 'UNGRIDDED_DIMS' in self.args:
            extra_dims = self.args['UNGRIDDED_DIMS'].strip('][').split(',')
            extra_rank = len(extra_dims)
        else:
            extra_rank = 0
        return ranks[self.args['DIMS']] + extra_rank
        
    def emit_declare_local(self):
        text = self.emit_header()
        type = 'real'
        kind = 'REAL32'
        rank = self.get_rank()
        dimension = 'dimension(:' + ',:'*(rank-1) + ')'
        text = text + type + '(kind=' + str(kind) + '), ' + dimension + ' :: ' + self.args['LOCAL_NAME'] + ' => null()'
        text = text + self.emit_trailer()
        return text

    def emit_get_pointer(self):
        text = self.emit_header()
        text = text + "call MAPL_GetPointer(" + self.category + ', ' + self.args['LOCAL_NAME'] + ", '" + self.args['SHORT_NAME'] + "', rc=status); VERIFY_(status)" 
        text = text + self.emit_trailer()
        return text

    def emit_header(self):
        text = self.newline()
        if 'CONDITION' in self.args and self.args['CONDITION']:
            self.indent = self.indent + 3
            text = text + "if (" + self.args['CONDITION']  + ") then" + self.newline() 
        return text

    def emit_args(self):
        self.indent = self.indent + 5
        text = "call MAPL_Add" + self.category + "Spec(" + self.continue_line()
        for option in MAPL_DataSpec.all_options:
            text = text + self.emit_arg(option)
        text = text + 'rc=status)' + self.newline()
        self.indent = self.indent - 5
        text = text + 'VERIFY_(status)'
        return text

    def emit_arg(self, option):
        text = ''
        if option in self.args:
            value = self.args[option]
            text = text + option + "="
            if option in MAPL_DataSpec.stringlike_options:
                value = "'" + value + "'"
            text = text + value + ", " + self.continue_line()
        return text

    def emit_trailer(self):
        if 'CONDITION' in self.args and self.args['CONDITION']:
            self.indent = self.indent - 3
            text = self.newline()
            text = text + "endif" + self.newline()
        else:
            text = self.newline()
        return text


