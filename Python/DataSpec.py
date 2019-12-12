class DataSpec:
    """Declare and manipulate an import/export/internal specs for a 
       MAPL Gridded component"""

    mandatory_options = ['DIMS', 'SHORT_NAME']
    # The following must be quoted when emitted as Fortran source.
    stringlike_options = ['SHORT_NAME', 'LONG_NAME', 'UNITS']
    # The following should NOT be quoted when emitted as Fortran source.
    literal_options = ['DIMS', 'VLOCATION', 'NUM_SUBTILES',
                       'REFRESH_INTERVAL', 'AVERAGING_INTERVAL', 'HALOWIDTH',
                       'PRECISION','DEFAULT','RESTART', 'UNGRIDDED_DIMS',
                       'FIELD_TYPE', 'STAGGERING', 'ROTATION']
    all_options = stringlike_options + literal_options


    def __init__(self, category, args, indent=3):
        self.category = category
        self.args = args
        self.indent  = indent
        self.has_condition = 'CONDITION' in self.args and DataSpec.not_empty(self.args['CONDITION'])


    def not_empty(string):
        return string and not string.isspace()
        
    def newline(self):
        return "\n" + " "*self.indent

    def continue_line(self):
        return "&" + self.newline() + "& "

    def emit_declare_spec(self):
        return self.wrap_conditional(self.emit_MAPL_AddSpec)

    def wrap_conditional(self, content_method):
        text = self.newline()
        if self.has_condition:
            text = text + "if (" + self.args['CONDITION']  + ") then"
            self.indent = self.indent + 3
            text = text + self.newline()
        text = text + content_method()
        if self.has_condition:
            self.indent = self.indent - 3
            text = text + self.newline()
            text = text + "endif"
        return text + self.newline()
        
    def get_rank(self):
        gridded_ranks = {'MAPL_DimsHorzVert':3, 'MAPL_DimsHorzOnly':2, 'MAPL_DimsVertOnly':1}
        if 'UNGRIDDED_DIMS' in self.args:
            extra_dims = self.args['UNGRIDDED_DIMS'].strip('][').split(',')
            extra_rank = len(extra_dims)
        else:
            extra_rank = 0
        return gridded_ranks[self.args['DIMS']] + extra_rank
        
    def emit_declare_local_variable(self):
        return self.wrap_conditional(self.emit_MAPL_declare_local_variable)

    def emit_MAPL_declare_local_variable(self):
        type = 'real'
        kind = 'REAL32'
        rank = self.get_rank()
        dimension = 'dimension(:' + ',:'*(rank-1) + ')'
        text = type + '(kind=' + str(kind) + '), ' + dimension + ' :: ' + self.args['LOCAL_NAME'] + ' => null()'
        return text

    def emit_get_pointer(self):
        return self.wrap_conditional(self.emit_MAPL_GetPointer)

    def emit_MAPL_GetPointer(self):
        text = "call MAPL_GetPointer(" + self.category + ', ' + self.args['LOCAL_NAME'] + ", '" + self.args['SHORT_NAME'] + "', rc=status); VERIFY_(status)" 
        return text


    def emit_MAPL_AddSpec(self):
        self.indent = self.indent + 5
        text = "call MAPL_Add" + self.category + "Spec(" + self.continue_line()
        for option in DataSpec.all_options:
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
            if option in DataSpec.stringlike_options:
                value = "'" + value + "'"
            text = text + value + ", " + self.continue_line()
        return text



