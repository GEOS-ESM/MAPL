#!/usr/bin/env python3
import unittest
import argparse
from itertools import product
from functools import reduce, partial
from operator import concat
from collections import namedtuple
import sys
import MAPL_GridCompSpecs_ACGv3 as acg3

TestParams = namedtuple('TestParams', 'value test msg'.split())

def general_msg(variable='EXPECTED VARIABLE', value=None):
    return f"{variable} should be {'None' if value is None else value}."

make_equal_test = lambda self, expected: partial(self.assertEqual, expected)

class TestMappings(unittest.TestCase):

    def test_get_internal_name(self):
        SHORT_NAME = acg3.SHORT_NAME
        get_internal_name = acg3.get_internal_name
        message = lambda value: general_msg('internal_name', value)
        equal_test = lambda expected: make_equal_test(self, expected)
        test_params = (
            TestParams(None, self.assertIsNone, message(None)),
            TestParams({'k': 'v'}, equal_test(acg3.EMPTY), message(None)),
            TestParams({SHORT_NAME: 'XX'}, equal_test('XX'), message('XX')),
            TestParams({SHORT_NAME: '*XX'}, equal_test('XX'), message('XX')),
            TestParams({SHORT_NAME: 'XX', acg3.ALIAS: 'XY'}, equal_test('XY'), message('XY')),
        )
        for value, test, msg in test_params:
            with self.subTest(value=value, test=test, msg=msg):
               test(acg3.get_internal_name(value), msg)

    def test_mangle_standard_name(self):
        equal_test = lambda expected: make_equal_test(self, expected)
        def message(e, a):
            return f"{a} should be {e}"
        values = (((None, None), None), (('*XX', None), f"trim(comp_name)//'XX'"),
                 (('*XX', 'P'), f"trim(P)//'XX'"), (('XX', None), f"'XX'"))
        test_params = (TestParams((n, p), equal_test(e), partial(message, e)) for (n, p), e in values)
        for (name, prefix), test, msg in test_params:
            a = acg3.mangle_standard_name(name, prefix)
            m = msg(a)
            with self.subTest(a=a,m=m):
                test(a, m)

    def test_construct_string_vector(self):
        TO_STRING_VECTOR = acg3.TO_STRING_VECTOR
        equal_test = lambda expected: make_equal_test(self, expected)
        def message(e, a):
            return general_msg(a, e)
        some_expected = f"{TO_STRING_VECTOR}('XX')"
        test_params = (TestParams(None, equal_test(None), partial(message, None)),
           TestParams('XX', equal_test(some_expected), partial(message, some_expected)))
        for value, test, msg in test_params:
            a = acg3.construct_string_vector(value)
            m = msg(a)
            with self.subTest(a=a, m=m):
                test(a, m)

    def test_mk_array(self):
        equal_test = lambda expected: make_equal_test(self, expected)
        def message(e, a):
            return general_msg(a, e)
        test_params = (
                TestParams(value, equal_test(expected), partial(message, expected)) for value, expected in
                ((None, None), (' a ', '[ a]'), (' a,  b ', '[ a,  b]'), (' [  a ] ', '[   a ]'), 
                 ('  [a,  b]  ', '[ a,  b]'), (' [a,  b', '[ a,  b]'), ('  a,  b ] ', '[ a,  b ]'))
        )
        for value, test, msg in test_params:
            a = acg3.mk_array(value)
            m = msg(a)
            with self.subTest(a=a, m=m):
                test(a, m)

    def test_mangled(self):
        mangle = acg3.NAMED_MAPPINGS[acg3.MANGLED]
        self.assertEqual(acg3.MANGLED, 'mangled')
        INTERLUDE = "'//trim(comp_name)//'"
        equal_test = lambda expected: make_equal_test(self, expected)
        def message(e, a):
            return general_msg(a, e)
        test_params = (
                TestParams(value, equal_test(expected), partial(message, expected)) for value, expected in
                ((None, None), ('XX', r"'XX'"), ('*XX', f"'{INTERLUDE}XX'"))
        )
        for value, test, msg in test_params:
            a = mangle(value)
            m = msg(a)
            with self.subTest(a=a, m=m):
                test(a, m)

    def test_compute_rank(self):
        DIMS = ("'z'", "'xy'", "'xyz'") 
        DIM_RANKS = (1, 2, 3)
        UNGRIDDED = ('[1]', '[1, 2, 3]', None)
        UNGRIDDED_RANKS = (1, 3, 0)
        VALUES = tuple(product(DIMS, UNGRIDDED))
        RANKS = tuple(map(sum, product(DIM_RANKS, UNGRIDDED_RANKS)))
        message = lambda v: general_msg('rank', v)
        actuals = tuple(acg3.compute_rank(*v) for v in VALUES)
        _ = (self.assertIsNotNone(a) for a in actuals)
        for a, e in zip(actuals, RANKS):
            with self.subTest():
                self.assertEqual(e, a, message(e))

    def test_compute_rank_None(self):
        UNGRIDDED = '[1]'
        r = acg3.compute_rank(None, UNGRIDDED)
        m = general_msg('rank', None)
        self.assertIsNone(r, m)
        r = acg3.compute_rank('txyz', UNGRIDDED)
        self.assertIsNone(r, m)
        
class TestHelpers(unittest.TestCase):

    def test_isiterable(self):
        test_params = ((None, False, 'None is not iterable.'),
                       ('string', False, 'string is not iterable.'),
                       ('string', True, 'string is iterable.'),
                       (object(), False, 'object is not iterable.'),
                       ([], True, 'list is iterable.'),
                       ((), True, 'tuple is iterable.'),
                       (set(), True, 'set is iterable.'),
                       ({}, True, 'dict is iterable.'))

        for v, e, m in test_params:
            with self.subTest(v=v, e=e, m=m):
                if e:
                    if isinstance(v, str):
                        self.assertTrue(acg3.isiterable(v, False), msg=m)
                    else:
                        self.assertTrue(acg3.isiterable(v), msg=m)
                else:
                    self.assertFalse(acg3.isiterable(v), msg=m)

    def test_mkiterable(self):
        mkiterable = acg3.mkiterable
        isiterable = acg3.isiterable
        test_params = (None, 'string', 2, 2.0, [2], (2,), {'2': 2}, {'2'})
        is_iterable = None
        for o in test_params:
            typename = o.__class__.__name__
            it = mkiterable(o)
            match o:
                case str() | int() | float() | bool():
                    is_iterable = False
                case tuple() | list() | set() | dict():
                    is_iterable = True
            with self.subTest(typename=typename, it=it, is_iterable=is_iterable):
                msg1 = f"mkiterable({typename}) should be iterable."
                msg2 = f"mkiterable({typename}) should equal {typename}."
                self.assertTrue(isiterable(it), msg=msg1)
                if is_iterable == True:
                    self.assertEqual(o, it, msg=msg2)
        o = 'string'
        it = mkiterable(o, exclude_string=False)
        typename = o.__class__.__name__ 
        msg1=f"mkiterable({typename}, exclude_string=False) should be iterable."
        self.assertTrue(isiterable(it, exclude_string=False), msg=msg1)

    def test_flatten_specs(self):
        message = general_msg('specs', 'a list')
        list_test = lambda o, msg: self.assertTrue(isinstance(acg3.flatten_specs(o), list), msg)
        d = {'A': [{'a': 'apple', 'b': 'banana'}, {'a': 'ant', 'b': 'boy'}],
            'B': [{'a': 'artichoke', 'b': 'ball'}, {'a': 'army', 'b': 'bottle'}]} 
        test_params = (
            TestParams(range(10), list_test, message),
            TestParams(d, list_test, message)
        )

        for value, test, msg in test_params:
            with self.subTest(value=value, test=test, msg=msg):
                test(value, msg)

    def test_flatten_options_dict(self):
        message = general_msg('options', 'a dict')
        options = {
            'specifications': {'short_name': {'mapping': 'mock_mapping'}},
            'args': dict((key, f"{key}.h") for key in 'import'.split()),
            'aliases': {'alpha': 'a'}}
        self.assertTrue(isinstance(acg3.flatten_options(options), dict), msg=message)

    def test_rm_quotes(self):
        has_no_quotes = lambda s, msg: self.assertNotRegex(s, "(\'|\")", msg=msg)
        message = lambda s: f"{s} has quotes."
        XX = "'XX'"
        XY = '"XX"'
        VALUES = (f"{XX}", f"{XY}", f"'{XY}'", f"{XX} and {XY}", f"'\"{XX}\"'", 'XX')
        TNONE = TestParams(None, self.assertIsNone, general_msg('None', None))
        test_params = (TNONE, *[TestParams(s, has_no_quotes, message(s)) for s in VALUES])
        for value, test, msg in test_params:
            with self.subTest(value=value, test=test, msg=msg):
                test(acg3.rm_quotes(value), msg)

    def test_add_quotes(self):
        XX = 'XX'
        XY = f"'{XX}'"
        XZ = f'"{XX}"'
        message = lambda t, s: f"{s} does not have the format {t}."
        values = ((None, None), (XX, XY), (XY, XY), (XZ, XY), (f'{XY} and {XZ}', f"'{XX} and {XX}'"))
        test_params = (TestParams(v, partial(self.assertEqual, r), partial(message, r)) for v, r in values)
        for value, test, msg in test_params:
            r = acg3.add_quotes(value)
            m = msg(r)
            with self.subTest(test=test, m=m):
                test(acg3.add_quotes(value), msg)

    def test_make_block(self):
        INDENT = acg3.INDENT
        CONDITION = '1 > 0'
        CONDITIONS = (None, CONDITION, CONDITION)
        IF_LINE = [f"if ({CONDITION}) then"]
        FIRST_LINES = ([], IF_LINE, IF_LINE)
        TEXT = [f'call subroutine(arg, &', f'{INDENT}& arg2)']
        INDENTED_TEXT = [f"{INDENT}{line}" for line in TEXT]
        TEXT_LINES = (TEXT, INDENTED_TEXT, INDENTED_TEXT)
        ELSE_BLOCKS = ([], [], acg3.make_else_block('XX'))
        END_LINE = ['end if']
        END_LINES = ([], END_LINE, END_LINE)
        EXPECTEDS = (reduce(concat, t) for t in tuple(zip(FIRST_LINES, TEXT_LINES, ELSE_BLOCKS, END_LINES)))
        equal_test = lambda expected: make_equal_test(self, expected)
        message = lambda value: general_msg('make_block', value)
        test_params = (TestParams((c, eb), equal_test(e), message(e)) for c, eb, e in
            list(zip(CONDITIONS, ELSE_BLOCKS, EXPECTEDS)))
        for (condition, else_block), test, msg in test_params:
            r = acg3.make_block(condition, TEXT, else_block)
            with self.subTest(test=test, msg=msg):
                test(r, msg)

    def test_make_else_block(self):
        EXPECTED = ['else', f'{acg3.INDENT}nullify(XX)']
        equal_test = lambda expected: make_equal_test(self, expected)
        message = lambda value: general_msg('else_block', value)
        test_params = (TestParams(None, equal_test([]), message([])),
                       TestParams('XX', equal_test(EXPECTED), message(EXPECTED)))
        for name, test, msg in test_params:
            r = acg3.make_else_block(name)
            with self.subTest(test=test, msg=msg):
                test(r, msg)

test_cases = (TestMappings, TestHelpers)

def load_tests(loader, tests, pattern):
    suite = unittest.TestSuite()
    for test_class in test_cases:
        tests = loader.loadTestsFromTestCase(test_class)
        suite.addTests(tests)
    return suite
