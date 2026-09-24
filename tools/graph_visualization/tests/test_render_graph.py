#!/usr/bin/env python3
"""Task 5.5: unittest coverage for render_graph.py (matching
apps/tests/acg3/acg3_unittests.py's existing convention) - success,
missing-dot, and malformed-DOT cases. The success case is skipped
(not failed) on a test machine where Graphviz's `dot` is not
installed; the missing-dot and malformed-input cases are exercised
via subprocess.run mocking so they do not depend on the test
environment's own Graphviz availability.
"""
import os
import shutil
import sys
import tempfile
import unittest
from unittest import mock

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import render_graph  # noqa: E402

VALID_DOT = 'digraph G { "a" -> "b"; }\n'
MALFORMED_DOT = 'digraph G { this is not valid dot @@@\n'


class TestOutputFormatFromPath(unittest.TestCase):

    def test_infers_format_from_extension(self):
        self.assertEqual('svg', render_graph.output_format_from_path('graph.svg'))
        self.assertEqual('png', render_graph.output_format_from_path('graph.png'))

    def test_raises_when_no_extension(self):
        with self.assertRaises(render_graph.RenderError):
            render_graph.output_format_from_path('graph')


class TestRender(unittest.TestCase):

    def setUp(self):
        self.tmpdir = tempfile.mkdtemp()

    def tearDown(self):
        shutil.rmtree(self.tmpdir, ignore_errors=True)

    def _write(self, name, content):
        path = os.path.join(self.tmpdir, name)
        with open(path, 'w') as f:
            f.write(content)
        return path

    @unittest.skipIf(shutil.which('dot') is None, 'Graphviz dot not installed on this machine')
    def test_successful_render(self):
        input_path = self._write('graph.dot', VALID_DOT)
        output_path = os.path.join(self.tmpdir, 'graph.svg')

        render_graph.render(input_path, output_path)

        self.assertTrue(os.path.exists(output_path))
        self.assertGreater(os.path.getsize(output_path), 0)

    def test_missing_dot_executable_raises_clear_error(self):
        input_path = self._write('graph.dot', VALID_DOT)
        output_path = os.path.join(self.tmpdir, 'graph.svg')

        with mock.patch('subprocess.run', side_effect=FileNotFoundError()):
            with self.assertRaises(render_graph.RenderError) as ctx:
                render_graph.render(input_path, output_path)

        self.assertIn('dot', str(ctx.exception).lower())
        self.assertFalse(os.path.exists(output_path))

    def test_malformed_dot_input_is_reported_not_swallowed(self):
        input_path = self._write('bad.dot', MALFORMED_DOT)
        output_path = os.path.join(self.tmpdir, 'bad.svg')

        fake_result = mock.Mock(returncode=1, stdout='', stderr='Error: syntax error')
        with mock.patch('subprocess.run', return_value=fake_result):
            with self.assertRaises(render_graph.RenderError) as ctx:
                render_graph.render(input_path, output_path)

        self.assertIn('syntax error', str(ctx.exception))
        self.assertFalse(os.path.exists(output_path))

    def test_failed_render_removes_stray_output_file(self):
        # Guards the "do not leave behind an output file that implies
        # success" requirement even if dot itself leaves a partial file
        # behind on failure (spec.md "Graphviz layout failure is
        # reported, not swallowed").
        input_path = self._write('bad.dot', MALFORMED_DOT)
        output_path = os.path.join(self.tmpdir, 'bad.svg')

        fake_result = mock.Mock(returncode=1, stdout='', stderr='boom')

        def run_and_touch(*args, **kwargs):
            # Simulate dot leaving a stray (partial) file behind.
            with open(output_path, 'w') as f:
                f.write('')
            return fake_result

        with mock.patch('subprocess.run', side_effect=run_and_touch):
            with self.assertRaises(render_graph.RenderError):
                render_graph.render(input_path, output_path)

        self.assertFalse(os.path.exists(output_path))


class TestMain(unittest.TestCase):

    def test_main_returns_nonzero_on_render_error(self):
        with mock.patch.object(render_graph, 'render', side_effect=render_graph.RenderError('boom')):
            rc = render_graph.main(['in.dot', 'out.svg'])
        self.assertEqual(1, rc)

    @unittest.skipIf(shutil.which('dot') is None, 'Graphviz dot not installed on this machine')
    def test_main_returns_zero_on_success(self):
        tmpdir = tempfile.mkdtemp()
        try:
            input_path = os.path.join(tmpdir, 'graph.dot')
            with open(input_path, 'w') as f:
                f.write(VALID_DOT)
            output_path = os.path.join(tmpdir, 'graph.svg')

            rc = render_graph.main([input_path, output_path])
            self.assertEqual(0, rc)
            self.assertTrue(os.path.exists(output_path))
        finally:
            shutil.rmtree(tmpdir, ignore_errors=True)


if __name__ == '__main__':
    unittest.main()
