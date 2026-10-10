import contextlib
import importlib.machinery
import importlib.util
import io
import json
import os
import tempfile
import unittest
from unittest import mock

HERE = os.path.dirname(os.path.realpath(__file__))
_loader = importlib.machinery.SourceFileLoader("ice_runner", os.path.join(HERE, "..", "ice-runner"))
runner = importlib.util.module_from_spec(importlib.util.spec_from_loader("ice_runner", _loader))
_loader.exec_module(runner)

CACHE_KEYS = [key for key, _ in runner.REAL_CACHES]
GOOD = {"configVersion": 2, "packages": [{"name": "a", "rootUri": "file:///real/pub-cache/a"}]}


def clean_environ(**extra):
    env = {k: v for k, v in os.environ.items() if k not in CACHE_KEYS and k != "GOMODCACHE"}
    env.update(extra)
    return env


class SandboxedEnv(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.sandbox = os.path.join(self.tmp.name, "sandbox")

    def test_caches_stay_real_while_home_is_sandboxed(self):
        with mock.patch.dict(os.environ, clean_environ(), clear=True):
            env = runner.sandboxed_env(self.sandbox)
            real = os.path.expanduser("~")
        self.assertEqual(env["HOME"], os.path.join(self.sandbox, "home"))
        self.assertEqual(env["PUB_CACHE"], os.path.join(real, ".pub-cache"))
        self.assertEqual(env["GRADLE_USER_HOME"], os.path.join(real, ".gradle"))
        self.assertEqual(env["CARGO_HOME"], os.path.join(real, ".cargo"))
        self.assertEqual(env["MISE_DATA_DIR"], os.path.join(real, ".local/share/mise"))
        self.assertEqual(env["GOMODCACHE"], os.path.join(real, "go", "pkg", "mod"))
        for key in CACHE_KEYS:
            self.assertNotIn(self.sandbox, env[key])

    def test_explicit_cache_is_preserved(self):
        with mock.patch.dict(os.environ, clean_environ(PUB_CACHE="/explicit/pub", GOPATH="/explicit/go"), clear=True):
            env = runner.sandboxed_env(self.sandbox)
        self.assertEqual(env["PUB_CACHE"], "/explicit/pub")
        self.assertEqual(env["GOMODCACHE"], os.path.join("/explicit/go", "pkg", "mod"))


class BaselineGuard(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.root = os.path.join(self.tmp.name, "proj")
        os.makedirs(os.path.join(self.root, ".dart_tool"))
        os.makedirs(os.path.join(self.root, ".ice"))
        self.config = os.path.join(self.root, ".dart_tool", "package_config.json")

    def run_baseline(self, body):
        script = os.path.join(self.tmp.name, "fake.py")
        with open(script, "w") as f:
            f.write("import os, sys, json\n" + body)
        with open(os.path.join(self.root, ".ice", "config"), "w") as f:
            f.write("test_cmd = %s %s\n" % (os.path.realpath(os.sys.executable), script))
        out = io.StringIO()
        with contextlib.redirect_stdout(out), mock.patch.object(runner, "resolved", lambda argv: argv):
            runner.baseline(self.root, True)
        return out.getvalue()

    def write_config(self, data):
        with open(self.config, "w") as f:
            json.dump(data, f)

    def read_config(self):
        with open(self.config) as f:
            return json.load(f)

    def test_sandbox_pointing_file_is_restored(self):
        self.write_config(GOOD)
        before = os.stat(self.config).st_mtime_ns
        out = self.run_baseline(
            "p = os.path.join(os.environ['HOME'], '.pub-cache', 'a')\n"
            "json.dump({'packages': [{'name': 'a', 'rootUri': 'file://' + p}]}, open('.dart_tool/package_config.json', 'w'))\n")
        self.assertEqual(self.read_config(), GOOD)
        self.assertEqual(os.stat(self.config).st_mtime_ns, before)
        self.assertIn("restored", out)

    def test_sandbox_pointing_file_without_snapshot_gets_a_note(self):
        out = self.run_baseline(
            "p = os.path.join(os.environ['HOME'], '.pub-cache', 'a')\n"
            "json.dump({'packages': [{'rootUri': 'file://' + p}]}, open('.dart_tool/package_config.json', 'w'))\n")
        self.assertIn("flutter pub get", out)

    def test_real_paths_are_kept(self):
        self.write_config({"packages": []})
        self.run_baseline("json.dump(%r, open('.dart_tool/package_config.json', 'w'))\n" % GOOD)
        self.assertEqual(self.read_config(), GOOD)

    def test_pub_cache_is_real_inside_the_run(self):
        self.run_baseline(
            "ok = os.environ['PUB_CACHE'] != os.path.join(os.environ['HOME'], '.pub-cache')\n"
            "open('.dart_tool/seen', 'w').write(str(ok))\n")
        with open(os.path.join(self.root, ".dart_tool", "seen")) as f:
            self.assertEqual(f.read(), "True")


if __name__ == "__main__":
    unittest.main()
