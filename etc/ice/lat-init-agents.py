#!/usr/bin/env python3
import os
import pty
import re
import select
import signal
import sys
import time

LABELS = {"claude": "Claude Code", "pi": "Pi", "codex": "Codex", "cursor": "Cursor",
          "copilot": "VS Code Copilot", "opencode": "OpenCode"}
ANSI = re.compile(r"\x1b\[[0-9;?]*[A-Za-z]")
TIMEOUT = 180


def keys_for(options, wanted):
    labels = [label for label, _ in options]
    missing = [w for w in wanted if LABELS[w] not in labels]
    if missing:
        raise SystemExit("lat-init-agents: lat init offers no %s (menu: %s)" % (", ".join(missing), labels))
    want = {LABELS[w] for w in wanted}
    keys, cursor = [], 0
    for index, (label, ticked) in enumerate(options):
        if (label in want) != ticked:
            keys += ["j"] * (index - cursor) + [" "]
            cursor = index
    return keys + ["\r"]


def main(argv):
    if len(argv) != 3:
        sys.stderr.write("usage: lat-init-agents.py DIR claude,codex,pi\n")
        return 2
    target, wanted = argv[1], [a for a in argv[2].split(",") if a]
    unknown = [w for w in wanted if w not in LABELS]
    if unknown:
        sys.stderr.write("lat-init-agents: unknown agent %s\n" % ", ".join(unknown))
        return 2
    pid, fd = pty.fork()
    if pid == 0:
        os.execvp("lat", ["lat", "init", target])
    state = {"screen": "", "open": True}
    seen, deadline = 0, time.time() + TIMEOUT
    agents_done = style_done = False

    def pump(wait):
        ready, _, _ = select.select([fd], [], [], wait)
        if not ready:
            return False
        try:
            chunk = os.read(fd, 65536).decode("utf-8", "replace")
        except OSError:
            chunk = ""
        if not chunk:
            state["open"] = False
            return False
        text = ANSI.sub("", chunk).replace("\r", "")
        sys.stdout.write(text)
        sys.stdout.flush()
        state["screen"] += text
        return True

    def send(text):
        os.write(fd, text.encode())

    def press_in_menu(key):
        renders = state["screen"].count("space: toggle")
        send(key)
        until = time.time() + 5
        while state["screen"].count("space: toggle") <= renders and time.time() < until and state["open"]:
            pump(0.1)

    while state["open"]:
        if time.time() > deadline:
            os.kill(pid, signal.SIGKILL)
            sys.stderr.write("lat-init-agents: timed out\n")
            return 1
        if pump(0.3):
            continue
        screen = state["screen"]
        fresh = screen[seen:]
        if not agents_done and "Which coding agents" in fresh and "space: toggle" in fresh:
            menu = fresh[fresh.index("Which coding agents"):fresh.index("space: toggle")]
            options = [(m.group(2).strip(), m.group(1) == "x") for m in re.finditer(r"\[([ x])\]\s+(.+)", menu)]
            keys = keys_for(options, wanted)
            for key in keys[:-1]:
                press_in_menu(key)
            send(keys[-1])
            agents_done, seen = True, len(state["screen"])
        elif not style_done and "How should agents run lat?" in fresh:
            send("\r")
            style_done, seen = True, len(screen)
        elif "Create lat.md/ directory?" in fresh:
            send("y\r")
            seen = len(screen)
        elif "Paste your key" in fresh:
            send("\r")
            seen = len(screen)
        elif "[Y/n]" in fresh:
            send("n\r")
            seen = len(screen)
    _, status = os.waitpid(pid, 0)
    return os.waitstatus_to_exitcode(status)


if __name__ == "__main__":
    sys.exit(main(sys.argv))
