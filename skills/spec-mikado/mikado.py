#!/usr/bin/env python3
"""Mikado graph harness: parse, validate, and gate a CHANGE.md prerequisite DAG."""

import argparse
import os
import re
import subprocess
import sys

NODE_HEADER = re.compile(r"^###\s+([A-Za-z0-9][A-Za-z0-9_-]*)\s*:\s*(.+?)\s*$")
FIELD = re.compile(r"^([a-z]+):\s*(.*?)\s*$")
SECTION = re.compile(r"^##\s+(.+?)\s*$")
STATES = ("open", "armed", "done")


class GraphError(Exception):
    pass


class Node:
    def __init__(self, nid, text, line):
        self.id = nid
        self.text = text
        self.header_line = line
        self.needs = []
        self.verify = ""
        self.state = "open"
        self.state_line = None


class Graph:
    def __init__(self, path):
        self.path = path
        self.lines = []
        self.nodes = {}
        self.order = []

    @property
    def root(self):
        return os.path.dirname(os.path.abspath(self.path)) or "."

    def load(self):
        if not os.path.exists(self.path):
            raise GraphError(f"no such file: {self.path}")
        with open(self.path) as fh:
            self.lines = fh.read().splitlines()
        in_nodes = False
        current = None
        for i, raw in enumerate(self.lines):
            section = SECTION.match(raw)
            if section:
                in_nodes = section.group(1).strip().lower() == "nodes"
                current = None
                continue
            if not in_nodes:
                continue
            header = NODE_HEADER.match(raw)
            if header:
                nid, text = header.group(1), header.group(2)
                if nid in self.nodes:
                    raise GraphError(f"duplicate node id: {nid} (line {i + 1})")
                current = Node(nid, text, i)
                self.nodes[nid] = current
                self.order.append(nid)
                continue
            if current is None:
                continue
            field = FIELD.match(raw)
            if not field:
                continue
            key, value = field.group(1), field.group(2)
            if key == "needs":
                current.needs = [d.strip() for d in value.split(",") if d.strip()]
            elif key == "verify":
                current.verify = value
            elif key == "state":
                if value not in STATES:
                    raise GraphError(
                        f"{current.id}: bad state {value!r} (line {i + 1}); "
                        f"expected one of {', '.join(STATES)}"
                    )
                current.state = value
                current.state_line = i
        if not self.nodes:
            raise GraphError(f"{self.path} has no '## Nodes' section with '### id: text' entries")
        return self

    def save(self):
        with open(self.path, "w") as fh:
            fh.write("\n".join(self.lines) + "\n")

    def set_state(self, node, state):
        if node.state_line is None:
            insert_at = node.header_line + 1
            self.lines.insert(insert_at, f"state: {state}")
            for other in self.nodes.values():
                if other.header_line > node.header_line:
                    other.header_line += 1
                if other.state_line is not None and other.state_line >= insert_at:
                    other.state_line += 1
            node.state_line = insert_at
        else:
            self.lines[node.state_line] = f"state: {state}"
        node.state = state
        self.save()

    def get(self, nid):
        if nid not in self.nodes:
            raise GraphError(f"unknown node: {nid}")
        return self.nodes[nid]

    def missing_deps(self):
        return [
            (n.id, d)
            for n in self.nodes.values()
            for d in n.needs
            if d not in self.nodes
        ]

    def cycles(self):
        WHITE, GREY, BLACK = 0, 1, 2
        color = {nid: WHITE for nid in self.nodes}
        found = []

        def walk(nid, stack):
            color[nid] = GREY
            for dep in self.nodes[nid].needs:
                if dep not in self.nodes:
                    continue
                if color[dep] == GREY:
                    found.append(stack[stack.index(dep):] + [dep])
                elif color[dep] == WHITE:
                    walk(dep, stack + [dep])
            color[nid] = BLACK

        for nid in self.order:
            if color[nid] == WHITE:
                walk(nid, [nid])
        return found

    def goals(self):
        depended = {d for n in self.nodes.values() for d in n.needs}
        return [nid for nid in self.order if nid not in depended]

    def unreachable(self):
        seen = set()

        def walk(nid):
            if nid in seen or nid not in self.nodes:
                return
            seen.add(nid)
            for dep in self.nodes[nid].needs:
                walk(dep)

        for g in self.goals():
            walk(g)
        return [nid for nid in self.order if nid not in seen]

    def ready(self):
        out = []
        for nid in self.order:
            node = self.nodes[nid]
            if node.state == "done":
                continue
            if all(self.nodes[d].state == "done" for d in node.needs if d in self.nodes):
                out.append(node)
        return out

    def run_verify(self, node):
        if not node.verify:
            raise GraphError(f"{node.id}: no verify command - it is not a node, split it")
        proc = subprocess.run(
            node.verify, shell=True, cwd=self.root,
            stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True,
        )
        return proc.returncode, proc.stdout


def cmd_check(g, _args):
    problems = []
    for nid, dep in g.missing_deps():
        problems.append(f"{nid}: needs unknown node {dep!r}")
    for cycle in g.cycles():
        problems.append("cycle: " + " -> ".join(cycle))
    for nid in g.order:
        if not g.nodes[nid].verify:
            problems.append(f"{nid}: no verify command - a node without a runnable verdict is not a node")
    for nid in g.unreachable():
        problems.append(f"{nid}: unreachable from any goal - orphan node")
    for nid in g.order:
        node = g.nodes[nid]
        if node.state == "done":
            open_deps = [d for d in node.needs if d in g.nodes and g.nodes[d].state != "done"]
            if open_deps:
                problems.append(f"{nid}: marked done but needs {', '.join(open_deps)} - parent implemented before its children")
    if problems:
        for p in problems:
            print(f"FAIL {p}")
        return 1
    goals = ", ".join(g.goals()) or "(none)"
    print(f"OK {len(g.nodes)} nodes, goals: {goals}")
    return 0


def cmd_ready(g, args):
    nodes = g.ready()
    if not nodes:
        blocked = [n for n in g.nodes.values() if n.state != "done"]
        print("nothing ready" if blocked else "all nodes done")
        return 1 if blocked else 0
    for n in nodes:
        if args.quiet:
            print(n.id)
        else:
            flag = "" if n.state == "armed" else "  (UNARMED)"
            print(f"{n.id}: {n.text}{flag}")
            print(f"    verify: {n.verify}")
    if not args.quiet and len(nodes) > 1:
        print(f"\n{len(nodes)} independent nodes - dispatchable in parallel")
    return 0


def cmd_arm(g, args):
    node = g.get(args.id)
    code, out = g.run_verify(node)
    if code == 0:
        print(f"REFUSED {node.id}: verify exited 0 before any work was done.")
        print("This verdict cannot fail, so it proves nothing. Narrow it.")
        print(f"    verify: {node.verify}")
        return 1
    if code in (126, 127):
        print(f"REFUSED {node.id}: verify exited {code} - command not found or not executable.")
        print("A broken command is not a failing verdict. Fix the command, then arm.")
        print(f"    verify: {node.verify}")
        print(out, end="" if out.endswith("\n") else "\n")
        return 1
    g.set_state(node, "armed")
    print(f"ARMED {node.id} (verify exited {code}, as required)")
    if args.verbose:
        print(out)
    return 0


def cmd_verify(g, args):
    node = g.get(args.id)
    code, out = g.run_verify(node)
    print(out, end="" if out.endswith("\n") else "\n")
    print(f"{'PASS' if code == 0 else 'FAIL'} {node.id} (exit {code})")
    return 0 if code == 0 else 1


def cmd_done(g, args):
    node = g.get(args.id)
    open_deps = [d for d in node.needs if d in g.nodes and g.nodes[d].state != "done"]
    if open_deps:
        print(f"REFUSED {node.id}: needs {', '.join(open_deps)} - implement children first")
        return 1
    if node.state != "armed":
        print(f"REFUSED {node.id}: state is {node.state!r}, expected 'armed'.")
        print(f"Run: mikado.py arm {node.id}   (verdict must be observed failing before it can pass)")
        return 1
    code, out = g.run_verify(node)
    if code != 0:
        print(out, end="" if out.endswith("\n") else "\n")
        print(f"REFUSED {node.id}: verify exited {code}")
        return 1
    g.set_state(node, "done")
    print(f"DONE {node.id}")
    return 0


def cmd_add(g, args):
    if args.id in g.nodes:
        raise GraphError(f"node already exists: {args.id}")
    needs = [d.strip() for d in (args.needs or "").split(",") if d.strip()]
    for d in needs:
        if d not in g.nodes:
            raise GraphError(f"needs unknown node: {d}")
    block = [
        "",
        f"### {args.id}: {args.text}",
        f"needs: {', '.join(needs)}",
        f"verify: {args.verify}",
        "state: open",
        "",
    ]
    end = len(g.lines)
    for i in range(len(g.lines) - 1, -1, -1):
        section = SECTION.match(g.lines[i])
        if section and section.group(1).strip().lower() != "nodes":
            end = i
        elif section:
            break
    g.lines[end:end] = block
    if args.parent:
        parent = g.get(args.parent)
        rewired = False
        for i in range(parent.header_line + 1, len(g.lines)):
            if NODE_HEADER.match(g.lines[i]) or SECTION.match(g.lines[i]):
                break
            field = FIELD.match(g.lines[i])
            if field and field.group(1) == "needs":
                existing = [d.strip() for d in field.group(2).split(",") if d.strip()]
                if args.id not in existing:
                    existing.append(args.id)
                g.lines[i] = "needs: " + ", ".join(existing)
                rewired = True
                break
        if not rewired:
            g.lines.insert(parent.header_line + 1, f"needs: {args.id}")
    g.save()
    print(f"ADDED {args.id}" + (f" -> prerequisite of {args.parent}" if args.parent else ""))
    return 0


def cmd_graph(g, _args):
    print("graph TD")
    for nid in g.order:
        node = g.nodes[nid]
        label = node.text.replace('"', "'")
        print(f'  {nid}["{label}"]:::{node.state}')
        for dep in node.needs:
            print(f"  {dep} --> {nid}")
    print("  classDef done fill:#1a7f37,color:#fff")
    print("  classDef armed fill:#9a6700,color:#fff")
    print("  classDef open fill:#656d76,color:#fff")
    return 0


def cmd_status(g, _args):
    counts = {s: 0 for s in STATES}
    for n in g.nodes.values():
        counts[n.state] += 1
    total = len(g.nodes)
    print(f"{counts['done']}/{total} done, {counts['armed']} armed, {counts['open']} open")
    ready = g.ready()
    if ready:
        print(f"ready: {', '.join(n.id for n in ready)}")
    blocked = [n for n in g.nodes.values() if n.state != "done" and n not in ready]
    if blocked:
        print(f"blocked: {', '.join(n.id for n in blocked)}")
    return 0


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("-f", "--file", default="CHANGE.md")
    sub = parser.add_subparsers(dest="cmd", required=True)

    sub.add_parser("check", help="validate the DAG").set_defaults(fn=cmd_check)
    p = sub.add_parser("ready", help="nodes whose prerequisites are all done")
    p.add_argument("-q", "--quiet", action="store_true", help="ids only")
    p.set_defaults(fn=cmd_ready)
    p = sub.add_parser("arm", help="require the verdict to fail before any work")
    p.add_argument("id")
    p.add_argument("-v", "--verbose", action="store_true")
    p.set_defaults(fn=cmd_arm)
    p = sub.add_parser("verify", help="run a node's verify command")
    p.add_argument("id")
    p.set_defaults(fn=cmd_verify)
    p = sub.add_parser("done", help="mark done (refuses unless armed and now passing)")
    p.add_argument("id")
    p.set_defaults(fn=cmd_done)
    p = sub.add_parser("add", help="add a discovered prerequisite")
    p.add_argument("id")
    p.add_argument("text")
    p.add_argument("--verify", required=True)
    p.add_argument("--needs", default="")
    p.add_argument("--parent", help="node this is a prerequisite of")
    p.set_defaults(fn=cmd_add)
    sub.add_parser("graph", help="mermaid output").set_defaults(fn=cmd_graph)
    sub.add_parser("status", help="progress summary").set_defaults(fn=cmd_status)

    args = parser.parse_args()
    try:
        return args.fn(Graph(args.file).load(), args)
    except GraphError as exc:
        print(f"ERROR {exc}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main())
