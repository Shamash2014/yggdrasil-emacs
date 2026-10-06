import { execFileSync } from "node:child_process";
import { existsSync } from "node:fs";
import { join } from "node:path";

const VENDORED = new Set([".direnv", ".venv", "venv", "site-packages", "node_modules", "vendor", "target", "build", "dist", ".dart_tool", "Pods"]);

export function compare(a, b) {
  return a < b ? -1 : a > b ? 1 : 0;
}

export function git(root, args) {
  return execFileSync("git", ["-C", root, ...args], { encoding: "utf8", maxBuffer: 1 << 28 });
}

export function isVendored(path) {
  return path.split("/").some((part) => VENDORED.has(part));
}

export function listFiles(root) {
  const names = new Set();
  for (const args of [["ls-files", "-z"], ["ls-files", "-z", "--others", "--exclude-standard"]]) {
    for (const name of git(root, args).split("\0")) if (name && !isVendored(name) && existsSync(join(root, name))) names.add(name);
  }
  return [...names].sort(compare);
}

const JSX_TEXT_BEFORE = /<[A-Za-z][^<>]*>[^<>'"`{}\n]*$/;
const REGEX_MAX = 500;
const REGEX_AFTER_CHARS = "(,=:[!&|?{};+-*%<>^~";
const REGEX_AFTER_WORD = /\b(?:return|case|typeof|void|delete|in|of|instanceof|throw|yield|await|else|do)$/;
const REGEX_AFTER_PAREN = /\b(?:if|while|for)\s*$/;

function skipTemplate(text, i) {
  const stack = [{ expression: false, depth: 0 }];
  while (i < text.length) {
    const top = stack[stack.length - 1];
    const c = text[i];
    if (!top.expression) {
      if (c === "\\") i += 2;
      else if (c === "`") {
        stack.pop();
        i++;
        if (!stack.length) return i;
      } else if (text.startsWith("${", i)) {
        stack.push({ expression: true, depth: 1 });
        i += 2;
      } else i++;
    } else if (text.startsWith("//", i)) {
      const end = text.indexOf("\n", i);
      i = end < 0 ? text.length : end;
    } else if (text.startsWith("/*", i)) {
      const end = text.indexOf("*/", i + 2);
      i = end < 0 ? text.length : end + 2;
    } else if (c === "/") {
      const end = regexEnd(text, i, text.slice(Math.max(0, i - 200), i));
      i = end < 0 ? i + 1 : end;
    } else if (c === "`") {
      stack.push({ expression: false, depth: 0 });
      i++;
    } else if (c === '"' || c === "'") {
      for (i++; i < text.length && text[i] !== c && text[i] !== "\n"; i++) if (text[i] === "\\") i++;
      i++;
    } else if (c === "}" && --top.depth === 0) {
      stack.pop();
      i++;
    } else {
      if (c === "{") top.depth++;
      i++;
    }
  }
  return text.length;
}

function closesControlHead(head) {
  let depth = 0;
  for (let i = head.length - 1; i >= 0; i--) {
    if (head[i] === ")") depth++;
    else if (head[i] === "(" && --depth === 0) return REGEX_AFTER_PAREN.test(head.slice(0, i));
  }
  return false;
}

function regexAllowed(before) {
  const head = before.trimEnd();
  const last = head.slice(-1);
  if (before.slice(before.lastIndexOf("\n") + 1).trim() === "") return true;
  if (last === ")") return closesControlHead(head);
  if ("+-".includes(last) && head.slice(-2) === last + last) return false;
  if (last === "<" && head.length === before.length) return false;
  return REGEX_AFTER_CHARS.includes(last) || REGEX_AFTER_WORD.test(head);
}

function regexEnd(text, i, before) {
  if (!regexAllowed(before)) return -1;
  let inClass = false;
  for (let j = i + 1; j < text.length && j < i + REGEX_MAX && text[j] !== "\n"; j++) {
    if (text[j] === "\\") j++;
    else if (text[j] === "[") inClass = true;
    else if (text[j] === "]") inClass = false;
    else if (text[j] === "/" && !inClass) {
      let end = j + 1;
      while (/[a-z]/.test(text[end] ?? "")) end++;
      return end;
    }
  }
  return -1;
}

export function* sourceSegments(text, language) {
  const python = language === ".py";
  const javascript = language === "js";
  const quotes = language === ".rs" ? '"' : javascript || language === ".go" ? "\"'`" : "\"'";
  let tail = "";
  let i = 0;
  while (i < text.length) {
    let end = -1;
    let kind = "string";
    if (python ? text[i] === "#" : text.startsWith("//", i)) {
      end = text.indexOf("\n", i);
      if (end < 0) end = text.length;
      kind = "comment";
    } else if (!python && text.startsWith("/*", i)) {
      end = text.indexOf("*/", i + 2);
      end = end < 0 ? text.length : end + 2;
      kind = "comment";
    } else if (javascript && text[i] === "/") {
      end = regexEnd(text, i, tail);
    } else if (javascript && text[i] === "`") {
      end = skipTemplate(text, i + 1);
    } else if (javascript && text[i] === "'" && /\w/.test(text[i - 1] ?? "") && /[A-Za-z]/.test(text[i + 1] ?? "") && JSX_TEXT_BEFORE.test(text.slice(Math.max(0, i - 200), i))) {
      end = -1;
    } else if (quotes.includes(text[i])) {
      const delimiter = python && text.startsWith(text[i].repeat(3), i) ? text[i].repeat(3) : text[i];
      const multiline = delimiter.length > 1 || delimiter === "`";
      let j = i + delimiter.length;
      while (j < text.length && !text.startsWith(delimiter, j) && (multiline || text[j] !== "\n")) j += text[j] === "\\" ? 2 : 1;
      end = text.startsWith(delimiter, j) ? j + delimiter.length : Math.min(j, text.length);
    }
    if (end < 0) {
      tail = (tail + text[i]).slice(-200);
      yield { kind: "code", start: i, end: i + 1, piece: text[i] };
      i++;
    } else {
      const piece = kind === "comment" ? " " : " 0 ";
      tail = (tail + piece).slice(-200);
      yield { kind, start: i, end, piece };
      i = end;
    }
  }
}

export function stripSource(text, language) {
  let out = "";
  for (const segment of sourceSegments(text, language)) out += segment.piece;
  return out;
}

export function maskSource(text, language) {
  const inString = new Uint8Array(text.length);
  let code = "";
  for (const { kind, start, end } of sourceSegments(text, language)) {
    if (kind === "comment") code += text.slice(start, end).replace(/[^\n]/g, " ");
    else {
      code += text.slice(start, end);
      if (kind === "string") inString.fill(1, start + 1, end);
    }
  }
  return { code, inString };
}
