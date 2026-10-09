import { existsSync, readFileSync, realpathSync } from "node:fs";
import { createRequire } from "node:module";
import { join } from "node:path";

const BUSY_MS = 1000;
const CO_CHANGE_TOP = 5;
const REQUIRED = {
  git_metadata: ["file_path", "commit_count_90d", "commit_count_total"],
  health_file_metrics: ["file_path", "score"],
  dead_code_findings: ["file_path", "kind"],
};

function openDatabase(path) {
  const emit = process.emitWarning;
  process.emitWarning = () => {};
  try {
    const { DatabaseSync } = createRequire(import.meta.url)("node:sqlite");
    const db = new DatabaseSync(path, { readOnly: true, timeout: BUSY_MS });
    db.exec(`PRAGMA busy_timeout = ${BUSY_MS}`);
    return db;
  } finally {
    process.emitWarning = emit;
  }
}

function columns(db, table) {
  return new Set(db.prepare(`PRAGMA table_info(${table})`).all().map((row) => row.name));
}

function sameDir(path, root) {
  try {
    return realpathSync(path) === root;
  } catch {
    return false;
  }
}

function repository(db, root) {
  if (!columns(db, "repositories").size) return null;
  const all = db.prepare("SELECT * FROM repositories").all();
  const local = (row) => row.local_path ?? row.path;
  if (all.length <= 1) {
    const only = all[0];
    if (only && typeof local(only) === "string" && local(only) && !sameDir(local(only), root)) throw new Error("repository belongs to another root");
    return only ?? null;
  }
  const row = all.find((candidate) => sameDir(local(candidate), root));
  if (!row) throw new Error("no repository matches root");
  return row;
}

function rows(db, table, cols, repo) {
  const have = columns(db, table);
  const picked = cols.filter((col) => have.has(col));
  const where = repo !== null && have.has("repository_id") ? " WHERE repository_id = ?" : "";
  return db.prepare(`SELECT ${picked.join(", ")} FROM ${table}${where}`).all(...(where ? [repo] : []));
}

function coChange(json) {
  let partners;
  try {
    partners = JSON.parse(json);
  } catch {
    return [];
  }
  if (!Array.isArray(partners)) return [];
  return partners
    .map((p) => ({ path: p?.file_path, weight: Number(p?.frequency ?? p?.co_change_count) }))
    .filter((p) => typeof p.path === "string" && p.weight > 0)
    .sort((a, b) => b.weight - a.weight || (a.path < b.path ? -1 : 1))
    .slice(0, CO_CHANGE_TOP)
    .map((p) => ({ path: p.path, weight: Number(p.weight.toFixed(4)) }));
}

function indexedCommit(dir, repo) {
  try {
    const commit = JSON.parse(readFileSync(join(dir, "state.json"), "utf8")).last_sync_commit;
    if (typeof commit === "string" && commit) return commit;
  } catch {}
  const head = repo?.head_commit;
  return typeof head === "string" && head ? head : null;
}

export function readRepowise(root) {
  const dir = join(root, ".repowise");
  const path = join(dir, "wiki.db");
  if (!existsSync(path)) return null;
  let db;
  try {
    db = openDatabase(path);
    for (const [table, needed] of Object.entries(REQUIRED)) {
      const have = columns(db, table);
      if (needed.some((col) => !have.has(col))) return null;
    }
    const repoRow = repository(db, realpathSync(root));
    const repo = repoRow?.id ?? null;
    const files = new Map();
    const entry = (file) => {
      if (!files.has(file)) files.set(file, { commits_90d: 0, commits_total: 0, hotspot: false, health: null, dead_code: [], co_change: [] });
      return files.get(file);
    };
    for (const row of rows(db, "git_metadata", ["file_path", "commit_count_90d", "commit_count_total", "is_hotspot", "co_change_partners_json"], repo)) {
      Object.assign(entry(row.file_path), {
        commits_90d: Number(row.commit_count_90d) || 0,
        commits_total: Number(row.commit_count_total) || 0,
        hotspot: Boolean(row.is_hotspot),
        co_change: row.co_change_partners_json ? coChange(row.co_change_partners_json) : [],
      });
    }
    for (const row of rows(db, "health_file_metrics", ["file_path", "score"], repo)) {
      if (row.score !== null && Number.isFinite(Number(row.score))) entry(row.file_path).health = Number(row.score);
    }
    for (const row of rows(db, "dead_code_findings", ["file_path", "kind"], repo)) {
      const kinds = entry(row.file_path).dead_code;
      if (!kinds.includes(row.kind)) kinds.push(row.kind);
    }
    for (const info of files.values()) info.dead_code.sort();
    return { indexed_commit: indexedCommit(dir, repoRow), files };
  } catch {
    return null;
  } finally {
    try {
      db?.close();
    } catch {}
  }
}
