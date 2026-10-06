const pairs = (value) => {
  if (Array.isArray(value)) {
    return Object.fromEntries(
      value.filter((p) => p && typeof p.name === "string").map((p) => [p.name, String(p.value ?? "")]),
    );
  }
  if (value && typeof value === "object") {
    return Object.fromEntries(Object.entries(value).map(([k, v]) => [k, String(v)]));
  }
  return {};
};

const clean = (name) => String(name).replace(/[^A-Za-z0-9_-]/g, "_");

const isObject = (v) => v !== null && typeof v === "object";

export const toPiConfig = (entry) => {
  const label = typeof entry?.name === "string" ? entry.name : "?";
  const skip = (why) => ({ skip: `MCP server ${label} skipped: ${why}` });
  if (!isObject(entry) || typeof entry.name !== "string" || !entry.name) return skip("no name");
  if (entry.type === "sse") return skip("pi supports stdio and streamable HTTP, not sse");
  const base = { exposure: "direct" };
  if (entry.url !== undefined && entry.url !== null && entry.url !== "") {
    if (typeof entry.url !== "string") return skip("url is not a string");
    if (entry.headers != null && !isObject(entry.headers)) return skip("headers are not an object or array");
    const headers = pairs(entry.headers);
    return {
      config: {
        ...base,
        url: entry.url,
        ...(Object.keys(headers).length ? { headers } : {}),
      },
    };
  }
  if (entry.command) {
    if (typeof entry.command !== "string") return skip("command is not a string");
    if (entry.args != null && !(Array.isArray(entry.args) && entry.args.every((a) => typeof a === "string"))) {
      return skip("args are not an array of strings");
    }
    if (entry.env != null && !isObject(entry.env)) return skip("env is not an object or array");
    if (entry.cwd != null && typeof entry.cwd !== "string") return skip("cwd is not a string");
    return {
      config: {
        ...base,
        command: entry.command,
        args: entry.args ?? [],
        env: pairs(entry.env),
        ...(entry.cwd ? { cwd: entry.cwd } : {}),
      },
    };
  }
  return skip("neither url nor command");
};

export const planServers = (list) => {
  const servers = [];
  const skipped = [];
  const used = new Set();
  for (const entry of list) {
    const planned = toPiConfig(entry);
    if (planned.skip) {
      skipped.push(planned.skip);
      continue;
    }
    const base = clean(entry.name);
    let name = base;
    for (let n = 2; used.has(name); n++) name = `${base}_${n}`;
    used.add(name);
    servers.push([name, planned.config]);
  }
  return { servers, skipped };
};

export const parseServers = (raw) => {
  if (!raw) return [];
  try {
    const list = JSON.parse(raw);
    return Array.isArray(list) ? list : [];
  } catch {
    return [];
  }
};

export default function (pi) {
  const { servers, skipped } = planServers(parseServers(process.env.AOB_PI_MCP_SERVERS));
  for (const [name, config] of servers) pi.registerMcpServer(name, config);
  if (skipped.length) {
    pi.on("session_start", (_event, ctx) => {
      for (const note of skipped) ctx.ui.notify(note, "warning");
    });
  }
}
