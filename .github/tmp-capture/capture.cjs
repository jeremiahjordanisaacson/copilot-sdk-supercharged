const { spawn } = require("child_process");
const fs = require("fs"), os = require("os"), path = require("path"), http = require("http");
const repo = process.argv[2], sess = process.argv[3];
const capture = path.join(sess, "captured-request.json");
try { fs.unlinkSync(capture); } catch {}
const work = fs.mkdtempSync(path.join(os.tmpdir(), "cap-work-")), home = fs.mkdtempSync(path.join(os.tmpdir(), "cap-home-"));
const proxy = spawn(process.execPath, ["--import", "tsx", "server.ts"], { cwd: path.join(repo, "test", "harness"), env: { ...process.env, CAPTURE_MISS_FILE: capture }, stdio: ["ignore", "pipe", "pipe"] });
let cli; const done = (code) => { try { cli && cli.kill(); } catch {} try { proxy.kill(); } catch {} process.exit(code); };
setTimeout(() => { console.log("TIMEOUT"); done(1); }, 120000);
let sbuf = "";
proxy.stdout.on("data", async (d) => {
  sbuf += d; const m = /Listening: (\S+)\s*(\{.*\})?/.exec(sbuf); if (!m || cli) return;
  const url = m[1], meta = m[2] ? JSON.parse(m[2]) : {};
  await new Promise((res, rej) => { const body = JSON.stringify({ filePath: path.join(repo, "test/snapshots/session/should_have_stateful_conversation.yaml"), workDir: work }); const u = new URL(url + "/config"); const r = http.request({ hostname: u.hostname, port: u.port, path: u.pathname, method: "POST", headers: { "content-type": "application/json", "content-length": Buffer.byteLength(body) } }, (resp) => { resp.resume(); resp.on("end", res); }); r.on("error", rej); r.end(body); });
const token = process.env.GH_TOKEN || "fake-test-token"; await new Promise((res, rej) => { const body = JSON.stringify({ token, response: { login: "e2e-test-user", id: 12345, copilot_plan: "individual_pro", endpoints: { api: url, telemetry: "https://sdk-hydro-collector.example.com" }, analytics_tracking_id: "e2e-test-tracking-id" } }); const u = new URL(url + "/copilot-user-config"); const r = http.request({ hostname: u.hostname, port: u.port, path: u.pathname, method: "POST", headers: { "content-type": "application/json", "content-length": Buffer.byteLength(body) } }, (resp) => { resp.resume(); resp.on("end", res); }); r.on("error", rej); r.end(body); });
  const env = { ...process.env, COPILOT_API_URL: url, COPILOT_DEBUG_GITHUB_API_URL: url, COPILOT_HOME: home, GH_CONFIG_DIR: home, XDG_CONFIG_HOME: home, XDG_STATE_HOME: home, GH_TOKEN: token, GITHUB_TOKEN: token, COPILOT_SDK_AUTH_TOKEN: token };
  delete env.COPILOT_HMAC_KEY; delete env.CAPI_HMAC_KEY;
  if (meta.connectProxyUrl) { env.HTTPS_PROXY = env.https_proxy = meta.connectProxyUrl; env.NO_PROXY = env.no_proxy = "127.0.0.1,localhost"; }
  if (meta.caFilePath) { env.NODE_EXTRA_CA_CERTS = meta.caFilePath; env.SSL_CERT_FILE = meta.caFilePath; }
  cli = spawn(process.execPath, [path.join(repo, "nodejs/node_modules/@github/copilot/npm-loader.js"), "--headless", "--no-auto-update", "--stdio", "--auth-token-env", "COPILOT_SDK_AUTH_TOKEN", "--no-auto-login"], { cwd: work, env, stdio: ["pipe", "pipe", "pipe"] });
  cli.stderr.on("data", (e) => { if (process.env.SHOWERR) process.stderr.write("[cli] " + e); });
  let buf = Buffer.alloc(0), id = 0; const pending = new Map();
  const write = (o) => { const b = JSON.stringify(o); cli.stdin.write(`Content-Length: ${Buffer.byteLength(b)}\r\n\r\n${b}`); };
  cli.stdout.on("data", (d) => { buf = Buffer.concat([buf, d]); for (;;) { const s = buf.toString("latin1"); const i = s.indexOf("\r\n\r\n"); if (i < 0) return; const len = +/Content-Length: (\d+)/i.exec(s.slice(0, i))[1]; if (buf.length < i + 4 + len) return; const msg = JSON.parse(buf.slice(i + 4, i + 4 + len).toString("utf8")); buf = buf.slice(i + 4 + len); if (msg.id !== undefined && pending.has(msg.id) && !msg.method) { pending.get(msg.id)(msg); pending.delete(msg.id); } else if (msg.method && msg.id !== undefined) { write({ jsonrpc: "2.0", id: msg.id, error: { code: -32601, message: "not handled" } }); } } });
  const call = (method, params) => new Promise((r) => { const i = ++id; pending.set(i, r); write({ jsonrpc: "2.0", id: i, method, params }); });
  const ping = await call("ping", { message: "x" }); console.log("ping protocol:", ping.result && ping.result.protocolVersion);
  const created = await call("session.create", { model: "claude-sonnet-4.5", workingDirectory: work, events: { streaming: false } });
  if (created.error) { console.log("create error:", created.error.message); return done(1); }
  const sid = created.result.sessionId; console.log("session:", sid);
  const sent = await call("session.send", { sessionId: sid, prompt: "What is 1+1?" }); console.log("send:", JSON.stringify(sent.result || sent.error).slice(0, 200));
  for (let t = 0; t < 60; t++) { if (fs.existsSync(capture) && fs.statSync(capture).size > 0) break; await new Promise((r) => setTimeout(r, 1000)); }
  if (!fs.existsSync(capture)) { console.log("NO MISS CAPTURED (request may have matched)"); return done(0); }
  const req = JSON.parse(fs.readFileSync(capture, "utf8"));
  console.log("top-level keys:", Object.keys(req).join(", "));
  (req.messages || []).forEach((m, i) => { const c = typeof m.content === "string" ? m.content : JSON.stringify(m.content); console.log(`--- [${i}] role=${m.role} len=${(c || "").length} keys=${Object.keys(m).join(",")}`); console.log(m.role === "system" ? (c || "").slice(0, 300) : (c || "").slice(0, 4000)); }); console.log("tools:", (req.tools || []).length, "model:", req.model);
  done(0);
});
proxy.stderr.on("data", (e) => { if (process.env.SHOWERR) process.stderr.write("[proxy] " + e); });
