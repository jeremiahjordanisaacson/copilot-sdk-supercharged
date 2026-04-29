# Multiple Sessions - Nim

Patterns for managing multiple independent Copilot conversations simultaneously in Nim.

## Basic Multi-Session Setup

**Scenario:** Run two or more independent conversations at the same time, each with its own context.

```nim
import std/[asyncdispatch, json]
import copilot_sdk

proc main() {.async.} =
  let client = newCopilotClient()
  await client.start()
  defer: client.stop()

  let codeSession = await client.createSession(newSessionConfig(
    systemPrompt = "You are an expert Nim programmer. Provide concise code examples.",
  ))

  let docsSession = await client.createSession(newSessionConfig(
    systemPrompt = "You are a technical writer. Write clear, concise documentation.",
  ))

  let codeResp = await codeSession.sendAndWait(newMessageOptions(
    "Write a proc to reverse a sequence in Nim.",
  ))
  echo "Code:\n", codeResp.message

  let docsResp = await docsSession.sendAndWait(newMessageOptions(
    "Write docs for a sequence reversal proc.",
  ))
  echo "Docs:\n", docsResp.message

waitFor main()
```

## Session Registry

**Scenario:** Manage a dynamic pool of named sessions that can be looked up and created on demand.

```nim
import std/[asyncdispatch, json, tables]
import copilot_sdk

type
  SessionRegistry = ref object
    client: CopilotClient
    sessions: Table[string, CopilotSession]

proc newSessionRegistry(client: CopilotClient): SessionRegistry =
  SessionRegistry(client: client, sessions: initTable[string, CopilotSession]())

proc getOrCreate(reg: SessionRegistry; name: string;
                 config: SessionConfig): Future[CopilotSession] {.async.} =
  if not reg.sessions.hasKey(name):
    reg.sessions[name] = await reg.client.createSession(config)
    echo "Created session: ", name
  result = reg.sessions[name]

proc send(reg: SessionRegistry; name, message: string): Future[SendResult] {.async.} =
  if not reg.sessions.hasKey(name):
    raise newException(KeyError, "No session: " & name)
  result = await reg.sessions[name].sendAndWait(newMessageOptions(message))

proc list(reg: SessionRegistry): seq[string] =
  for k in reg.sessions.keys:
    result.add(k)

proc main() {.async.} =
  let client = newCopilotClient()
  await client.start()
  defer: client.stop()

  let registry = newSessionRegistry(client)

  discard await registry.getOrCreate("frontend", newSessionConfig(
    systemPrompt = "You are a frontend expert.",
  ))
  discard await registry.getOrCreate("backend", newSessionConfig(
    systemPrompt = "You are a backend expert.",
  ))

  let resp = await registry.send("frontend", "How do I lazy-load images?")
  echo resp.message
  echo "Active sessions: ", registry.list()

waitFor main()
```

## Per-User Sessions

**Scenario:** In a multi-user application, each user gets their own isolated session.

```nim
import std/[asyncdispatch, json, tables]
import copilot_sdk

type
  UserSessions = ref object
    client: CopilotClient
    defaultPrompt: string
    sessions: Table[string, CopilotSession]

proc newUserSessions(client: CopilotClient;
                     defaultPrompt: string): UserSessions =
  UserSessions(
    client: client,
    defaultPrompt: defaultPrompt,
    sessions: initTable[string, CopilotSession](),
  )

proc chat(us: UserSessions; userId, message: string): Future[SendResult] {.async.} =
  if not us.sessions.hasKey(userId):
    us.sessions[userId] = await us.client.createSession(newSessionConfig(
      systemPrompt = us.defaultPrompt & "\nYou are assisting user: " & userId,
    ))
  result = await us.sessions[userId].sendAndWait(newMessageOptions(message))

proc endSession(us: UserSessions; userId: string) =
  us.sessions.del(userId)

proc main() {.async.} =
  let client = newCopilotClient()
  await client.start()
  defer: client.stop()

  let userSessions = newUserSessions(client, "You are a coding assistant.")

  let resp1 = await userSessions.chat("alice", "How do I read a file in Nim?")
  echo "Alice got: ", resp1.message

  let resp2 = await userSessions.chat("bob", "How do I write to a file in Nim?")
  echo "Bob got: ", resp2.message

  userSessions.endSession("alice")

waitFor main()
```

## Round-Robin Across Sessions

**Scenario:** Distribute requests across multiple sessions to compare responses or balance load.

```nim
import std/[asyncdispatch, json]
import copilot_sdk

type
  RoundRobinPool = ref object
    sessions: seq[CopilotSession]
    index: int

proc newRoundRobinPool(sessions: seq[CopilotSession]): RoundRobinPool =
  RoundRobinPool(sessions: sessions, index: 0)

proc send(pool: RoundRobinPool;
          message: string): Future[(SendResult, int)] {.async.} =
  let idx = pool.index mod pool.sessions.len
  pool.index = idx + 1
  let resp = await pool.sessions[idx].sendAndWait(newMessageOptions(message))
  result = (resp, idx)

proc main() {.async.} =
  let client = newCopilotClient()
  await client.start()
  defer: client.stop()

  var sessions: seq[CopilotSession]
  for prompt in ["Be concise.", "Be detailed.", "Be creative."]:
    sessions.add(await client.createSession(newSessionConfig(
      systemPrompt = prompt,
    )))

  let pool = newRoundRobinPool(sessions)

  for q in ["What is a closure?", "Explain generics.", "What are macros?"]:
    let (resp, idx) = await pool.send(q)
    echo "Session ", idx, " answered '", q, "':\n", resp.message, "\n"

waitFor main()
```

## Best Practices

1. **One client, many sessions.** Create a single `CopilotClient` and spawn sessions from it to share the underlying connection.
2. **Use a registry or pool pattern** to manage session lifecycles and avoid losing references.
3. **Delete table entries** when sessions are no longer needed so memory is reclaimed.
4. **Give each session a distinct system prompt** so it stays focused on its purpose.
5. **Wrap session operations in try/except** to isolate failures in one session from affecting others.
6. **Avoid unbounded session creation.** Track active sessions and set a maximum limit for production use.
