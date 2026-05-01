// =====================================================
// 0. SESSION SYSTEM
// =====================================================
const SESSION_KEY = "engine_session_v1"
const CURRENT_SESSION_VERSION = 1

// 🧠 MIGRATION PIPELINE (AUTO RUNNER)
const SESSION_MIGRATIONS = {
  1: (data) => data
}

// =====================================================
// AUTO MIGRATION ENGINE
// =====================================================
function runMigrations(data) {
  let version = data.version ?? 0

  while (version < CURRENT_SESSION_VERSION) {
    const nextVersion = version + 1
    const migrateFn = SESSION_MIGRATIONS[nextVersion]

    if (typeof migrateFn === "function") {
      data = migrateFn(data)
    }

    version = nextVersion
    data.version = version
  }

  return data
}

// =====================================================
// SAVE SESSION
// =====================================================
function saveSession(ctx, history) {
  const data = {
    version: CURRENT_SESSION_VERSION,
    engine: ctx.getEngine(),
    history: history.getAll(),
    index: history.getIndex()
  }

  localStorage.setItem(SESSION_KEY, JSON.stringify(data))
}

// =====================================================
// LOAD SESSION
// =====================================================
function loadSession(ctx, history) {
  const raw = localStorage.getItem(SESSION_KEY)
  if (!raw) return

  try {
    let data = JSON.parse(raw)

    data = runMigrations(data)

    if (data.engine) ctx.setEngine(data.engine)

    if (Array.isArray(data.history)) {
      data.history.forEach(h => history._pushRaw(h))
    }

    if (typeof data.index === "number") {
      history._setIndex(data.index)
    }

  } catch (e) {
    console.log("SESSION LOAD FAILED", e)
  }
}

// =====================================================
// DEBOUNCE + AUTO SAVE
// =====================================================
function debounce(fn, delay = 500) {
  let t
  return (...args) => {
    clearTimeout(t)
    t = setTimeout(() => fn(...args), delay)
  }
}

let autoSaveSession

function setupAutoSave(ctx, history) {
  autoSaveSession = debounce(() => {
    saveSession(ctx, history)
  }, 500)
}

// =====================================================
// ENGINE CONTEXT
// =====================================================
function createCtx() {
  const dirtyScopes = new Set()
  const subscribers = new Set()

  let engine = {}
  const dependencies = {}

  return {
    invalidate(scope) {
      dirtyScopes.add(scope)
    },

    registerDependencies(map) {
      Object.keys(dependencies).forEach(k => delete dependencies[k])
      Object.assign(dependencies, map)
    },

    getEngine() {
      return engine
    },

    setEngine(next) {
      engine = next
      subscribers.forEach(fn => fn())
    },

    subscribe(fn) {
      subscribers.add(fn)
    },

    _dirtyScopes: dirtyScopes,
    _dependencies: dependencies
  }
}

// =====================================================
// ENGINE REBUILD
// =====================================================
function rebuildEngine(ctx) {
  const dirty = ctx._dirtyScopes
  const deps = ctx._dependencies

  const prev = ctx.getEngine()
  const next = { ...prev }

  let changed = false

  for (const key in deps) {
    const scopes = Array.isArray(deps[key]) ? deps[key] : []

    const shouldRebuild = scopes.some(s => dirty.has(s))

    if (shouldRebuild) {
      next[key] = {
        value: (prev[key]?.value ?? 0) + 1,
        updatedAt: new Date().toISOString()
      }
      changed = true
    }
  }

  if (changed) ctx.setEngine(next)

  dirty.clear()
}

// =====================================================
// HISTORY
// =====================================================
function createHistory() {
  const snapshots = []
  let index = -1

  return {
    save(entry) {
      snapshots.splice(index + 1)
      snapshots.push(JSON.parse(JSON.stringify(entry)))
      index++
    },

    undo() {
      if (index <= 0) return null
      index--
      return snapshots[index]
    },

    redo() {
      if (index >= snapshots.length - 1) return null
      index++
      return snapshots[index]
    },

    getAll() {
      return snapshots
    },

    getIndex() {
      return index
    },

    goTo(i) {
      if (i < 0 || i >= snapshots.length) return null
      index = i
      return snapshots[index]
    },

    _pushRaw(entry) {
      snapshots.push(entry)
    },

    _setIndex(i) {
      index = i
    }
  }
}

// =====================================================
// DIFF ENGINE
// =====================================================
function diffStates(prev = {}, next = {}) {
  const diff = { added: [], removed: [], updated: [] }

  const keys = new Set([...Object.keys(prev), ...Object.keys(next)])

  for (const k of keys) {
    if (!(k in prev)) diff.added.push(k)
    else if (!(k in next)) diff.removed.push(k)
    else if (JSON.stringify(prev[k]) !== JSON.stringify(next[k])) {
      diff.updated.push(k)
    }
  }

  return diff
}

function getHistoryDiff(history, a, b) {
  const A = history.getAll()[a]
  const B = history.getAll()[b]

  if (!A || !B) return null

  return diffStates(A.state, B.state)
}

function renderDiff(a, b) {
  const el = document.getElementById("diff")
  if (!el) return

  const diff = getHistoryDiff(window.history, a, b)
  if (!diff) return

  el.textContent =
    "➕ Added: " + diff.added.join(", ") + "\n" +
    "🔁 Updated: " + diff.updated.join(", ") + "\n" +
    "❌ Removed: " + diff.removed.join(", ")
}

// =====================================================
// INIT
// =====================================================
const ctx = createCtx()
const history = createHistory()

ctx.registerDependencies({
  engineA: ["user"],
  engineB: ["settings"]
})

loadSession(ctx, history)
setupAutoSave(ctx, history)

// =====================================================
// RENDER
// =====================================================
function render() {
  const el = document.getElementById("output")
  if (!el) return

  el.textContent = JSON.stringify(ctx.getEngine(), null, 2)
}

// =====================================================
// HISTORY RENDER
// =====================================================
function renderHistory() {
  const el = document.getElementById("history")
  if (!el) return

  el.innerHTML = ""

  history.getAll().forEach((item, i) => {
    const div = document.createElement("div")
    div.textContent = i + " → " + item.action

    div.onclick = () => {
      ctx.setEngine(item.state)
      history.goTo(i)

      render()
      renderHistory()

      if (i > 0) renderDiff(i - 1, i)
    }

    el.appendChild(div)
  })
}

// =====================================================
// ACTIONS
// =====================================================
function runAction(scope) {
  ctx.invalidate(scope)
  rebuildEngine(ctx)

  history.save({
    action: scope,
    state: ctx.getEngine()
  })

  render()
  renderHistory()

  saveSession(ctx, history)
  if (autoSaveSession) autoSaveSession()
}

function onUserChange() {
  runAction("user")
}

function onSettingsChange() {
  runAction("settings")
}

// =====================================================
// UNDO / REDO
// =====================================================
function undo() {
  const s = history.undo()
  if (!s) return

  ctx.setEngine(s.state)
  render()
  renderHistory()
}

function redo() {
  const s = history.redo()
  if (!s) return

  ctx.setEngine(s.state)
  render()
  renderHistory()
}

// =====================================================
// INIT UI
// =====================================================
window.addEventListener("load", () => {
  render()
  renderHistory()
})

// =====================================================
// EXPORT
// =====================================================
window.onUserChange = onUserChange
window.onSettingsChange = onSettingsChange
window.undo = undo
window.redo = redo
window.saveSession = () => saveSession(ctx, history)

window.ctx = ctx
window.history = history