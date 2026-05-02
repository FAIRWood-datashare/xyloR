// =====================================================
// 1. APP STATE (single source of truth)
// =====================================================

const state = {
  engine: {},
  activeTab: "dashboard",

  cmd: {
    open: false,
    query: "",
    index: 0
  },

  _cmdList: []
}

// =====================================================
// 2. RENDER SCHEDULER
// =====================================================

const scheduler = {
  raf: null,

  schedule(fn) {
    if (this.raf) return

    this.raf = requestAnimationFrame(() => {
      this.raf = null
      fn()
    })
  }
}

// =====================================================
// 3. STATE UPDATE (single entry point)
// =====================================================

function setState(patch) {
  if (patch.cmd) {
    state.cmd = { ...state.cmd, ...patch.cmd }
    delete patch.cmd
  }

  Object.assign(state, patch)
  scheduler.schedule(render)
}

// =====================================================
// 4. ENGINE
// =====================================================

const engine = {
  invalidate(scope) {
    state.engine[scope] = (state.engine[scope] || 0) + 1
  }
}

// =====================================================
// 5. HISTORY
// =====================================================

const appHistory = {
  items: [],
  index: -1,

  push(entry) {
    this.items.splice(this.index + 1)
    this.items.push(entry)
    this.index++
  },

  getAll() {
    return this.items
  }
}

// =====================================================
// 6. COMMAND SYSTEM
// =====================================================

const usage = new Map()

function record(id) {
  usage.set(id, (usage.get(id) || 0) + 1)
}

const COMMANDS = [
  {
    id: "dashboard",
    label: "Go to Dashboard",
    category: "navigate",
    run: () => setState({ activeTab: "dashboard" })
  },
  {
    id: "engine",
    label: "Go to Engine",
    category: "navigate",
    run: () => setState({ activeTab: "engine" })
  },
  {
    id: "history",
    label: "Go to History",
    category: "navigate",
    run: () => setState({ activeTab: "history" })
  },
  {
    id: "debug",
    label: "Go to Debug",
    category: "navigate",
    run: () => setState({ activeTab: "debug" })
  },
  {
    id: "user",
    label: "User Action",
    category: "action",
    run: () => runAction("user")
  },
  {
    id: "settings",
    label: "Settings Action",
    category: "action",
    run: () => runAction("settings")
  }
]

// =====================================================
// 7. SCORING ENGINE
// =====================================================

function score(cmd, q) {
  if (!q) return 1

  const query = q.toLowerCase()
  const text = cmd.label.toLowerCase()

  let s = 0

  if (text === query) s += 200
  if (text.startsWith(query)) s += 120
  if (text.includes(query)) s += 60

  let qi = 0
  for (let i = 0; i < text.length; i++) {
    if (text[i] === query[qi]) {
      s += 4
      qi++
    }
    if (qi >= query.length) break
  }

  s += (usage.get(cmd.id) || 0) * 6

  return s
}

function getCommands() {
  const q = state.cmd.query

  return COMMANDS
    .map(c => ({ c, s: score(c, q) }))
    .filter(x => x.s > 0)
    .sort((a, b) => b.s - a.s)
    .map(x => x.c)
}

// =====================================================
// 8. ACTIONS
// =====================================================

function runAction(scope) {
  engine.invalidate(scope)

  appHistory.push({
    action: scope,
    state: { ...state.engine }
  })
}

// =====================================================
// 9. COMMAND EXECUTION
// =====================================================

function runCommand(cmd) {
  if (!cmd) return

  record(cmd.id)
  cmd.run()

  setState({
    cmd: { open: false, query: "", index: 0 }
  })
}

function runCommandById(id) {
  const cmd = COMMANDS.find(c => c.id === id)
  if (cmd) runCommand(cmd)
}

window.runCommandById = runCommandById

// =====================================================
// 10. KEYBOARD (FIXED)
// =====================================================

window.addEventListener("keydown", (e) => {

  if (e.key === "k" && e.metaKey) {
    e.preventDefault()

    setState({
      cmd: {
        open: !state.cmd.open,
        query: "",
        index: 0
      }
    })
  }

  if (!state.cmd.open) return

  const list = state._cmdList

  if (e.key === "ArrowDown") {
    setState({
      cmd: {
        ...state.cmd,
        index: Math.min(state.cmd.index + 1, list.length - 1)
      }
    })
  }

  if (e.key === "ArrowUp") {
    setState({
      cmd: {
        ...state.cmd,
        index: Math.max(state.cmd.index - 1, 0)
      }
    })
  }

  if (e.key === "Enter") {
    runCommand(list[state.cmd.index])
  }

  if (e.key === "Escape") {
    setState({
      cmd: { open: false, query: "", index: 0 }
    })
  }
})

// =====================================================
// 11. RENDER PIPELINE
// =====================================================

function render() {
  renderSidebar()
  renderMain()
  renderInspector()
  renderCommandPalette()
}

// =====================================================
// 12. SIDEBAR
// =====================================================

const TABS = ["dashboard", "engine", "history", "debug"]

function renderSidebar() {
  const el = document.getElementById("sidebar")

  el.innerHTML = TABS.map(tab => `
    <div onclick="setTab('${tab}')"
      style="
        padding:10px;
        margin:4px 0;
        cursor:pointer;
        border-radius:8px;
        background:${state.activeTab === tab ? "#eef2ff" : "transparent"};
        font-weight:${state.activeTab === tab ? "600" : "400"};
      ">
      ${tab}
    </div>
  `).join("")
}

window.setTab = (t) => setState({ activeTab: t })

// =====================================================
// 13. MAIN VIEW
// =====================================================

function renderMain() {
  const el = document.getElementById("main")

  if (state.activeTab === "dashboard") {
    el.innerHTML = `<pre>${JSON.stringify(state.engine, null, 2)}</pre>`
  }

  if (state.activeTab === "engine") {
    el.innerHTML = `<pre>${JSON.stringify(state.engine, null, 2)}</pre>`
  }

  if (state.activeTab === "history") {
    el.innerHTML = appHistory.getAll()
      .map((h, i) => `<div>${i} → ${h.action}</div>`)
      .join("")
  }

  if (state.activeTab === "debug") {
    el.innerHTML = `<pre>${JSON.stringify(state, null, 2)}</pre>`
  }
}

// =====================================================
// 14. INSPECTOR
// =====================================================

function renderInspector() {
  const el = document.getElementById("inspector")

  el.innerHTML = `
    <h4>Inspector</h4>
    <pre>${JSON.stringify(state.engine, null, 2)}</pre>

    <div style="margin-top:10px;font-size:12px;opacity:0.6">
      Tab: ${state.activeTab}
    </div>
  `
}

// =====================================================
// 15. COMMAND PALETTE
// =====================================================

function renderCommandPalette() {
  const el = document.getElementById("cmd")

  if (!state.cmd.open) {
    el.style.display = "none"
    return
  }

  const list = getCommands()
  state._cmdList = list

  const selected = list[state.cmd.index]

  el.style.display = "block"

  el.innerHTML = `
    <div style="
      position:fixed;
      top:18%;
      left:50%;
      transform:translateX(-50%);
      width:700px;
      background:white;
      border:1px solid #e5e7eb;
      border-radius:14px;
      box-shadow:0 30px 80px rgba(0,0,0,0.25);
      overflow:hidden;
      font-family:system-ui;
    ">

      <input
        autofocus
        value="${state.cmd.query}"
        oninput="setState({ cmd: { query: this.value, index: 0 } })"
        placeholder="Search commands..."
        style="width:100%;padding:12px;border:none;outline:none;border-bottom:1px solid #eee;"
      />

      <div style="display:flex;height:320px">

        <div style="flex:1;overflow:auto">

          ${list.map((c,i)=>`
            <div
              onclick="runCommandById('${c.id}')"
              style="
                padding:10px;
                cursor:pointer;
                border-radius:8px;
                background:${i===state.cmd.index?'#e6f7ff':'transparent'};
              ">
              ${c.label}
            </div>
          `).join("")}

        </div>

        <div style="width:240px;background:#fafafa;padding:12px">

          ${selected ? `
            <div style="font-size:11px;opacity:0.5">PREVIEW</div>
            <div style="font-weight:600;margin-top:6px">${selected.label}</div>
            <div style="font-size:12px;margin-top:8px">
              ${selected.category}
            </div>
          ` : `<div style="opacity:0.5">No selection</div>`}

        </div>

      </div>
    </div>
  `
}

// =====================================================
// 16. INIT
// =====================================================

window.addEventListener("load", () => {
  render()
  console.log("🚀 Production Linear UI Ready (Final Fixed)")
})

// =====================================================
// 17. SHINY SAFE EXPORT (CLEAN)
// =====================================================

window.App = {
  state,
  engine,
  history: appHistory,
  cmd: {
    run: runCommand,
    runById: runCommandById
  },
  render,
  setState
}
