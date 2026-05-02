console.log("APP.JS START")

// =========================
// APP CORE
// =========================
window.App = {

  init() {
    console.log("APP INIT")

    window.AppState.init()
    window.AppRender.init()

    // important: initialize history AFTER state exists
    AppHistory.reset(AppState.get())

    // timeline init (if exists)
    window.AppTimeline?.init?.()

    this.render()
  },

  render() {
    window.AppRender.render()
  },

  // =========================
  // HISTORY API (clean wrapper)
  // =========================
  undo() {
    const prev = AppHistory.undo()
    if (!prev) return

    AppState.set(prev)
    this.render()
  },

  redo() {
    const next = AppHistory.redo()
    if (!next) return

    AppState.set(next)
    this.render()
  }
}


// =========================
// ENGINE (single source of truth for updates)
// =========================
window.AppEngine = (() => {

  function dispatch(type, payload) {

    const prev = AppState.get()

    let next = null

    switch (type) {

      case "SET_ENGINE":
        next = {
          ...prev,
          engine: {
            ...prev.engine,
            ...payload,
            lastUpdate: Date.now()
          }
        }
        break

      case "SET_TAB":
        next = {
          ...prev,
          activeTab: payload
        }
        break

      case "HYDRATE_STATE":
        AppState.set(payload)
        return

      default:
        console.warn("Unknown action:", type)
        return
    }

    // save history
    AppHistory.push(prev)

    // apply state
    AppState.set(next)
  }

  function invalidate(scope = "manual") {
    dispatch("SET_ENGINE", { scope })
  }

  return {
    dispatch,
    invalidate
  }

})()


// =========================
// TIMELINE (was incorrectly "lost inside app.js")
// =========================
window.AppTimeline = (() => {

  let isDragging = false

  function render() {
    const el = document.getElementById("timeline-inspector")
    if (!el || !window.AppHistory) return

    const history = AppHistory.all()
    const current = AppHistory.currentIndex()

    el.innerHTML = `
      <div style="padding:6px; font-size:12px;">

        <input
          id="timeline-slider"
          type="range"
          min="0"
          max="${Math.max(history.length - 1, 0)}"
          value="${current}"
          style="width:100%"
        />

        <div style="margin-top:6px;">
          step: ${current} / ${history.length - 1}
        </div>

      </div>
    `

    const slider = document.getElementById("timeline-slider")
    if (!slider) return

    // avoid duplicate bindings
    if (!slider.dataset.bound) {

      slider.dataset.bound = "true"

      slider.addEventListener("input", (e) => {
        isDragging = true

        const index = Number(e.target.value)
        scrub(index)
      })

      slider.addEventListener("change", (e) => {
        isDragging = false

        const index = Number(e.target.value)
        commit(index)
      })
    }
  }

  // 🔥 live preview (NO commit)
  function scrub(index) {
    const snap = AppHistory.jump(index)
    if (!snap) return

    AppEngine.dispatch("HYDRATE_STATE", snap)
    App.render()
  }

  // 🔥 final commit (sets pointer correctly)
  function commit(index) {
    const snap = AppHistory.jump(index)
    if (!snap) return

    AppEngine.dispatch("HYDRATE_STATE", snap)
    App.render()

    console.log("scrub commit →", index)
  }

  function init() {
    setInterval(render, 150)
  }

  return {
    init,
    render
  }

})()