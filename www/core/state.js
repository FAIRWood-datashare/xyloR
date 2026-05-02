window.AppState = (() => {

  let state = {
    engine: {},
    activeTab: "dashboard",
    cmd: {}
  }

  const listeners = new Set()

  // 🔥 SET FULL STATE (immutable)
  function set(next) {
    state = JSON.parse(JSON.stringify(next))
    notify()
  }

  // 🔥 SAFE PATCH (recommended for future use)
  function patch(partial) {
    state = JSON.parse(JSON.stringify({
      ...state,
      ...partial
    }))
    notify()
  }

  function get() {
    return JSON.parse(JSON.stringify(state)) // safer for debugging
  }

  function subscribe(fn) {
    listeners.add(fn)
    return () => listeners.delete(fn)
  }

  function notify() {
    listeners.forEach(fn => fn())
  }

  function init() {}

  return {
    set,
    patch,   // 🔥 ADD THIS
    get,
    subscribe,
    init
  }

})()