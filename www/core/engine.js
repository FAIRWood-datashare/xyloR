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
        // timeline / history restore path
        AppState.set(payload)
        return

      default:
        console.warn("Unknown action:", type)
        return
    }

    // 🔥 FIX: store PREVIOUS state, not next
    if (window.AppHistory?.push) {
      AppHistory.push(JSON.parse(JSON.stringify(prev)))
    }

    // commit state
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