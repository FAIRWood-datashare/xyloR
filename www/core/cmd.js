console.log("CMD LOADED")

window.AppCmd = (() => {

  function run(command, payload) {

    if (!window.AppEngine) {
      console.error("AppEngine not ready")
      return
    }

    switch (command) {

      case "engine:set":
        AppEngine.dispatch("SET_ENGINE", payload)
        break

      case "tab:set":
        AppEngine.dispatch("SET_TAB", payload)
        break

      case "engine:invalidate":
        AppEngine.invalidate(payload)
        break

      default:
        console.warn("Unknown command:", command, payload)
    }
  }

  return {
    run
  }

})()