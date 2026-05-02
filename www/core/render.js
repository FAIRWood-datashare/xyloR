window.AppRender = (() => {

  function init() {
    // reactive render hook
    AppState.subscribe(render)
  }

  function render() {
    renderMain()
  }

  function renderMain() {
    const state = AppState.get()
    const el = document.getElementById("main")

    if (!el) return

    el.innerHTML = `
      <h3>${state.activeTab}</h3>
      <pre>${JSON.stringify(state.engine, null, 2)}</pre>
      <small>lastUpdate: ${state.engine.lastUpdate ?? ""}</small>
    `
  }

  return {
    init,
    render
  }

})()