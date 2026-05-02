export function initBridge(App) {

  if (!window.Shiny) return

  Shiny.addCustomMessageHandler("engine_sync", (msg) => {

    if (msg.engine) {
      App.state.engine = msg.engine
    }

    if (msg.stage) {
      App.state.activeTab = msg.stage
    }

    App.render()
  })
}