import { createEngineCore } from "./core.js"
import { saveSession, loadSession } from "./session.js"

export function createEngineApp(history) {
  const engine = createEngineCore()

  function run(scope) {
    engine.invalidate(scope)
    engine.rebuild()

    history.save({
      action: scope,
      state: engine.getEngine()
    })
  }

  function init() {
    loadSession(engine, history)
  }

  function save() {
    saveSession(engine, history)
  }

  return {
    run,
    init,
    save,
    getState: engine.getEngine,
    engine
  }
}