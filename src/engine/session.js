const KEY = "engine_session_v1"

export function saveSession(engine, history) {
  const data = {
    engine: engine.getEngine(),
    history: history.getAll(),
    index: history.getIndex()
  }

  localStorage.setItem(KEY, JSON.stringify(data))
}

export function loadSession(engine, history) {
  const raw = localStorage.getItem(KEY)
  if (!raw) return

  const data = JSON.parse(raw)

  if (data.engine) engine.setEngine(data.engine)

  if (Array.isArray(data.history)) {
    data.history.forEach(h => history.save(h))
  }
}