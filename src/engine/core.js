export function createEngineCore() {
  const dirtyScopes = new Set()
  let engine = {}
  const dependencies = {}

  function invalidate(scope) {
    dirtyScopes.add(scope)
  }

  function registerDependencies(map) {
    Object.keys(dependencies).forEach(k => delete dependencies[k])
    Object.assign(dependencies, map)
  }

  function getEngine() {
    return engine
  }

  function setEngine(next) {
    engine = next
  }

  function rebuild() {
    const prev = engine
    const next = { ...prev }

    let changed = false

    for (const key in dependencies) {
      const scopes = dependencies[key] || []

      if (scopes.some(s => dirtyScopes.has(s))) {
        next[key] = {
          value: (prev[key]?.value ?? 0) + 1,
          updatedAt: new Date().toISOString()
        }
        changed = true
      }
    }

    if (changed) setEngine(next)

    dirtyScopes.clear()

    return changed
  }

  return {
    invalidate,
    registerDependencies,
    getEngine,
    setEngine,
    rebuild
  }
}