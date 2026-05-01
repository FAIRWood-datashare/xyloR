export function createCtx() {
  const dirtyScopes = new Set()
  let engines = {}
  let dependencies = {}

  return {
    invalidate(scope) {
      dirtyScopes.add(scope)
    },

    registerDependencies(map) {
      dependencies = map
    },

    getEngine() {
      return engines
    },

    setEngine(next) {
      engines = next
    },

    _dirtyScopes: dirtyScopes,
    _dependencies: dependencies
  }
}