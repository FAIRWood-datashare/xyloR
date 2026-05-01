export function rebuildEngine(ctx) {
  const dirty = ctx._dirtyScopes
  const deps = ctx._dependencies

  const next = { ...ctx.getEngine() }

  let changed = false

  for (const key in deps) {
    const shouldRebuild = deps[key].some(s => dirty.has(s))

    if (shouldRebuild) {
      next[key] = `rebuild:${key}`
      changed = true
    }
  }

  if (changed) {
    ctx.setEngine(next)
  }

  dirty.clear()
}