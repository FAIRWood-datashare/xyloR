export function createHistory() {
  const snapshots = []

  return {
    save(state) {
      snapshots.push(JSON.parse(JSON.stringify(state)))
    },

    getAll() {
      return snapshots
    },

    getLatest() {
      return snapshots[snapshots.length - 1]
    }
  }
}