window.AppHistory = (() => {

  let stack = []
  let pointer = -1

  function reset(initialState = null) {
    stack = []
    pointer = -1

    if (initialState) {
      const snap = JSON.parse(JSON.stringify(initialState))
      stack.push(snap)
      pointer = 0
    }
  }

  function push(state) {
    const snap = JSON.parse(JSON.stringify(state))

    // cut future
    stack = stack.slice(0, pointer + 1)

    stack.push(snap)
    pointer = stack.length - 1
  }

  function undo() {
    if (stack.length === 0) return null
    if (pointer <= 0) return stack[0]

    pointer--
    return JSON.parse(JSON.stringify(stack[pointer]))
  }

  function redo() {
    if (stack.length === 0) return null
    if (pointer >= stack.length - 1) return stack[pointer]

    pointer++
    return JSON.parse(JSON.stringify(stack[pointer]))
  }

  function jump(index) {
    if (index < 0 || index >= stack.length) return null

    pointer = index
    return JSON.parse(JSON.stringify(stack[pointer]))
  }

  function currentIndex() {
    return pointer
  }

  function all() {
    return stack
  }

  // 🔥 NEW: safe accessor for timeline + debugging
  function getCurrent() {
    if (pointer < 0 || pointer >= stack.length) return null
    return JSON.parse(JSON.stringify(stack[pointer]))
  }

  return {
    reset,
    push,
    undo,
    redo,
    jump,
    all,
    currentIndex,
    getCurrent
  }

})()