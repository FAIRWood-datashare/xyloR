// =====================================================
// MINI REACT FIBER RUNTIME (FULL CLEAN IMPLEMENTATION)
// =====================================================

const TEXT_ELEMENT = "TEXT_ELEMENT"

// =====================================================
// 1. VIRTUAL DOM
// =====================================================

function h(type, props, ...children) {
  return {
    type,
    props: {
      ...props,
      children: children
        .flat()
        .map(c =>
          typeof c === "object" ? c : createTextElement(c)
        )
    }
  }
}

function createTextElement(text) {
  return {
    type: TEXT_ELEMENT,
    props: {
      nodeValue: text,
      children: []
    }
  }
}

// =====================================================
// 2. FIBER STATE
// =====================================================

let nextUnitOfWork = null
let wipRoot = null
let currentRoot = null

// =====================================================
// 3. DOM CREATION
// =====================================================

function createDom(fiber) {
  const dom =
    fiber.type === TEXT_ELEMENT
      ? document.createTextNode("")
      : document.createElement(fiber.type)

  updateDom(dom, {}, fiber.props)
  return dom
}

// =====================================================
// 4. DOM DIFF (minimal React-like behavior)
// =====================================================

function updateDom(dom, prevProps, nextProps) {

  const isEvent = k => k.startsWith("on")
  const isProp = k => k !== "children" && !isEvent(k)

  // remove old
  Object.keys(prevProps).forEach(name => {
    if (isEvent(name)) {
      const event = name.toLowerCase().substring(2)
      dom.removeEventListener(event, prevProps[name])
    }

    if (isProp(name)) {
      dom[name] = ""
    }
  })

  // add new
  Object.keys(nextProps).forEach(name => {
    if (isEvent(name)) {
      const event = name.toLowerCase().substring(2)
      dom.addEventListener(event, nextProps[name])
    }

    if (isProp(name)) {
      dom[name] = nextProps[name]
    }
  })
}

// =====================================================
// 5. RECONCILIATION ENGINE
// =====================================================

function reconcileChildren(wipFiber, elements) {
  let index = 0
  let oldFiber = wipFiber.alternate?.child
  let prevSibling = null

  while (index < elements.length || oldFiber) {

    const element = elements[index]
    let newFiber = null

    const sameType =
      oldFiber &&
      element &&
      oldFiber.type === element.type

    if (sameType) {
      newFiber = {
        type: oldFiber.type,
        props: element.props,
        dom: oldFiber.dom,
        parent: wipFiber,
        alternate: oldFiber
      }
    }

    if (element && !sameType) {
      newFiber = {
        type: element.type,
        props: element.props,
        dom: null,
        parent: wipFiber
      }
    }

    if (oldFiber && !sameType) {
      oldFiber = oldFiber.sibling
      index++
      continue
    }

    if (index === 0) {
      wipFiber.child = newFiber
    } else if (element) {
      prevSibling.sibling = newFiber
    }

    prevSibling = newFiber
    oldFiber = oldFiber?.sibling
    index++
  }
}

// =====================================================
// 6. SCHEDULER (work loop)
// =====================================================

function workLoop(deadline) {
  let shouldYield = false

  while (nextUnitOfWork && !shouldYield) {
    nextUnitOfWork = performUnitOfWork(nextUnitOfWork)
    shouldYield = deadline.timeRemaining() < 1
  }

  if (!nextUnitOfWork && wipRoot) {
    commitRoot()
  }

  requestIdleCallback(workLoop)
}

requestIdleCallback(workLoop)

// =====================================================
// 7. UNIT OF WORK
// =====================================================

function performUnitOfWork(fiber) {

  const isFunctionComponent =
    typeof fiber.type === "function"

  if (isFunctionComponent) {
    updateFunctionComponent(fiber)
  } else {
    updateHostComponent(fiber)
  }

  if (fiber.child) return fiber.child

  let next = fiber
  while (next) {
    if (next.sibling) return next.sibling
    next = next.parent
  }

  return null
}

// =====================================================
// 8. COMPONENT HANDLING
// =====================================================

function updateFunctionComponent(fiber) {
  const children = [fiber.type(fiber.props)]
  reconcileChildren(fiber, children)
}

function updateHostComponent(fiber) {
  if (!fiber.dom) {
    fiber.dom = createDom(fiber)
  }

  reconcileChildren(fiber, fiber.props.children)
}

// =====================================================
// 9. COMMIT PHASE
// =====================================================

function commitRoot() {
  commitWork(wipRoot.child)
  currentRoot = wipRoot
  wipRoot = null
}

function commitWork(fiber) {
  if (!fiber) return

  let parentFiber = fiber.parent
  while (!parentFiber.dom) {
    parentFiber = parentFiber.parent
  }

  const parentDom = parentFiber.dom

  if (fiber.dom) {
    parentDom.appendChild(fiber.dom)
  }

  commitWork(fiber.child)
  commitWork(fiber.sibling)
}

// =====================================================
// 10. PUBLIC RENDER API
// =====================================================

function render(element, container) {
  wipRoot = {
    dom: container,
    props: { children: [element] },
    alternate: currentRoot
  }

  nextUnitOfWork = wipRoot
}

// =====================================================
// 11. SAMPLE COMPONENTS
// =====================================================

function App() {
  return h(
    "div",
    null,

    h("h1", null, "⚛️ Mini React Fiber Runtime"),

    h("button", {
      onclick: () => alert("clicked")
    }, "Click Me"),

    h("p", null, "Fiber reconciliation is running"),
  )
}

// =====================================================
// 12. BOOTSTRAP
// =====================================================

window.addEventListener("load", () => {
  const root = document.getElementById("app")
  render(h(App, null), root)

  console.log("🚀 FULL Fiber Runtime Ready")
})