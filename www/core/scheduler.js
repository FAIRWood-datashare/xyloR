window.AppScheduler = (() => {

  let raf = null

  function schedule(fn) {
    if (raf) return

    raf = requestAnimationFrame(() => {
      raf = null
      fn()
    })
  }

  return {
    schedule
  }

})()