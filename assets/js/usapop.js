// for non-mobile devices
if (typeof screen.orientation !== 'undefined') {
  window.addEventListener('load', function() {
    wh = usapop.contentDocument.children[0].getAttribute('viewBox').split(' ').slice(2)
  })
  window.onload = window.onresize = function () {
    const w = document.documentElement.clientWidth
    let h = w*(wh[1]/wh[0])
    if (h > document.documentElement.clientHeight) {
      h = document.documentElement.clientHeight
    }
    usapop.style.width = w + "px"
    usapop.style.height = h + "px"
  }
}
