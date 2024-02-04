brandContainer = document.querySelector('.navbar-brand-container')
brandContainer.classList.remove('mx-auto')
tools = document.querySelectorAll('[id$="quarto-navbar-tools-item"], .navbar .nav-link[href$="index.xml"]')
if (tools.length > 0) {
  // if a tool is already added, this places the rest before them
  tools = [...tools].reverse()
  tools.forEach((n, i) => {
    tools[i] = n.parentElement
  })
  d = document.querySelector('div.quarto-navbar-tools')
  tools.forEach(
    (n) => {
      inner = n.innerHTML
      ni = document.createElement('div')
      ni.classList.add('nav-item', 'dropdown')
      ni.insertAdjacentHTML('beforeend', inner)
      d.insertAdjacentElement('afterbegin', ni)
      n.remove()
    }
  )
  document.querySelector('.navbar-container').insertAdjacentElement(
    'beforeend', d
  )
}
scrollElement = document.querySelector('#quarto-back-to-top')
