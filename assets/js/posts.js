title = document.querySelector('.quarto-title-block .title').innerHTML
codeTools = document.querySelectorAll('#quarto-code-tools-menu, #quarto-code-tools-menu+.dropdown-menu')
if (codeTools.length > 0) {
  codeTools[0].classList.add('ms-auto')
  codeTools = [...codeTools].reverse()
  d = document.createElement('div')
  d.classList.add('btn-group', 'quarto-code-tools')
  codeTools.forEach(
    (n) => d.insertAdjacentElement(
    'afterbegin', n
    )
  )
  document.querySelector('.navbar-container').insertAdjacentElement(
    'beforeend', d
  )
} else {
  // does something with #quarto-code-tools-source
}
document.querySelector('.navbar-brand-container').insertAdjacentHTML(
  'afterend', '<div class="navbar-title-container me-auto" style="display: inline-block;"><ul class="navbar-nav navbar-nav-scroll"><li class="nav-item"><span><font color="#dbd9d8">'+title+'</font></span></li></ul></div>'
);
reuseHTML = document.querySelector('#quarto-reuse > div').outerText
document.querySelector('#quarto-reuse > div').outerHTML = reuseHTML
