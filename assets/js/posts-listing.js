navbar = document.querySelector('.navbar')
navbar.classList.remove('navbar-expand-lg')
navbar.classList.add('navbar-expand-md')
title = document.querySelector('.quarto-title-block .title').innerHTML
document.querySelector('.navbar-container').insertAdjacentHTML(
  'afterbegin', '<div class="navbar-title-container me-auto"><ul class="navbar-nav navbar-nav-scroll"><li class="nav-item"><span class="nav-link">'+title+'</span></li></ul></div>'
);
