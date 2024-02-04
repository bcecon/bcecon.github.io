window.onload = function() {
    document.querySelectorAll('a').forEach(function(link) {
        link.setAttribute('target', '_blank');
    });
};
