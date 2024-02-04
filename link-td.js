window.onload = function() {
    document.querySelectorAll('td a').forEach(function(link) {
        link.setAttribute('target', '_blank');
    });
};
