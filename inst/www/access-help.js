(function () {
  'use strict';
  if (window.datapondAccessHelpInitialized) return;
  window.datapondAccessHelpInitialized = true;

  function fallbackCopy(text) {
    var area = document.createElement('textarea');
    area.value = text;
    area.setAttribute('readonly', '');
    area.style.position = 'fixed';
    area.style.opacity = '0';
    document.body.appendChild(area);
    try {
      area.select();
      if (!document.execCommand('copy')) throw new Error('Copy unavailable');
    } finally {
      document.body.removeChild(area);
    }
  }

  document.addEventListener('click', function (event) {
    var button = event.target.closest('[data-copy-command]');
    if (!button) return;
    var source = document.getElementById(button.getAttribute('data-copy-command'));
    if (!source) return;
    var status = button.parentElement.querySelector('[role="status"]');
    var text = source.textContent;
    var copy = navigator.clipboard && window.isSecureContext ?
      navigator.clipboard.writeText(text) : Promise.reject(new Error('Clipboard unavailable'));
    copy.catch(function () { fallbackCopy(text); }).then(function () {
      status.textContent = 'Copied. Paste into your local R terminal.';
    }).catch(function () {
      status.textContent = 'Copy unavailable. Select and copy the command above.';
    }).finally(function () { button.focus(); });
  });
})();
