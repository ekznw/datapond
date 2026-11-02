(function () {
  'use strict';
  if (window.datapondGuideInitialized) return;
  window.datapondGuideInitialized = true;

  function activeTab(id) {
    var nav = document.getElementById(id);
    var link = nav && nav.querySelector('li.active > a[data-value]');
    return link ? link.getAttribute('data-value') : null;
  }

  function syncSelection() {
    var main = activeTab('main_dataset_tabs');
    var section = activeTab('dataset_edit_sections');
    document.querySelectorAll('.dataset-guide-step').forEach(function (card) {
      var active = card.getAttribute('data-guide-main-tab') === main &&
        (main !== 'dataset_workspace' || card.getAttribute('data-guide-section') === section);
      card.classList.toggle('is-active', active);
      var link = card.querySelector('a');
      if (!link) return;
      if (active) link.setAttribute('aria-current', 'step');
      else link.removeAttribute('aria-current');
    });
  }

  // Use the displayed tabs so guide clicks, status links and server navigation
  // agree. Nested tabs leave the enclosing guide step selected.
  $(document).on('shown.bs.tab.datapondGuide', function (event) {
    if ($(event.target).closest('#main_dataset_tabs, #dataset_edit_sections').length) syncSelection();
  });
  $(document).on('shiny:bound.datapondGuide', function (event) {
    var id = event.target && event.target.id;
    if (id === 'main_dataset_tabs' || id === 'dataset_edit_sections' ||
        (id && id.indexOf('guide_tab_') === 0)) syncSelection();
  });
  $(syncSelection);
})();
