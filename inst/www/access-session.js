(function () {
  'use strict';
  if (window.datapondAccessSessionInitialized) return;
  window.datapondAccessSessionInitialized = true;

  // Attach to each table before its first Ajax request, including dynamic tabs.
  $(document).on('preInit.dt.datapondAccess', function (event, settings) {
    $(settings.nTable).on('xhr.dt.datapondAccess', function (event, settings, json, xhr) {
      if (json === null && xhr && xhr.status === 403 &&
          xhr.getResponseHeader('X-DataPond-Access') === 'denied') {
        // The session gate deliberately denies old/revoked table URLs. The
        // login transition handles this; retain 403 and never return cached data.
        return true;
      }
      // All other errors keep DataTables' normal reporting.
    });
  });

  // A renderUI screen replacement unbinds its outputs before removing them.
  // Abort outstanding requests so detached tables cannot interrupt sign-in.
  $(document).on('shiny:unbound.datapondAccess', function (event) {
    if (!$.fn.dataTable || !event.target.querySelectorAll) return;
    event.target.querySelectorAll('table.dataTable').forEach(function (table) {
      if (!$.fn.dataTable.isDataTable(table)) return;
      var settings = $(table).DataTable().settings()[0];
      var request = settings && settings.jqXHR;
      if (request && request.readyState !== 4) request.abort();
    });
  });
})();
