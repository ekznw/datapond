(function () {
  'use strict';
  if (window.datapondTableTools) return;

  var historyWidths = [0, 0, 140, 120, 110, 140, 140, 110, 220, 220, 130, 220];
  function settings(table) { return table.settings()[0]; }
  function state(table) { return settings(table)._datapondTools; }
  function key(column) {
    var match = column.sClass.match(/metadata-history-width-\d+/);
    return match ? match[0] : column._datapondKey;
  }
  function publicColumns(table) {
    return settings(table).aoColumns.filter(function (column) { return column._datapondPublic; });
  }
  function columnApi(table, column) {
    return table.column(settings(table).aoColumns.indexOf(column));
  }
  function label(column) { return column.nTh.textContent.trim() || 'Column ' + (column.idx + 1); }
  function decorate(table) {
    if (state(table).history) return;
    publicColumns(table).forEach(function (column) {
      var width = state(table).widths[key(column)];
      columnApi(table, column).nodes().each(function (cell) {
        var wrapper = cell.querySelector(':scope > .datapond-table-cell-content');
        if (width) {
          if (!wrapper) {
            wrapper = document.createElement('div');
            wrapper.className = 'datapond-table-cell-content';
            // Move existing nodes, preserving links, controls and event handlers.
            while (cell.firstChild) wrapper.appendChild(cell.firstChild);
            cell.appendChild(wrapper);
          }
          wrapper.style.width = width + 'px';
        } else if (wrapper) {
          while (wrapper.firstChild) cell.insertBefore(wrapper.firstChild, wrapper);
          wrapper.remove();
        }
      });
    });
  }
  function adjust(table) {
    var cfg = settings(table);
    decorate(table);
    cfg.oFeatures.bAutoWidth = true;
    cfg.aoColumns.forEach(function (column) {
      // ColReorder can copy calculated widths into sWidthOrig. Use only the
      // caller's original widths, or the current explicit user choice.
      column.sWidthOrig = state(table).widths[key(column)] ? null : column._datapondOriginalWidth;
      column.sWidth = null;
      column.nTh.style.width = '';
    });
    // Hiding every column can leave DataTables with a zero-width table.
    // Recalculate against its original container width when columns return.
    cfg.nTable.style.width = state(table).tableWidth;
    table.columns.adjust();
    if (cfg._responsive) table.responsive.rebuild().recalc();
  }
  function forceResponsiveColumn(table, column, reason) {
    if (!settings(table)._responsive) return;
    column[reason] = true;
    if (!/(^|\s)all(\s|$)/.test(column.sClass)) column.sClass += ' all';
    column.nTh.classList.add('all');
  }
  function dialog(table, title) {
    var box = document.createElement('dialog');
    box.className = 'datapond-table-dialog';
    box.setAttribute('aria-label', title);
    var heading = document.createElement('h4');
    heading.textContent = title;
    box.appendChild(heading);
    var form = document.createElement('form');
    var close = document.createElement('button');
    close.type = 'button';
    close.className = 'btn btn-default';
    close.textContent = 'Close';
    close.addEventListener('click', function () { box.close(); });
    form.appendChild(close);
    box.appendChild(form);
    box.addEventListener('close', function () { box.remove(); });
    table.table().container().appendChild(box);
    return { box: box, form: form };
  }

  window.datapondTableTools = {
    // The metadata history keeps its existing five-line text previews.
    cell: function (data, meta) {
      var span = document.createElement('span');
      span.className = 'metadata-history-preview';
      // DT escape=TRUE has already escaped source text.
      span.innerHTML = data == null ? '' : data;
      var button = document.createElement('button');
      button.type = 'button';
      button.className = 'metadata-history-cell';
      button.title = 'Read full text';
      var name = key(meta.settings.aoColumns[meta.col]);
      button.style.width = 'var(--' + name + ', ' + historyWidths[Number(name.split('-').pop())] + 'px)';
      button.appendChild(span);
      return button.outerHTML;
    },

    bind: function (table, config) {
      var cfg = settings(table);
      if (cfg._datapondTools) return;
      cfg._datapondTools = {
        history: !!(config && config.history), widths: {},
        autoWidth: cfg.oFeatures.bAutoWidth, tableWidth: cfg.sDestroyWidth || '100%'
      };
      cfg.aoColumns.forEach(function (column, index) {
        column._datapondKey = 'datapond-table-width-' + index;
        column._datapondPublic = column.bVisible && !column.nTh.classList.contains('datapond-table-internal');
        column._datapondOriginalWidth = column.sWidthOrig;
        column._datapondOriginalClass = column.sClass;
        if (column._datapondPublic) column.nTh.classList.add('datapond-table-public');
      });
      table.table().container().classList.add('datapond-table-tools');
      if (!cfg.oScroll.sX && !cfg.oScroll.sY) {
        $(table.table().node()).wrap('<div class="datapond-table-viewport"></div>');
      }
      $(table.table().node()).on('draw.dt.datapondTools', function () { decorate(table); });
      if (state(table).history) {
        $(table.table().body()).on('click.datapondTools', '.metadata-history-cell', function () {
          var cell = table.cell(this.closest('td'));
          var view = dialog(table, table.column(cell.index().column).header().textContent.trim());
          var text = document.createElement('p');
          text.textContent = this.textContent;
          view.box.insertBefore(text, view.form);
          view.box.showModal();
        });
      }
    },

    visibility: function (table) {
      var view = dialog(table, 'Show / hide columns');
      var help = document.createElement('p');
      help.textContent = 'Checked columns are visible. Changes apply immediately.';
      var choices = document.createElement('div');
      choices.className = 'datapond-table-columns';
      publicColumns(table).forEach(function (column) {
        var row = document.createElement('label');
        var checkbox = document.createElement('input');
        checkbox.type = 'checkbox';
        checkbox.checked = columnApi(table, column).visible() && column.nTh.style.display !== 'none';
        checkbox.dataset.column = key(column);
        checkbox.addEventListener('change', function () {
          columnApi(table, column).visible(checkbox.checked, false);
          if (checkbox.checked) forceResponsiveColumn(table, column, '_datapondVisibilityForced');
          adjust(table);
        });
        row.appendChild(checkbox);
        row.appendChild(document.createTextNode(label(column)));
        choices.appendChild(row);
      });
      var showAll = document.createElement('button');
      showAll.type = 'button';
      showAll.className = 'btn btn-default';
      showAll.textContent = 'Show all columns';
      showAll.addEventListener('click', function () {
        publicColumns(table).forEach(function (column) {
          columnApi(table, column).visible(true, false);
          forceResponsiveColumn(table, column, '_datapondVisibilityForced');
        });
        choices.querySelectorAll('input').forEach(function (checkbox) { checkbox.checked = true; });
        adjust(table);
      });
      view.box.insertBefore(help, view.form);
      view.box.insertBefore(choices, view.form);
      view.box.insertBefore(showAll, view.form);
      view.box.showModal();
    },

    widths: function (table) {
      var view = dialog(table, 'Column widths');
      var help = document.createElement('p');
      help.className = 'datapond-table-width-help';
      help.textContent = 'Choose a column, then move the slider. Changes apply immediately.';
      view.box.insertBefore(help, view.form);
      var columnLabel = document.createElement('label');
      columnLabel.textContent = 'Column';
      var select = document.createElement('select');
      select.className = 'form-control';
      var columns = publicColumns(table);
      columns.forEach(function (column) {
        var option = document.createElement('option');
        option.value = key(column);
        option.textContent = label(column);
        select.appendChild(option);
      });
      columnLabel.appendChild(select);
      var widthLabel = document.createElement('label');
      widthLabel.appendChild(document.createTextNode('Width '));
      var value = document.createElement('output');
      widthLabel.appendChild(value);
      var slider = document.createElement('input');
      slider.type = 'range';
      slider.min = '60';
      slider.max = '800';
      slider.step = '10';
      slider.setAttribute('aria-label', 'Column width in pixels');
      widthLabel.appendChild(slider);
      function selectedColumn() { return columns.find(function (column) { return key(column) === select.value; }); }
      function selectColumn() {
        var column = selectedColumn();
        if (!column) return;
        var fallback = state(table).history ? historyWidths[Number(key(column).split('-').pop())] :
          (parseFloat(column._datapondOriginalWidth) || column.nTh.getBoundingClientRect().width || 140);
        slider.value = state(table).widths[select.value] || fallback;
        value.textContent = slider.value + ' px';
      }
      select.addEventListener('change', selectColumn);
      slider.addEventListener('input', function () {
        var column = selectedColumn();
        if (!column) return;
        state(table).widths[select.value] = Number(slider.value);
        table.table().container().style.setProperty('--' + select.value, slider.value + 'px');
        value.textContent = slider.value + ' px';
        forceResponsiveColumn(table, column, '_datapondWidthForced');
        adjust(table);
      });
      var reset = document.createElement('button');
      reset.type = 'button';
      reset.className = 'btn btn-default';
      reset.textContent = 'Reset widths';
      reset.addEventListener('click', function () {
        columns.forEach(function (column) {
          table.table().container().style.removeProperty('--' + key(column));
          if (column._datapondWidthForced && !column._datapondVisibilityForced) {
            column.sClass = column._datapondOriginalClass;
            column.nTh.classList.toggle('all', /(^|\s)all(\s|$)/.test(column.sClass));
          }
          column._datapondWidthForced = false;
        });
        state(table).widths = {};
        adjust(table);
        settings(table).oFeatures.bAutoWidth = state(table).autoWidth;
        selectColumn();
      });
      view.box.insertBefore(columnLabel, view.form);
      view.box.insertBefore(widthLabel, view.form);
      view.box.insertBefore(reset, view.form);
      selectColumn();
      view.box.showModal();
    }
  };
})();
