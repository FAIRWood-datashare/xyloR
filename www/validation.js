// Listen for validation configuration from R
Shiny.addCustomMessageHandler('highlightNumericHeaders', function(validationConfig) {
  
  var headers = document.querySelectorAll('.ht_clone_top thead th');
  
  headers.forEach(function(header, i) {
    var columnName = header.innerText.trim().toLowerCase();
    var columnConfig = validationConfig.site_table[columnName];
    var colData = document.querySelectorAll('.ht_master tbody tr td:nth-child(' + (i + 1) + ')');
    var hasInvalid = false;
    var hasMissing = false;
    
    Array.from(colData).forEach(function(td) {
      var value = td.innerText.trim();

      // Remove tooltip if cell is now valid
      if (td.style.backgroundColor !== 'rgb(231, 76, 60)') {
        var tooltipId = td.getAttribute('data-tooltip-id');
        if (tooltipId) {
          var tooltip = document.getElementById(tooltipId);
          if (tooltip) tooltip.remove();
          td.removeAttribute('data-tooltip-id');
        }
      }

      // Validation logic
      if (columnConfig.required && (value === '' || value === 'NA' || value === null)) {
        td.style.backgroundColor = '#e74c3c';
        createTooltip(td, columnConfig.tooltip_message || 'This field is required');
        hasMissing = true;
      }

      if (columnConfig.type === "numeric" && isNaN(value)) {
        td.style.backgroundColor = '#e74c3c';
        createTooltip(td, 'This field must be numeric');
        hasInvalid = true;
      }

      if (columnConfig.type === "numeric" && columnConfig.min !== undefined && parseFloat(value) < columnConfig.min) {
        td.style.backgroundColor = '#e74c3c';
        createTooltip(td, `Value must be greater than ${columnConfig.min}`);
        hasInvalid = true;
      }

      if (columnConfig.type === "numeric" && columnConfig.max !== undefined && parseFloat(value) > columnConfig.max) {
        td.style.backgroundColor = '#e74c3c';
        createTooltip(td, `Value must be less than ${columnConfig.max}`);
        hasInvalid = true;
      }
    });

    // Handle header coloring based on validation status
    if (hasInvalid || hasMissing) {
      header.style.setProperty('background-color', '#e74c3c', 'important');
    } else {
      header.style.setProperty('background-color', 'green', 'important');
    }
    header.style.setProperty('color', 'white', 'important');
  });

  // Update the save button state based on header validity
  var allHeadersGreen = true;
  headers.forEach(function(header) {
    if (header.style.backgroundColor === 'rgb(231, 76, 60)') {
      allHeadersGreen = false;
    }
  });

  Shiny.setInputValue('all_headers_green', allHeadersGreen);
});

// Create the tooltip element and append it to the document
function createTooltip(td, message) {
  var tooltipId = 'tooltip-' + td.rowIndex + '-' + td.cellIndex;

  // Remove existing tooltip by ID (if any)
  var oldTooltip = document.getElementById(tooltipId);
  if (oldTooltip) oldTooltip.remove();

  var tooltip = document.createElement('div');
  tooltip.className = 'custom-tooltip';
  tooltip.id = tooltipId;
  tooltip.innerText = message;
  document.body.appendChild(tooltip);

  td.addEventListener('mouseover', function(e) {
    tooltip.style.display = 'block';
    tooltip.style.left = (e.pageX + 10) + 'px';
    tooltip.style.top = (e.pageY + 10) + 'px';
  });

  td.addEventListener('mouseout', function() {
    tooltip.style.display = 'none';
  });
}
