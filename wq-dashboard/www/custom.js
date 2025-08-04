$(document).on('click', '#dwnld', function() {
  $(this).prop('disabled', true).html('<span class=\"spinner-border spinner-border-sm\" role=\"status\"></span> Downloading...');
});
Shiny.addCustomMessageHandler('reset_download_button', function(message) {
  $('#dwnld').prop('disabled', false).html('<i class=\"fa fa-download\"></i> Download data');
});
$(document).ready(function() {
  $('.navbar-brand').css('cursor', 'pointer').on('click', function() {
    $('a[data-value=\"overview\"]').tab('show');
    Shiny.setInputValue('main-nav', 'overview');
    setTimeout(function() {
      $('a[data-value=\"about\"]').tab('show');
    }, 100);
  });
});
$(document).on('click', 'a[href^=\"http\"]', function(e) {
  e.preventDefault();
  e.stopPropagation();
  window.open(this.href, '_blank');
});
$(document).on('click', 'a[href^=\"#\"]', function(e) {
  e.preventDefault();
  var target = this.getAttribute('href').substring(1);
  console.log('Clicked anchor:', target);
  
  // Map your anchor names to nav panel values
  var panelMap = {
    'byarea': 'byarea',
    'parmcomp': 'parmcomp', 
    'download': 'download',
    'oyster': 'oyster',
    'addlresources': 'addlresources'
  };
  
  if (panelMap[target]) {
    console.log('Navigating to:', panelMap[target]);
    
    // Try multiple methods to trigger navigation
    var targetValue = panelMap[target];
    
    // Bootstrap tab method
    $('a[data-value="' + targetValue + '"]').tab('show');

  }
});