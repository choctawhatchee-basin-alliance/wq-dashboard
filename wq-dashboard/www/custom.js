// Download spinner
$(document).on('click', '#dwnld', function() {
  $(this).prop('disabled', true).html('<span class=\"spinner-border spinner-border-sm\" role=\"status\"></span> Downloading...');
});
// Reset download button after download is complete
Shiny.addCustomMessageHandler('reset_download_button', function(message) {
  $('#dwnld').prop('disabled', false).html('<i class=\"fa fa-download\"></i> Download data');
});
// Switch to overview tab if navbar text is clicked
$(document).ready(function() {
  $('.navbar-brand').css('cursor', 'pointer').on('click', function() {
    $('a[data-value=\"overview\"]').tab('show');
    Shiny.setInputValue('main-nav', 'overview');
    setTimeout(function() {
      $('a[data-value=\"about\"]').tab('show');
    }, 100);
  });
});
// Handle external links only
$(document).on('click', 'a[target="_blank"]', function(e) {
  e.preventDefault();
  window.open(this.href, '_blank');
});
// Handle internal navigation links only
$(document).on('click', 'a[target="_self"][data-nav]', function(e) {
  e.preventDefault();
  var target = $(this).data('nav');
  $('a[data-value="' + target + '"]').tab('show');
});