// custom_scripts.js

$(document).on('click', '#show_add_author, #update_author, #show_add_publication, #update_publication', function() {
  setTimeout(function() {
    $('.accordion-button').each(function() {
      if ($(this).hasClass('collapsed')) {
        $(this).click();
      }
    });
  }, 200);  // slight delay to ensure accordion is rendered
});
