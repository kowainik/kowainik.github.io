$(document).ready(function() {
    $('[data-toggle="collapse"]').click(function() {
        $(this).toggleClass( "change-text" );
        if ($(this).hasClass("change-text")) {
            $(this).text("Hide solution");
        } else {
            $(this).text("Show solution");
        }
    });
});
