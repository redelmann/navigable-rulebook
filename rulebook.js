$(function() {
 
  $("a.reference").click(function(e) {

    var target = $($(this).attr("href"));

    if (target.length == 0) {
      return false;
    }

    var targetSection = target.closest(".chapter, .section, .subsection, .note, li");
    var targetDefinition = target.closest(".definition");

    $('html, body').stop(true);

    $('html, body').animate({
      scrollTop: targetSection.offset().top
    }, 1000);

    targetDefinition.animate({ backgroundColor: "#ffffaa" }, 1500);
    setTimeout(function() {
      targetDefinition.animate({ backgroundColor: "transparent" }, 2000);
    }, 2000);

    return true;
  });

  $(".toc a").click(function(e) {
    var target = $($(this).attr("href"));

    $('html, body').stop(true);

    $('html, body').animate({
      scrollTop: target.offset().top
    }, 1000);

    return true;
  });
})