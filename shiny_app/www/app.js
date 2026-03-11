// ============================================================
//  FaceLab — app.js
//  Script is loaded at end of body so jQuery + Shiny are ready
// ============================================================

// Called via inline onclick on each .face-card div (most reliable in Shiny)
function faceCardClick(el) {
  var $card  = $(el);
  var faceId = $card.attr("data-face-id");

  $card.toggleClass("selected");

  if ($card.hasClass("selected")) {
    $card.append('<div class="face-check">\u2713</div>');
  } else {
    $card.find(".face-check").remove();
  }

  // Always preview the most recently clicked face
  Shiny.setInputValue("preview_face", faceId, { priority: "event" });

  // Sync full selected list to Shiny
  var selected = [];
  $(".face-card.selected").each(function () {
    selected.push($(this).attr("data-face-id"));
  });
  // Send null when empty so ignoreNULL=FALSE handler fires correctly
  Shiny.setInputValue("selected_faces", selected.length > 0 ? selected : null,
                      { priority: "event" });
}

// Server → client: wipe all visual selection state
Shiny.addCustomMessageHandler("clearSelection", function (_msg) {
  $(".face-card").removeClass("selected").find(".face-check").remove();
});
