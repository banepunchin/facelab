// Fade the rating face on each new face load
document.addEventListener("shiny:value", function(e) {
  if (e.name === "rating_face_ui") {
    var frame = document.querySelector(".face-frame");
    if (frame) {
      frame.style.opacity = "0";
      setTimeout(function() {
        frame.style.transition = "opacity 0.25s ease";
        frame.style.opacity = "1";
      }, 50);
    }
  }
});
