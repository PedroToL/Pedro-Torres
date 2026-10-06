// Interactions for the research page and the JMP page.
// Each paper is wrapped in an element with the attribute data-paper, and
// every control only affects the paper it sits in.

document.querySelectorAll("[data-paper]").forEach(function (paper) {

  // "more" / "less" on the abstract
  var moreButton = paper.querySelector("[data-toggle-abstract]");
  if (moreButton) {
    var shortText = paper.querySelector("[data-abstract-short]");
    var fullText = paper.querySelector("[data-abstract-full]");
    moreButton.addEventListener("click", function () {
      var opening = fullText.hidden;
      fullText.hidden = !opening;
      shortText.hidden = opening;
      moreButton.innerHTML = opening
        ? 'Show less <i class="bi bi-chevron-up"></i>'
        : 'Read more <i class="bi bi-chevron-down"></i>';
      moreButton.setAttribute("aria-expanded", opening);
    });
  }

  // Cite button opens the citation panel
  var citeButton = paper.querySelector("[data-toggle-cite]");
  var panel = paper.querySelector(".cite-panel");
  if (!citeButton || !panel) return;

  citeButton.addEventListener("click", function () {
    var opening = panel.hidden;
    panel.hidden = !opening;
    citeButton.classList.toggle("active", opening);
    citeButton.setAttribute("aria-expanded", opening);
  });

  // Text / BibTeX tabs
  var tabs = panel.querySelectorAll(".cite-tab");
  var bodies = panel.querySelectorAll("[data-cite-body]");
  tabs.forEach(function (tab) {
    tab.addEventListener("click", function () {
      tabs.forEach(function (t) { t.classList.toggle("active", t === tab); });
      bodies.forEach(function (b) { b.hidden = b.dataset.citeBody !== tab.dataset.citeTab; });
    });
  });

  // Copy whichever tab is showing
  var copyButton = panel.querySelector(".cite-copy");
  copyButton.addEventListener("click", function () {
    var shown = panel.querySelector("[data-cite-body]:not([hidden])");
    navigator.clipboard.writeText(shown.innerText).then(function () {
      copyButton.innerHTML = '<i class="bi bi-check2"></i> Copied';
    });
  });
});
