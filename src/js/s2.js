document.addEventListener("DOMContentLoaded", function () {
  const treeContainer = document.getElementById("iotree");

  function updateSummaries(pathStr) {
    fetch(`/partial?selected=${encodeURIComponent(pathStr)}`)
      .then(res => res.text())
      .then(data => {
        document.getElementById("selection-summary").innerHTML = data;
        //document.getElementById("right-summary").innerHTML = data.right;
      });
  }

  function updateSelectionStyles(selectedPathStr) {
    document.querySelectorAll(".tree-link").forEach(link => {
      const path = link.dataset.selected;
      const row = link.closest(".tree-row");

      if (path === selectedPathStr) {
        link.style.color = "purple";
        row?.classList.add("selected");
      } else {
        link.style.color = "blue";
        row?.classList.remove("selected");
      }
    });
  }

  treeContainer.addEventListener("click", function (e) {
    const link = e.target.closest(".tree-link");
    if (link) {
      e.preventDefault();
      const pathStr = link.dataset.selected;

      updateSummaries(pathStr);
      updateSelectionStyles(pathStr);
      const graphDiv = document.getElementById('toggleDiv');
      const container = document.getElementById('svg-container');
      if (graphDiv && graphDiv.style.display !== 'none') {
        svgLoaded = false;
	if (container) {
          container.innerHTML = '<p style=\"font-style: italic; color: #555; \">Loading graph...</p>';
	}
	fetchAndRender();
      } else {
        svgLoaded = false;
      }
      const newUrl = new URL(window.location.href);
      newUrl.searchParams.set("selected", pathStr);
      history.pushState(null, "", newUrl.toString());
    }
  });

  // Handle back/forward
  window.addEventListener("popstate", function () {
    const urlParams = new URLSearchParams(window.location.search);
    const pathStr = urlParams.get("selected");
    if (pathStr) {
      updateSummaries(pathStr);
      updateSelectionStyles(pathStr);
      svgLoaded = false;
    }
  });
});
