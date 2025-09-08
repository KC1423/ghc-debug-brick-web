document.addEventListener("DOMContentLoaded", function () {
  function updateSelectedInfo(pathStr) {
    fetch(`/partial?selected=${encodeURIComponent(pathStr)}`)
      .then(res => res.text())
      .then(html => {
        const summary = document.getElementById("selection-summary");
        console.log("summary element: ", summary);
        document.getElementById("selection-summary").innerHTML = html;
      });
  }
  function updateSelectionStyles(selectedPathStr) {
    // Loop over all tree links
    document.querySelectorAll(".tree-link").forEach(link => {
      const path = link.dataset.selected;
  
      // Set the color based on whether it's the selected one
      if (path === selectedPathStr) {
        link.style.color = "purple";
        link.closest(".tree-row")?.classList.add("selected");
      } else {
        link.style.color = "blue";
        link.closest(".tree-row")?.classList.remove("selected");
      }
    });
  }

  // Intercept clicks on tree links
  document.querySelectorAll(".tree-link").forEach(link => {
    link.addEventListener("click", function (e) {
      e.preventDefault();
      const pathStr = this.dataset.selected;

      // Update selected info
      updateSelectedInfo(pathStr);
      updateSelectionStyles(pathStr);

      // Change URL in address bar
      const newUrl = new URL(window.location.href);
      newUrl.searchParams.set("selected", pathStr);
      history.pushState(null, "", newUrl.toString());

      // Optional: highlight selected row
      document.querySelectorAll(".tree-row.selected").forEach(row => {
        row.classList.remove("selected");
      });
      this.closest(".tree-row").classList.add("selected");
    });
  });

  // Handle browser back/forward
  window.addEventListener("popstate", function () {
    const urlParams = new URLSearchParams(window.location.search);
    const pathStr = urlParams.get("selected");
    if (pathStr) {
      updateSelectedInfo(pathStr);
      updateSelectionStyles(pathStr);
    }
  });
});
