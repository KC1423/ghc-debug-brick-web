document.addEventListener("DOMContentLoaded", () => {
  document.body.addEventListener("click", async (e) => {
      const button = e.target.closest(".expand-button");
      if (!button) return;
      e.preventDefault();

      const path = button.dataset.path;
      const urlParams = new URLSearchParams(window.location.search);
      let selected = urlParams.get("selected");
      if (!selected) {
        selected = "0"
      }

      const containerId = "children-" + path;
      let container = document.getElementById(containerId);

      const res = await fetch(`/toggle?toggle=${path}&selected=${selected}`);
      const html = await res.text();

      if (!container) {
        container = document.createElement("div");
        container.id = containerId;
        container.classList.add("children");
        button.closest(".tree-row")?.insertAdjacentElement("afterend", container);
        container.innerHTML = html;
        button.textContent = "+";
      } else {
        container.remove();
	button.textContent = "-";
      } 
  });
});

