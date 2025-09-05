document.addEventListener("DOMContentLoaded", () => {
  document.body.addEventListener("click", async (e) => {
      const button = e.target.closest(".expand-button");
      if (!button) return;
      e.preventDefault();

      const path = button.dataset.path;
      const containerId = "children-" + path;
      let container = document.getElementById(containerId);

      const res = await fetch(`/toggle?toggle=${path}`);
      const html = await res.text();

      if (!container) {
        container = document.createElement("div");
        container.id = containerId;
        container.classList.add("children");
        button.closest(".tree-row")?.insertAdjacentElement("afterend", container);
      }

      container.innerHTML = html;

      const isEmpty = html.trim() === "";
      container.style.display = isEmpty ? "none" : "block";
      button.textContent = isEmpty ? "-" : "+";
  });
});

