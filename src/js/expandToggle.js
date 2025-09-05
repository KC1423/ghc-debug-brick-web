document.addEventListener("DOMContentLoaded", () => {
  attachExpandListeners();
});
function attachExpandListeners() {
  document.querySelectorAll('.expand-button').forEach(button => {
    if (button.dataset.listenerAttached === "true") return; // prevent duplicate
    button.dataset.listenerAttached = "true";
    button.addEventListener('click', async (e) => {
      e.preventDefault();
      const form = button.closest("form");
      const pathInput = form.querySelector("input[name=toggle]");
      const path = pathInput.value;
      const containerId = "children-" + path;
      let container = document.getElementById(containerId);
      if (container) {
        const isVisible = container.style.display !== "none";
        container.style.display = isVisible ? "none" : "block";
        button.textContent = isVisible ? "-" : "+";
      } else {
        const res = await fetch(`/tree/children?toggle=${encodeURIComponent(path)}`);
        const html = await res.text();
        container = document.createElement("div");
        container.id = containerId;
        container.innerHTML = html;
        container.style.display = "block";
        button.closest(".tree-row")?.insertAdjacentElement("afterend", container);
        attachExpandListeners();  // Recurse to attach events to newly added buttons
        button.textContent = "+";
      }
    });
  });
}

     
