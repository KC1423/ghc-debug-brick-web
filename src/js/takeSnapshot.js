document.getElementById("snapshotForm").addEventListener("submit", function(event) {
  event.preventDefault();
  const form = event.target;
  if (!form.checkValidity()) {
    form.reportValidity();
    return;
  }
  const input = document.getElementById("filenameInput");
  const name = input.value.trim();
  fetch(`/takeSnapshot?filename=${encodeURIComponent(name)}`)
  .then(response => response.text())
  .then(data => {
    document.getElementById('snapshotResult').textContent = data;
  });
});
