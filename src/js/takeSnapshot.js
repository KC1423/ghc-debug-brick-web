function submitSnapshot() {
  const input = document.getElementById("filenameInput");
  const name = input.value.trim();
  if (name === "") {
    document.getElementById('snapshotResult').textContent = "Input must not be empty";
    return;
  }
  fetch(`/takeSnapshot?filename=${encodeURIComponent(name)}`)
  .then(response => response.text())
  .then(data => {
    document.getElementById('snapshotResult').textContent = data;
  });
}
