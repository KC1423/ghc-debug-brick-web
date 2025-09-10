function submitSearchLimit() {
  const input = document.getElementById("searchLimitInput");
  const limit = input.value;
  fetch(`/setSearchLimit?index=${encodeURIComponent(limit)}`)
  .then(response => response.text())
  .then(data => {
    document.getElementById('limitResult').textContent = data;
  });
}
