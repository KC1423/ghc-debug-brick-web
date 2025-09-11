document.getElementById("searchLimitForm").addEventListener("submit", function(event) {
  event.preventDefault();
  const form = event.target;
  if (!form.checkValidity()) {
    form.reportValidity();
    return;
  }
  const input = document.getElementById("searchLimitInput");
  const limit = input.value;
  fetch(`/setSearchLimit?index=${encodeURIComponent(limit)}`)
  .then(response => response.text())
  .then(data => {
    document.getElementById('limitResult').textContent = data;
  });
});
