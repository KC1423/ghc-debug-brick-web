function handleSelectChange(selectId, inputId) {
  var select = document.getElementById(selectId);
  var input = document.getElementById(inputId);
  if (select && input) {
    if (select.value === '__other__') {
      input.style.display = 'inline-block';
      input.required = true;
      select.name = '';
      input.name = 'pattern';
    } else {
      input.style.display = 'none';
      input.required = false;
      input.name = '';
      select.name = 'pattern';
    }
  }
}
