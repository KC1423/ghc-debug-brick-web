let svgLoaded = false;

function fetchAndRender() {
  const container = document.getElementById('svg-container');
  if (!svgLoaded && container) {
    container.innerHTML = '<p style="font-style: italic; color: #555;">Loading graph...</p>';

    fetch('/graph')
      .then(res => res.text())
      .then(svg => {
        container.innerHTML = svg;
        document.getElementById('download-link').style.display = 'inline-block';

        const element = document.querySelector('#svg-container svg');
        panzoom(element, {
          bounds: true,
          boundsPadding: 0.1,
          zoomDoubleClickSpeed: 1,
          maxZoom: 10,
          minZoom: 0.1
        });

        svgLoaded = true;
      })
      .catch(err => {
        console.error('Failed to load graph:', err);
        container.innerHTML = '<p style="color: red;">Failed to load graph.</p>';
      });
  }
}

function toggleDiv() {
  const div = document.getElementById('toggleDiv');
  const btn = document.getElementById('toggleButton');

  if (div.style.display === 'none') {
    div.style.display = 'block';
    btn.textContent = 'Hide';
    localStorage.setItem('toggleDivState', 'shown');
    svgLoaded = false;
    fetchAndRender();
  } else {
    div.style.display = 'none';
    btn.textContent = 'Show';
    localStorage.setItem('toggleDivState', 'hidden');
  }
}

function updateSummaries(pathStr) {
  fetch(`/partial?selected=${encodeURIComponent(pathStr)}`)
    .then(res => res.json())
    .then(data => {
      document.getElementById("selection-summary").innerHTML = data.summary;
      document.getElementById("image-title").innerHTML = data.imgName;
    })
    .catch(err => {
      console.error('Failed to update summaries:', err);
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

function updateSelection(pathStr) {
  updateSummaries(pathStr);
  updateSelectionStyles(pathStr);
  svgLoaded = false;

  const graphDiv = document.getElementById('toggleDiv');
  const container = document.getElementById('svg-container');

  if (graphDiv && graphDiv.style.display !== 'none') {
    if (container) {
      container.innerHTML = '<p style="font-style: italic; color: #555;">Loading graph...</p>';
    }
    setTimeout(() => {
      fetchAndRender();
    }, 100);
  }
}

// Event delegation to handle all future <a> clicks
document.addEventListener('click', function (event) {
  const target = event.target.closest('a');
  if (target && target.href && target.closest('.tree-row')) {
    const url = new URL(target.href);
    const selected = url.searchParams.get('selected');
    if (selected) {
      event.preventDefault();
      history.pushState(null, '', `?selected=${encodeURIComponent(selected)}`);
      updateSelection(selected);
    }
  }
});

// Restore toggle state on page load
document.addEventListener('DOMContentLoaded', function() {
  const div = document.getElementById('toggleDiv');
  const btn = document.getElementById('toggleButton');
  const state = localStorage.getItem('toggleDivState');

  if (state === 'shown') {
    div.style.display = 'block';
    btn.textContent = 'Hide';
    svgLoaded = false;
    fetchAndRender();
  } else {
    div.style.display = 'none';
    btn.textContent = 'Show';
  }

  // Optional: initialize based on current selected path in URL
  const url = new URL(window.location.href);
  const selected = url.searchParams.get('selected');
  if (selected) {
    updateSelection(selected);
  }
});

// Handle browser back/forward navigation
window.addEventListener('popstate', function () {
  const url = new URL(window.location.href);
  const selected = url.searchParams.get('selected');
  if (selected) {
    updateSelection(selected);
  }
});
