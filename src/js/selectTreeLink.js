let svgLoaded = false;

function fastModeToggle() {
  const graphDiv = document.getElementById('toggleDiv');
  const container = document.getElementById('svg-container');
  if (graphDiv && graphDiv.style.display !== 'none') {
    svgLoaded = false;
    if (container) {
      container.innerHTML = '<p style="font-style: italic; color: #555;">Loading graph...</p>';
      document.getElementById('download-link').style.display = 'none';
    }
    fetchAndRender();
  }
}

function fetchAndRender() {
  const container = document.getElementById('svg-container');
  if (!svgLoaded && container) {
    container.innerHTML = '<p style="font-style: italic; color: #555;">Loading graph...</p>';

    const fastCheckbox = document.getElementById('fastModeCheckbox')
    console.log(fastCheckbox);
    const isFast = fastCheckbox?.checked;
    console.log(isFast);
    const url = isFast ? '/graph?fastMode=True' : '/graph';

    fetch(url)
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
  const savedState = localStorage.getItem('toggleDivState');
  const div = document.getElementById('toggleDiv');
  const btn = document.getElementById('toggleButton');

  const available = div?.dataset?.available !== "false";

  if (div.style.display === 'none') {
    div.style.display = 'block';
    btn.textContent = 'Hide';
    localStorage.setItem('toggleDivState', 'shown');
    if (!svgLoaded) {
      fetchAndRender();
    }
  } else {
    div.style.display = 'none';
    btn.textContent = 'Show';
    localStorage.setItem('toggleDivState', 'hidden');
  }
}

function applyToggleState() {
  const savedState = localStorage.getItem('toggleDivState');
  const div = document.getElementById('toggleDiv');
  const btn = document.getElementById('toggleButton');

  const available = div?.dataset?.available !== "false";

  if (savedState === 'shown' && available) {
    div.style.display = 'block';
    btn.textContent = 'Hide';
    fetchAndRender();
  } else {
    div.style.display = 'none';
    btn.textContent = 'Show';
  }
}

function updateSummaries(pathStr) {
  fetch(`/partial?selected=${encodeURIComponent(pathStr)}`)
    .then(res => res.json())
    .then(data => {
      document.getElementById("selection-summary").innerHTML = data.summary;
      let imgTitle = document.getElementById('image-title');
      if (imgTitle) {
        imgTitle.innerHTML = data.imgName;
      }
      applyToggleState();
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
      document.getElementById('download-link').style.display = 'none';
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

  if (div) {
    if (state === 'shown') {
      div.style.display = 'block';
      btn.textContent = 'Hide';
      svgLoaded = false;
      fetchAndRender();
    } else {
      div.style.display = 'none';
      btn.textContent = 'Show';
    }
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
