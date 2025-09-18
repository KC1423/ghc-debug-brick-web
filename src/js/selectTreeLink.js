let svgLoaded = false;
let panzoomInstance = null;

function fastModeToggle() {
  const graphDiv = document.getElementById('toggleDiv');
  const container = document.getElementById('svg-container');
  svgLoaded = false;
  if (graphDiv && graphDiv.style.display !== 'none') {
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
    const isFast = fastCheckbox?.checked;
    const url = isFast ? '/graph?fastMode=True' : '/graph';

    fetch(url)
      .then(res => {
	if (!res.ok) {
          return res.text().then(msg => {
            throw new Error(msg);
	  });
	}
	return res.text();
      })
      .then(svgText => {
	const parser = new DOMParser();
	const doc = parser.parseFromString(svgText, "image/svg+xml");
	const svg = doc.querySelector('svg');
        container.innerHTML = '';
	container.appendChild(svg);

        //const element = document.querySelector('#svg-container svg');
        panzoomInstance = panzoom(svg/*element*/, {
          bounds: true,
          boundsPadding: 0.1,
          zoomDoubleClickSpeed: 1,
          maxZoom: 10,
          minZoom: 0.1
        });


	/*const serializer = new XMLSerializer();
        const svgString = serializer.serializeToString(svg);
	const blob = new Blob([svgString], { type: "image/svg+xml" });
	const blobUrl = URL.createObjectURL(blob);*/
	const downloadLink = document.getElementById('download-link');
        downloadLink.style.display = 'inline-block';


        svgLoaded = true;
      })
      .catch(err => {
	if (err.message === 'cancelled') {
          console.log('Render cancelled - staying in loading state');
          return;
	}
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
  } else {
    div.style.display = 'none';
    btn.textContent = 'Show';
  }
}


function updateSummaries(pathStr) {
  return fetch(`/partial?selected=${encodeURIComponent(pathStr)}`)
    .then(res => res.json())
    .then(data => {
      document.getElementById("selection-summary").innerHTML = data.summary;
      let imgTitle = document.getElementById('image-title');
      if (imgTitle) {
        imgTitle.innerHTML = data.imgName;
      }
      let capWarning = document.getElementById('capWarning');
      if (capWarning) {
        capWarning.innerHTML = data.capWarning;
      }
      applyToggleState();
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
  updateSelectionStyles(pathStr);
  svgLoaded = false;

  const graphDiv = document.getElementById('toggleDiv');
  const container = document.getElementById('svg-container');

  if (graphDiv && graphDiv.style.display !== 'none') {
    if (container) {
      container.innerHTML = '<p style="font-style: italic; color: #555;">Loading graph...</p>';
      document.getElementById('download-link').style.display = 'none';
    }
    updateSummaries(pathStr)
      .then(() => {
        fetchAndRender();
      })
      .catch(err => {
        console.error('Failed to update summaries or render graph:', err);
        container.innerHTML = '<p style="color: red;">Failed to update summaries or load graph.</p>';
      });
  } else {
    updateSummaries(pathStr).catch(err => {
      console.error('Failed to update summaries:', err);
    });
  }
}

function forceExpandPath(path) {
  svgLoaded = false;
  const downloadLink = document.getElementById('download-link');
  downloadLink.style.display = 'inline-block';


  const url = new URL(window.location.href);
  let selected = url.searchParams.get('selected');
  if (!selected) {
    selected = "0" 
  }
  fetch(`/forceExpand?selected=${selected}&force=${path}`)
      .then(res => {
	if (!res.ok) {
          return res.text().then(msg => {
            throw new Error(msg);
	  });
	}
	return res.text();
      })
      .then(svgText => {
        const parser = new DOMParser();
	const doc = parser.parseFromString(svgText, "image/svg+xml");
	const svg = doc.querySelector('svg');


	const container = document.getElementById('svg-container');
        const transform = panzoomInstance.getTransform();
        container.innerHTML = '';
	container.appendChild(svg);

        panzoomInstance = panzoom(svg, {
          bounds: true,
          boundsPadding: 0.1,
          zoomDoubleClickSpeed: 1,
          maxZoom: 10,
          minZoom: 0.1
        });
        panzoomInstance.moveTo(transform.x, transform.y);
	panzoomInstance.zoomAbs(transform.x, transform.y, transform.scale);

	const downloadLink = document.getElementById('download-link');
        downloadLink.style.display = 'inline-block';


        svgLoaded = true;
      })
    .catch(err => {
	if (err.message === 'cancelled') {
        console.log('Render cancelled - staying in loading state');
        return;
      }
      console.error('Failed to load graph:', err);
      container.innerHTML = '<p style="color: red;">Failed to load graph.</p>';
    });

}

/*
function applyGraphDiff(oldSvg, newSvg) {
  const oldGraph = oldSvg.querySelector('g#graph0');
  const newGraph = newSvg.querySelector('g#graph0');

  const oldNodes = new Map();
  const oldEdges = new Map();
  
  oldGraph.querySelectorAll('g.node').forEach(node => {
    const id = node.id;
    if (id) oldNodes.set(id, node);
  });
  oldGraph.querySelectorAll('g.edge').forEach(edge => {
    const id = edge.id;
    if (id) oldEdges.set(id, edge);
  });
  newGraph.querySelectorAll('g.node').forEach(node => {
    const id = node.id;
    if (!oldNodes.has(id)) {
      console.log(id);
      oldGraph.appendChild(node.cloneNode(true));
    }
  });
  newGraph.querySelectorAll('g.edge').forEach(edge => {
    const id = edge.id;
    if (!oldEdges.has(id)) {
      console.log(id);
      oldGraph.appendChild(edge.cloneNode(true));
    }
  });
}
*/

// Event delegation to handle all future <a> clicks
document.addEventListener('DOMContentLoaded', function () {
  document.addEventListener('click', function (event) {
    const target = event.target.closest('a');
    if (!target) return;
    const rawHref = target.getAttribute('href') || target.getAttribute('xlink:href');
    if (!rawHref) return;
    const url = new URL(rawHref, window.location.origin);
    if (target.closest('.tree-row')) {
      const selected = url.searchParams.get('selected');
      if (selected) {
        event.preventDefault();
        history.pushState(null, '', `?selected=${encodeURIComponent(selected)}`);
        updateSelection(selected);
        return;
      }
    }
    if (url.pathname === '/forceExpand') {
      const path = url.searchParams.get('path');
      if (path) {
        event.preventDefault();
        forceExpandPath(path);
        return;
      }
    }
  });
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
