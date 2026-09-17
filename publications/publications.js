(function() {
  function getCard(el) {
    while (el && !el.classList?.contains('pub-card')) el = el.parentNode;
    return el;
  }
  function showPanel(card, label, html, mode) {
    const area = card.querySelector('.toggle-area');
    const labelEl = area.querySelector('.panel-label');
    const contentEl = area.querySelector('.content');

    // Toggle: if clicking same action while open, collapse
    const current = area.getAttribute('data-current');
    if (current === mode && area.style.display === 'block') {
      area.style.display = 'none';
      area.setAttribute('data-current', '');
      return;
    }

    labelEl.textContent = label;
    if (mode === 'bib') {
      contentEl.innerHTML = '';
      const pre = document.createElement('pre');
      pre.textContent = html;
      contentEl.appendChild(pre);
    } else if (mode === 'abstract') {
      contentEl.textContent = html;
    } else {
      contentEl.innerHTML = html; // citation HTML
    }
    area.style.display = 'block';
    area.setAttribute('data-current', mode);
  }

  // Resource link handler (toggle)
  document.addEventListener('click', function(evt) {
    const a = evt.target.closest('a[data-action]');
    if (!a) return;
    const action = a.getAttribute('data-action');
    const card   = getCard(a);
    if (!card) return;
    evt.preventDefault();

    if (action === 'abstract' || action === 'citation' || action === 'bib') {
      const key = card.getAttribute('data-key');  // <-- Pandoc ref id suffix

      // Build label + payload
      let label = action === 'citation' ? 'Citation' : (action === 'bib' ? 'BibTeX' : 'Abstract');
      let html  = '';

      if (action === 'abstract') {
        const payloadEl = card.querySelector('.payload-abstract');
        const text = (payloadEl?.textContent || 'No abstract available.').trim();
        showPanel(card, label, text, 'abstract');
        return;
      }

      if (action === 'citation') {
        // Pull APA-formatted HTML from hidden #refs list: <div id="ref-KEY">
        const ref = document.querySelector('#ref-' + key);
        html = (ref ? ref.innerHTML : 'Citation unavailable.').trim();
        showPanel(card, label, html, 'citation');
        return;
      }

      if (action === 'bib') {
        const payloadEl = card.querySelector('.payload-bibtex');
        const text = (payloadEl?.textContent || 'No BibTeX available.').trim();
        showPanel(card, label, text, 'bib');
        return;
      }
    }
  }, false);

  // Copy button
  document.addEventListener('click', function(evt) {
    const btn = evt.target.closest('.copy-btn');
    if (!btn) return;
    const card = getCard(btn);
    const area = card.querySelector('.toggle-area');
    const contentEl = area.querySelector('.content');

    let textToCopy = '';
    const pre = contentEl.querySelector('pre');
    if (pre) textToCopy = pre.textContent;
    else textToCopy = contentEl.innerText;

    navigator.clipboard.writeText(textToCopy).then(() => {
      const toast = area.querySelector('.copy-toast');
      toast.style.display = 'inline-block';
      setTimeout(() => { toast.style.display = 'none'; }, 1200);
    }).catch(() => {
      const ta = document.createElement('textarea');
      ta.value = textToCopy;
      document.body.appendChild(ta);
      ta.select();
      try { document.execCommand('copy'); } catch (e) {}
      document.body.removeChild(ta);
    });
  }, false);
})();
