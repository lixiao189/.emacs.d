;;; init-markdown.el --- Markdown -*- lexical-binding: t; -*-

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :hook (markdown-mode . visual-line-mode)
  :init
  (setq markdown-enable-math t                   ; fontify $..$ and $$..$$
        markdown-fontify-code-blocks-natively t))

;;;; Live preview inside Emacs (xwidget-webkit) ------------------------------------------
;; Rendering happens in the webkit widget (markdown-it + KaTeX from jsdelivr), so no
;; pandoc/multimarkdown is needed. Edits are pushed with JS, which keeps it flicker-free.
(defconst my/markdown-preview-template
  "<!doctype html>
<html>
<head>
<meta charset='utf-8'>
<base href='{{base}}'>
<link rel='stylesheet' href='https://cdn.jsdelivr.net/npm/katex/dist/katex.min.css'>
<link rel='stylesheet' href='https://cdn.jsdelivr.net/npm/markdown-it-texmath/css/texmath.min.css'>
<script src='https://cdn.jsdelivr.net/npm/markdown-it/dist/markdown-it.min.js'></script>
<script src='https://cdn.jsdelivr.net/npm/katex/dist/katex.min.js'></script>
<script src='https://cdn.jsdelivr.net/npm/markdown-it-texmath/texmath.min.js'></script>
<style>
  body { background: {{bg}}; color: {{fg}}; margin: 0 auto; padding: 24px 32px; max-width: 860px;
         font: 16px/1.6 -apple-system, BlinkMacSystemFont, sans-serif; }
  a { color: {{link}}; }
  h1, h2 { border-bottom: 1px solid rgba(127,127,127,.3); padding-bottom: .3em; }
  code, pre { font-family: ui-monospace, Menlo, monospace; font-size: .9em;
              background: rgba(127,127,127,.15); border-radius: 4px; }
  code { padding: .15em .35em; }
  pre { padding: 12px 16px; overflow-x: auto; }
  pre code { padding: 0; background: none; }
  blockquote { margin: 0; padding-left: 1em; border-left: 4px solid rgba(127,127,127,.4); opacity: .85; }
  table { border-collapse: collapse; }
  th, td { border: 1px solid rgba(127,127,127,.4); padding: 4px 12px; }
  img { max-width: 100%; }
  hr { border: 0; border-top: 1px solid rgba(127,127,127,.3); }
</style>
</head>
<body>
<div id='content'></div>
<script>
  var content = document.getElementById('content'), md = null;
  if (window.markdownit && window.katex && window.texmath)
    md = markdownit({ html: true, linkify: true }).use(texmath, {
      engine: katex,
      delimiters: ['dollars', 'brackets', 'beg_end'],
      katexOptions: { throwOnError: false }
    });
  // Tag top-level blocks with their source line so the editor can scroll us to a line.
  if (md)
    md.core.ruler.push('source_line', function (state) {
      state.tokens.forEach(function (t) {
        if (t.map && t.level === 0 && t.nesting >= 0 && t.type !== 'inline')
          t.attrSet('data-line', String(t.map[0]));
      });
    });
  // Put source line LINE (zero-based) at the top; -1 means the end of the document.
  function scrollToLine(line) {
    var doc = document.documentElement;
    if (line < 0) { window.scrollTo(0, doc.scrollHeight); return; }
    var els = content.querySelectorAll('[data-line]'), prev = null, next = null;
    for (var i = 0; i < els.length; i++) {
      if (+els[i].dataset.line <= line) prev = els[i]; else { next = els[i]; break; }
    }
    if (!prev) { window.scrollTo(0, 0); return; }
    var top = prev.getBoundingClientRect().top + window.scrollY, y = top;
    if (next) {  // interpolate between the two nearest tagged blocks
      var pl = +prev.dataset.line, nl = +next.dataset.line;
      y += (next.getBoundingClientRect().top + window.scrollY - top) * (line - pl) / (nl - pl);
    }
    window.scrollTo(0, line === 0 ? 0 : y);
  }
  // Called from Emacs on every edit. LINE is the source line at the top of the editor window.
  function render(src, line) {
    if (!md) {
      content.textContent = 'Could not load markdown-it/KaTeX from cdn.jsdelivr.net (offline?).';
      return;
    }
    content.innerHTML = md.render(src);
    scrollToLine(line);
  }
  render({{init}}, {{line}});
</script>
</body>
</html>"
  "Preview page.  The {{...}} placeholders are filled in by `my/markdown-preview'.")

(defvar-local my/markdown-preview--buffer nil
  "The xwidget buffer previewing this markdown buffer.")
(defvar-local my/markdown-preview--timer nil)

(defun my/markdown-preview--source ()
  "Buffer text as a JS string literal that is safe inside <script>."
  (string-replace "</" "<\\/" (json-encode (buffer-substring-no-properties
                                             (point-min) (point-max)))))

(defun my/markdown-preview--top-line (&optional win start)
  "Zero-based source line at the top of WIN, or -1 when WIN shows the buffer's end.
START is WIN's new start position when called while it is scrolling."
  (let* ((win (or win (get-buffer-window (current-buffer))))
         (start (or start (and win (window-start win)) (point-min))))
    (if (and win (> start (point-min)) (pos-visible-in-window-p (point-max) win))
        -1
      (1- (line-number-at-pos start t)))))

(defun my/markdown-preview--eval (js)
  "Run JS in this buffer's preview, closing the preview if its buffer is gone."
  (if (buffer-live-p my/markdown-preview--buffer)
      (xwidget-webkit-execute-script
       (with-current-buffer my/markdown-preview--buffer (xwidget-at (point-min)))
       js)
    (my/markdown-preview--close)))

(defun my/markdown-preview--update (buf)
  "Re-render BUF in its preview."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (my/markdown-preview--eval
       (format "render(%s,%d)" (my/markdown-preview--source)
               (my/markdown-preview--top-line))))))

(defun my/markdown-preview--schedule (&rest _)
  (when (timerp my/markdown-preview--timer) (cancel-timer my/markdown-preview--timer))
  (setq my/markdown-preview--timer
        (run-with-idle-timer 0.3 nil #'my/markdown-preview--update (current-buffer))))

(defun my/markdown-preview--scroll (win start)
  "Scroll the preview to the source line at the top of WIN (on `window-scroll-functions')."
  (with-current-buffer (window-buffer win)
    (my/markdown-preview--eval
     (format "scrollToLine(%d)" (my/markdown-preview--top-line win start)))))

(defun my/markdown-preview--close ()
  (remove-hook 'after-change-functions #'my/markdown-preview--schedule t)
  (remove-hook 'window-scroll-functions #'my/markdown-preview--scroll t)
  (remove-hook 'kill-buffer-hook #'my/markdown-preview--close t)
  (when (timerp my/markdown-preview--timer) (cancel-timer my/markdown-preview--timer))
  (when (buffer-live-p my/markdown-preview--buffer)
    (let ((buf my/markdown-preview--buffer)
          (kill-buffer-query-functions nil))
      (dolist (w (get-buffer-window-list buf nil t))
        (unless (one-window-p t w) (delete-window w)))
      (kill-buffer buf)))
  (setq my/markdown-preview--buffer nil))

(defun my/markdown-preview ()
  "Toggle a live preview (with math) of this buffer in a side window.
The preview follows edits and scrolls together with the editor window."
  (interactive)
  (unless (and (featurep 'xwidget-internal) (display-graphic-p))
    (user-error "Preview needs a graphical Emacs built with xwidgets"))
  (if (buffer-live-p my/markdown-preview--buffer)
      (my/markdown-preview--close)
    (let ((file (expand-file-name "var/markdown-preview.html" user-emacs-directory))
          (html my/markdown-preview-template)
          (win (selected-window))
          preview)
      ;; {{init}} goes last so "{{bg}}" etc. in the document itself is left alone.
      (dolist (kv `(("{{bg}}" . ,(face-background 'default))
                    ("{{fg}}" . ,(face-foreground 'default))
                    ("{{link}}" . ,(face-foreground 'link nil 'default))
                    ("{{base}}" . ,(url-encode-url (concat "file://" (expand-file-name default-directory))))
                    ("{{line}}" . ,(number-to-string (my/markdown-preview--top-line)))
                    ("{{init}}" . ,(my/markdown-preview--source))))
        (setq html (string-replace (car kv) (cdr kv) html)))
      (let ((coding-system-for-write 'utf-8))
        (write-region html nil file nil 'silent))
      (select-window (split-window-right))
      (xwidget-webkit-browse-url (concat "file://" file) t)
      (setq preview (current-buffer))
      (select-window win)
      (setq my/markdown-preview--buffer preview)
      (add-hook 'after-change-functions #'my/markdown-preview--schedule nil t)
      (add-hook 'window-scroll-functions #'my/markdown-preview--scroll nil t)
      (add-hook 'kill-buffer-hook #'my/markdown-preview--close nil t))))

(provide 'init-markdown)
;;; init-markdown.el ends here
