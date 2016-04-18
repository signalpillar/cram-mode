Simple major mode for the `cram <https://pypi.python.org/pypi/cram>`_ files. Major adds syntax highlighting.
``cram-mode`` is automatically enabled for files with '.t' extension.

Installation
------------

Package is not present in any of known package archives.

Installation procedure for Spacemacs users look the following

- Add package to the ``dotspacemacs-additional-packages``::

   dotspacemacs-additional-packages `(
    (cram-mode :location (recipe :fetcher github :repo "signalpillar/cram-mode"))
   )

- Add ``(require 'cram-mode)`` in ``dotspacemacs/user-config`` body.


References
----------

* `How to Write a Emacs Major Mode for Syntax Coloring <http://ergoemacs.org/emacs/elisp_syntax_coloring.html>`_
