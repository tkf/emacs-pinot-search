====================================================
 Search your desktop using Pinot_ from Emacs
====================================================

Pinot_ is an application for "Personal search and metasearch for the
Free Desktop".  pinot.el provides interface to incrementally search
your desktop using Emacs's helm/anything interface.

.. _pinot: http://code.google.com/p/pinot-search/


Requirements
============

* pinot_ (of course)
* helm.el or anything.el


Install
=======

1. Using el-get_

   Just do ``M-x el-get-install RET pinot``.  That's all.

2. Manual install

   Download pinot.el, put somewhere in your `load-path`, and then
   add this in your configuration file::

     (autoload 'helm-pinot-search "pinot" nil t)

   or if you are using anything.el::

     (autoload 'anything-pinot-search "pinot" nil t)

.. _el-get: https://github.com/dimitri/el-get


Usage
=====

`M-x helm-pinot-search` or `M-x anything-pinot-search` does the job.
