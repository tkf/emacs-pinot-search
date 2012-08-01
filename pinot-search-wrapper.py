#!/usr/bin/env python

"""
Wrap `pinot-search --toxml -` to get right document title from text markups.

Supported markups: reStructuredText, Markdown.
"""

# Copyright (C) 2012 Takafumi Arakaki

# Author: Takafumi Arakaki <aka.tkf at gmail.com>

# This file is NOT part of GNU Emacs.

# pinot-search-wrapper.py is free software: you can redistribute it
# and/or modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation, either version 3 of
# the License, or (at your option) any later version.

# pinot-search-wrapper.py is distributed in the hope that it will be
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty
# of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with pinot-search-wrapper.py.
# If not, see <http://www.gnu.org/licenses/>.


import sys
import os
import re
from itertools import imap, ifilter, izip, tee
from xml.dom import minidom
import subprocess


def get_title_rst(path):
    try:
        from docutils.core import  publish_doctree
        from docutils import io
        from docutils import nodes
    except ImportError:
        return

    doc = publish_doctree(None, path, io.FileInput)
    for nd in doc.traverse(nodes.title):
        return nd.astext().encode('utf-8')


HEADING_UNDERLINE_RE = re.compile("^(=+|-+)$")
HEADING_SHARPS_RE = re.compile("^#{1,6} .+$")


def iparse_underline_headings(lines):
    lines = iter(lines)
    previous = lines.next()
    yield
    for line in lines:
        if HEADING_UNDERLINE_RE.match(line.rstrip()):
            yield previous
        else:
            yield
        previous = line


def iparse_sharps_headings(lines):
    for line in lines:
        if HEADING_SHARPS_RE.match(line):
            yield line.strip("#").strip()
        else:
            yield


def first(iterative):
    for item in iterative:
        return item


def get_first_heading(lines):
    lines = imap(str.rstrip, lines)
    parsers = [iparse_underline_headings, iparse_sharps_headings]
    iteratives = map(lambda p, ls: p(ls), parsers, tee(lines, len(parsers)))
    candidates = first(ifilter(any, izip(*iteratives)))
    return first(ifilter(None, candidates))  # get non-None candidate


def get_title_md(path):
    return get_first_heading(open(path).xreadlines())


exts_func = [
    (('rst', 'rest'), get_title_rst),
    (('md', 'markdown'), get_title_md),
]

dispatcher = dict((ext, func) for (exts, func) in exts_func for ext in exts)


def get_title(path):
    """
    Get title of the document at `path` or None if cannot be retrieved.
    """
    ext = os.path.splitext(path)[1].lower()[1:]
    func = dispatcher.get(ext)
    if func:
        return func(path)


def fix_title(dom):
    """
    Fix document titles in `dom` IN-PLACE.
    """
    for item in dom.getElementsByTagName('item'):
        title = first(item.getElementsByTagName('title'))
        link = first(item.getElementsByTagName('link'))
        if title and link:
            path = link.firstChild.nodeValue
            if not path.startswith('file://'):
                continue
            path = path[len('file://'):]
            real_title = get_title(path)
            if real_title:
                title.firstChild.nodeValue = real_title


def pinot_search(args):
    proc = subprocess.Popen(
        ['pinot-search', '--toxml', '-'] + args,
        stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    dom = minidom.parse(proc.stdout)
    fix_title(dom)
    dom.writexml(sys.stdout)


def main(args=None):
    if args is None:
        args = sys.argv[1:]
    pinot_search(args)


if __name__ == '__main__':
    main()
