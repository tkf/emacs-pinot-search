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


def gene_iparse_underline_headings(symbols):
    underline_re = re.compile(r'({0})\1* *$'.format(symbols))

    def iparse_underline_headings(lines):
        lines = iter(lines)
        previous = lines.next()
        yield
        for line in lines:
            if underline_re.match(line.rstrip()):
                yield previous
            else:
                yield
            previous = line

    return iparse_underline_headings

iparse_md_underline_headings = gene_iparse_underline_headings('=-')
iparse_rst_underline_headings = gene_iparse_underline_headings(
    '[!-/:-@[-`{-~]')
# See also: docutils.parsers.rst.states.Body.pats['nonalphanum7bit']


HEADING_SHARPS_RE = re.compile("^#{1,6} .+$")


def iparse_sharps_headings(lines):
    for line in lines:
        if HEADING_SHARPS_RE.match(line):
            yield line.strip("#").strip()
        else:
            yield


def first(iterative):
    for item in iterative:
        return item


def get_first_heading(lines, parsers):
    lines = imap(str.rstrip, lines)
    iteratives = map(lambda p, ls: p(ls), parsers, tee(lines, len(parsers)))
    candidates = first(ifilter(any, izip(*iteratives)))
    return first(ifilter(None, candidates))  # get non-None candidate


def get_title_rst(path):
    return get_first_heading(
        open(path).xreadlines(),
        [iparse_rst_underline_headings])


def get_title_md(path):
    return get_first_heading(
        open(path).xreadlines(),
        [iparse_md_underline_headings, iparse_sharps_headings])


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
    from argparse import ArgumentParser
    parser = ArgumentParser(description=__doc__)
    parser.add_argument(
        'pinot_args', nargs='+',
        help='Arguments passed to pinot-search.')
    ns = parser.parse_args(args)
    pinot_search(ns.pinot_args)


if __name__ == '__main__':
    main()
