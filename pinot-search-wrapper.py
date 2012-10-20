#!/usr/bin/env python

"""
Wrap `pinot-search --toxml -` to get right document title from text markups.

Supported markups: reStructuredText, Markdown, org-mode.
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
import codecs


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


def gene_iparse_prefix_headings(regexp):
    prefix_re = re.compile(r'^{0} .+'.format(regexp))

    def iparse_prefix_headings(lines):
        for line in lines:
            if prefix_re.match(line):
                yield line.strip("#").strip()
            else:
                yield

    return iparse_prefix_headings

iparse_md_underline_headings = gene_iparse_underline_headings(r'[=\-]')
iparse_rst_underline_headings = gene_iparse_underline_headings(
    '[!-/:-@[-`{-~]')
# See also: docutils.parsers.rst.states.Body.pats['nonalphanum7bit']

iparse_sharps_headings = gene_iparse_prefix_headings('#{1,6}')
iparse_asterisk_headings = gene_iparse_prefix_headings(r'\*+')


def first(iterative):
    for item in iterative:
        return item


def get_first_heading(lines, parsers):
    lines = imap(str.rstrip, lines)
    iteratives = map(lambda p, ls: p(ls), parsers, tee(lines, len(parsers)))
    candidates = first(ifilter(any, izip(*iteratives)))
    if candidates:
        return first(ifilter(None, candidates))  # get non-None candidate


def get_title_rst(path):
    return get_first_heading(
        open(path).xreadlines(),
        [iparse_rst_underline_headings])


def get_title_md(path):
    return get_first_heading(
        open(path).xreadlines(),
        [iparse_md_underline_headings, iparse_sharps_headings])


def get_title_org(path):
    return get_first_heading(
        open(path).xreadlines(),
        [iparse_asterisk_headings])


exts_func = [
    (('rst', 'rest'), get_title_rst),
    (('md', 'markdown'), get_title_md),
    (('org',), get_title_org),
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
                title.firstChild.nodeValue = unicode(
                    real_title, encoding='UTF-8')


def ouput_dom(dom):
    fix_title(dom)
    dom.writexml(codecs.getwriter('UTF-8')(sys.stdout))


def pinot_search(args):
    proc = subprocess.Popen(
        ['pinot-search', '--toxml', '-'] + args,
        stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    dom = minidom.parse(codecs.getreader('UTF-8')(proc.stdout))
    ouput_dom(dom)


def strip_tags(text):
    """Strip tags in description.  Currently only for <b> tag."""
    return text.replace('<b>', '').replace('</b>', '')


def dbus_reply_to_xml(hitslist):
    doc = minidom.Document()
    rss = doc.createElement('rss')
    rss.setAttribute('version', '2.0')
    doc.appendChild(rss)
    channel = doc.createElement('channel')
    rss.appendChild(channel)

    for hit in map(dict, hitslist):
        item = doc.createElement('item')
        channel.appendChild(item)

        title = doc.createElement('title')
        link = doc.createElement('link')
        description = doc.createElement('description')
        map(item.appendChild, [title, link, description])

        for (node, key) in [(title, 'caption'),
                            (link, 'url')]:
            node.appendChild(doc.createTextNode(hit[key]))

        if 'extract' in hit:
            text = strip_tags(hit['extract'])
            description.appendChild(doc.createTextNode(text))

    return doc


def pinot_dbus_search(args):
    import dbus
    bus = dbus.SessionBus()
    session = bus.get_object('de.berlios.Pinot', '/de/berlios/Pinot')
    query_method = session.get_dbus_method('Query')
    (engine_type, engine_name, search_text) = args
    (estimated_hits, hitslist) = query_method(
        engine_type, engine_name, search_text,
        dbus.UInt32(0), dbus.UInt32(10),
        timeout=1)
    dom = dbus_reply_to_xml(hitslist)
    ouput_dom(dom)


def main(args=None):
    from argparse import ArgumentParser
    parser = ArgumentParser(description=__doc__)
    parser.add_argument(
        '--dbus', action='store_true', default=False)
    parser.add_argument(
        'pinot_args', nargs='+',
        help='Arguments passed to pinot-search.')
    ns = parser.parse_args(args)
    if ns.dbus:
        pinot_dbus_search(ns.pinot_args)
    else:
        pinot_search(ns.pinot_args)


if __name__ == '__main__':
    main()
