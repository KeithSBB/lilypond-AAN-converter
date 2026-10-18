%
% Copyright (C) 2026
% Author: Keith Smith <keith@santabayanian.com>
% aaan-convert.ly
% https://github.com/KeithSBB/lilypond-AAN-converter
%
%  This program is free software; you can redistribute it and/or modify
%  it under the terms of the GNU General Public License, version 3,
%  as published by the Free Software Foundation.
%
%  WARNING: this file under GPLv3 only, not GPLv3+
%
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
%  See the GNU General Public License for more details.  It is
%  available  at
%  http://www.gnu.org/licenses/gpl-3.0.html
%
% Public include.  Pulls in the engraving converter, which includes the
% MIDI converter and the shared American Accordion Notation (AAAN / AAA)
% tables.  One include provides every function those files define.

\version "2.24.4"

\include "aaan-engrave.ly"
