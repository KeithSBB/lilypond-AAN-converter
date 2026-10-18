#!/usr/bin/env python3
"""Fail if a Russian C-major cue sounds, or the chord tones are missing.

LilyPond writes the MIDI file after the .ly file has finished executing,
so test-convert.ly calls this script from write-performances-midis.
Expected pitches for \\aaan-convert { c'1^\"M\" } in Russian: E3 G3 C4.
The cue is C3 (note 48) and must be absent.
"""
import struct
import sys


def notes(path):
    data = open(path, "rb").read()
    if data[:4] != b"MThd":
        raise SystemExit("not a MIDI file: %s" % path)
    hl = struct.unpack(">I", data[4:8])[0]
    _fmt, ntr, _div = struct.unpack(">HHH", data[8:8 + hl])
    pos = 8 + hl
    found = []
    for _ti in range(ntr):
        if data[pos:pos + 4] != b"MTrk":
            raise SystemExit("bad MIDI track")
        ln = struct.unpack(">I", data[pos + 4:pos + 8])[0]
        pos += 8
        end = pos + ln
        running = None
        while pos < end:
            while True:
                b = data[pos]
                pos += 1
                if not b & 0x80:
                    break
            b = data[pos]
            if b >= 0x80:
                pos += 1
                status = b
                running = status
            else:
                status = running
            if status == 0xFF:
                pos += 1
                ln2 = 0
                while True:
                    b = data[pos]
                    pos += 1
                    ln2 = (ln2 << 7) | (b & 0x7F)
                    if not b & 0x80:
                        break
                pos += ln2
                continue
            hi = status & 0xF0
            if hi in (0x80, 0x90, 0xA0, 0xB0, 0xE0):
                d1 = data[pos]
                d2 = data[pos + 1]
                pos += 2
            elif hi in (0xC0, 0xD0):
                d1 = data[pos]
                d2 = None
                pos += 1
            else:
                raise SystemExit("bad MIDI status %s" % status)
            if hi == 0x90 and d2:
                found.append(d1)
        pos = end
    return found


def main():
    if len(sys.argv) != 2:
        raise SystemExit("usage: check-cue-midi.py FILE.midi")
    got = set(notes(sys.argv[1]))
    cue = 48
    chord = {52, 55, 60}
    if cue in got:
        raise SystemExit("cue note 48 is in %s: %s" % (sys.argv[1], sorted(got)))
    missing = chord - got
    if missing:
        raise SystemExit("chord notes %s missing from %s: %s" % (sorted(missing), sys.argv[1], sorted(got)))
    print("cue MIDI ok:", sorted(got))


if __name__ == "__main__":
    main()
