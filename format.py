#!/usr/bin/env python3
"""Turn the scheduler's JSON output into an HTML page (or email paste) with:

  1. one table with the weeks as columns, listing each day's lessons
  2. a second table listing each student's lessons

Usage:
    python format_schedule.py schedule.json --start 2026-10-26 -o schedule.html

--start is the date of the MONDAY of week 1. Open the HTML file in a browser,
select all, copy, and paste into an email.
"""

import argparse
import html
import json
import sys
from collections import defaultdict
from datetime import date, timedelta

DAY_LONG = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"]
DAY_SHORT = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]

CELL = "border:1px solid #888; padding:8px 12px; vertical-align:top;"


def fmt_time(t: int) -> str:
    """900 -> '09:00', 1330 -> '13:30'"""
    return f"{t // 100:02d}:{t % 100:02d}"


def fmt_date(d: date) -> str:
    """No leading zeros: 10/26, 11/4"""
    return f"{d.month}/{d.day}"


def load_solution(path: str, index: int) -> dict:
    with open(path, encoding="utf-8-sig") as f:  # utf-8-sig tolerates a BOM
        data = json.load(f)
    if isinstance(data, list):
        if not data:
            sys.exit("The JSON contains no solutions.")
        if index >= len(data):
            sys.exit(f"Only {len(data)} solution(s) in the file; --solution {index} is out of range.")
        return data[index]
    return data


def collect_sessions(solution: dict, start: date):
    """Returns (by_day, by_student).

    by_day:     {week: {day_index: [(time, student_name), ...]}}
    by_student: {student_name: [(date, day_index, time), ...]}
    Empty slots (Name == "") are skipped.
    """
    by_day = defaultdict(lambda: defaultdict(list))
    by_student = defaultdict(list)

    for week in solution["Weeks"]:
        w = week["Week"]
        for day in week["Days"]:
            d = day["Day"]
            when = start + timedelta(days=(w - 1) * 7 + d)
            for slot in day["Slots"]:
                name = slot["Name"]
                if not name:
                    continue
                by_day[w][d].append((slot["Time"], name))
                by_student[name].append((when, d, slot["Time"]))

    return by_day, by_student


def make_display_names(names, full_names: bool) -> dict:
    """First names only, unless two students share one (then use full names for those)."""
    if full_names:
        return {n: n for n in names}
    firsts = [n.split()[0] for n in names]
    return {
        n: (n.split()[0] if firsts.count(n.split()[0]) == 1 else n)
        for n in names
    }


def build_html(solution: dict, start: date, full_names: bool) -> str:
    by_day, by_student = collect_sessions(solution, start)
    display = make_display_names(sorted(by_student), full_names)
    esc = html.escape

    # ---- table 1: weeks as columns ----
    week_cells = []
    for w in sorted(by_day):
        blocks = []
        for d in sorted(by_day[w]):
            when = start + timedelta(days=(w - 1) * 7 + d)
            lines = [f"{DAY_LONG[when.weekday()]} {fmt_date(when)}"]
            for time, name in sorted(by_day[w][d]):
                lines.append(f"{fmt_time(time)} {esc(display[name])}")
            blocks.append(
                '<div style="margin-bottom:1em">' + "<br>".join(lines) + "</div>"
            )
        week_cells.append(f'<td style="{CELL}">' + "".join(blocks) + "</td>")

    table1 = (
        '<table style="border-collapse:collapse"><tr>'
        + "".join(week_cells)
        + "</tr></table>"
    )

    # ---- table 2: lessons by student ----
    rows = []
    for name in sorted(by_student, key=lambda n: display[n].lower()):
        lines = [
            f"{fmt_date(when)} {DAY_SHORT[when.weekday()]} {fmt_time(time)}"
            for when, _d, time in sorted(by_student[name])
        ]
        rows.append(
            "<tr>"
            f'<td style="{CELL} vertical-align:middle">{esc(display[name])}</td>'
            f'<td style="{CELL}">' + "<br>".join(lines) + "</td>"
            "</tr>"
        )

    table2 = '<table style="border-collapse:collapse">' + "".join(rows) + "</table>"

    return (
        "<!DOCTYPE html>\n<html><head><meta charset=\"utf-8\">"
        "<title>Makeup schedule</title></head>\n<body>\n"
        + table1
        + "\n<p>If you want to see your lessons by name:</p>\n"
        + table2
        + "\n</body></html>\n"
    )


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("json_file", help="the scheduler's output JSON")
    ap.add_argument("--start", required=True, help="date of the Monday of week 1, YYYY-MM-DD (e.g. 2026-10-26)")
    ap.add_argument("-o", "--output", default="schedule.html", help="output HTML file (default: schedule.html)")
    ap.add_argument("--solution", type=int, default=0, help="which solution to use if the JSON has several (default: 0)")
    ap.add_argument("--full-names", action="store_true", help="use full names instead of first names")
    args = ap.parse_args()

    try:
        start = date.fromisoformat(args.start)
    except ValueError:
        sys.exit("--start must look like 2026-10-26")
    if start.weekday() != 0:
        sys.exit(f"--start must be a Monday, but {args.start} is a {DAY_LONG[start.weekday()]}.")

    solution = load_solution(args.json_file, args.solution)
    page = build_html(solution, start, args.full_names)

    with open(args.output, "w", encoding="utf-8") as f:
        f.write(page)
    print(f"Wrote {args.output}")


if __name__ == "__main__":
    main()