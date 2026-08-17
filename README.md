# A recursive scheduler written in F#.

Run by doing `dotnet run`.

Currently, input file must be `input.json` and results will be written in `result.json`, but will add option for command line filename inputs in a future commit.

Currently supports available slots using time ranges, preferences and avoidances by the hour, and slots are on the hour. Future commits will add more granular slots (maybe 30min) as well as flexible break times (lunch, coffee, etc).

The algorithm chooses the next student to process based on MRV, and then chooses the time based on LRV, with dead ending and branch pruning.

## Sample input.json

In `slots` and `availableSlots` you have a 5-element array (representing each business day), each with an array of time ranges, which are represented by `[start, end]`. Empty days are just a single empty array (not a nested empty one). For preferences and avoidances, these 5-element arrays just contain an array of preferred times (not ranges). Days are 0-indexed to Monday. Day preferences and avoidances are given +/- 10 weight, and times are given +/- 5. Days and times preferences and avoidances are not mutually exclusive, with day meaning any time in the day (so they get a double benefit if it's on the preferred day and time). Preferences and avoidances do not have to be present.

```
{
    "slots": [
        [[900, 1200], [1300, 1600]],
        [[900, 1200], [1400, 1600]],
        [[900, 1200]],
        [],
        [[1300, 1700]]
    ],
    "students": [{
        "name": "Eff Sharp",
        "availableSlots": [
            [[900, 1100], [1300, 1500], [1700, 1900]],
            ...
        ],
        "preferences": {
            "times": [[1000, 1400, 1800], [], [1500], [900], []],
            "days": [0, 1]
        },
        "avoidances": {
            ...
        }
    },
    {
        ...
    },
    ...
    ]
}

```

## Sample result.json

The result will have an array of all schedules with the maximum score.

```
[
    {
        "Score": 15
        "Schedule": {
            "Mon": [
                { "Time": 900, "Name": "Eff Sharp", "Score": { "Preference": 0, "Avoidance": 0, "Total": 0 }}
                ...
            ]
        }
    },
    {
        ...
    },
    ...
]

```