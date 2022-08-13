---
title: "Holiday Calendars"
slug: "holiday-calendars"
draft: false
images: []
weight: 9903
type: docs
toc: true
---

## Create a custom calendar
Here is how to create a custom calendar. The example given is a french calendar -- so it provides many examples.

    from pandas.tseries.holiday import AbstractHolidayCalendar, Holiday, EasterMonday, Easter
    from pandas.tseries.offsets import Day, CustomBusinessDay

    class FrBusinessCalendar(AbstractHolidayCalendar):
        """ Custom Holiday calendar for France based on
            https://en.wikipedia.org/wiki/Public_holidays_in_France
          - 1 January: New Year's Day
          - Moveable: Easter Monday (Monday after Easter Sunday)
          - 1 May: Labour Day
          - 8 May: Victory in Europe Day
          - Moveable Ascension Day (Thursday, 39 days after Easter Sunday)
          - 14 July: Bastille Day
          - 15 August: Assumption of Mary to Heaven
          - 1 November: All Saints' Day
          - 11 November: Armistice Day
          - 25 December: Christmas Day
        """
        rules = [
            Holiday('New Years Day', month=1, day=1),
            EasterMonday,
            Holiday('Labour Day', month=5, day=1),
            Holiday('Victory in Europe Day', month=5, day=8),
            Holiday('Ascension Day', month=1, day=1, offset=[Easter(), Day(39)]),
            Holiday('Bastille Day', month=7, day=14),
            Holiday('Assumption of Mary to Heaven', month=8, day=15),
            Holiday('All Saints Day', month=11, day=1),
            Holiday('Armistice Day', month=11, day=11),
            Holiday('Christmas Day', month=12, day=25)
        ]


## Use a custom calendar
Here is how to use the custom calendar.

# Get the holidays between two dates

    import pandas as pd
    from datetime import date

    # Creating some boundaries
    year = 2016
    start = date(year, 1, 1)
    end = start + pd.offsets.MonthEnd(12)
    
    # Creating a custom calendar
    cal = FrBusinessCalendar()
    # Getting the holidays (off-days) between two dates
    cal.holidays(start=start, end=end)

    # DatetimeIndex(['2016-01-01', '2016-03-28', '2016-05-01', '2016-05-05',
    #                '2016-05-08', '2016-07-14', '2016-08-15', '2016-11-01',
    #                '2016-11-11', '2016-12-25'],
    #               dtype='datetime64[ns]', freq=None)

# Count the number of working days between two dates

It is sometimes useful to get the number of working days by month whatever the year in the future or in the past. Here is how to do that with a custom calendar.

    from pandas.tseries.offsets import CDay
 
    # Creating a series of dates between the boundaries 
    # by using the custom calendar
    se = pd.bdate_range(start=start, 
                        end=end,
                        freq=CDay(calendar=cal)).to_series()
    # Counting the number of working days by month
    se.groupby(se.dt.month).count().head()

    # 1    20
    # 2    21
    # 3    22
    # 4    21
    # 5    21

