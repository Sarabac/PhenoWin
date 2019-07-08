CREATE VIEW IF NOT EXISTS PIXEL_DATE AS
Select strftime("%Y-%m-%d", (Year||'-01-01'), '+'||DOY||' day', '-1 day') as DATE, * from PIXEL;

CREATE VIEW IF NOT EXISTS START_END AS
select min(DATE) as START, max(DATE) as END, Area, Crop, Year, P  from PIXEL_DATE group by Area, Crop, Year, P;

CREATE VIEW IF NOT EXISTS Period_length AS
select min(START) as START, julianday(max(end)) - julianday(min(start)) as len from START_END;

CREATE VIEW IF NOT EXISTS Total_period AS
WITH RECURSIVE
  org(n) AS (
    VALUES(1)
    UNION
    SELECT n+1 FROM org, Period_length
     WHERE n<=len
  )
SELECT date(START, "+"||n||" day") as days FROM org, Period_length
