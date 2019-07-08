CREATE VIEW IF NOT EXISTS PIXEL_DATE AS 
Select strftime("%Y-%m-%d", (Year||'-01-01'), '+'||DOY||' day', '-1 day') as DATE, * from PIXEL;

CREATE VIEW IF NOT EXISTS START_END AS 
select min(DATE) as START, max(DATE) as END, Area, Crop, Year, P  from PIXEL_DATE group by Area, Crop, Year, P;

