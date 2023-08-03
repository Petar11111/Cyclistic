# Cyclistic
Data Analysis project

BULK INSERT Cyclistic_Df
FROM 'C:\Users\Owner\Desktop\Project 1\Cyclistic.csv'
WITH (
    FIELDTERMINATOR = ',',
    ROWTERMINATOR = '\n',
    FIRSTROW = 2
);

SELECT TOP 10 * from Cyclistic_Df;

SELECT TOP 10 
	start_station_name AS Start,
	end_station_name AS [End]
FROM Cyclistic_Df;

SELECT COUNT(*) AS NumberOfRows
FROM Cyclistic_Df;

SELECT DISTINCT(start_station_name)
FROM Cyclistic_Df;


SELECT COUNT(*) AS NumberOfRows
FROM Cyclistic_Df;

SELECT COUNT(*) AS NUllCount 
FROM Cyclistic_Df
WHERE end_station_name IS NULL;

SELECT start_station_name, COUNT(*) AS popularity
FROM Cyclistic_Df
GROUP BY start_station_name
ORDER BY popularity DESC;
