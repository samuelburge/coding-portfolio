SELECT q.*,
	   EXTRACT(SECOND FROM q.turnout_time) + EXTRACT(MINUTE FROM q.turnout_time)*60 + 
	   EXTRACT(HOUR FROM q.turnout_time)*(60^2) AS turnout_seconds,
	   EXTRACT(SECOND FROM q.response_time) + EXTRACT(MINUTE FROM q.response_time)*60 + 
	   EXTRACT(HOUR FROM q.response_time)*(60^2) AS response_seconds

FROM (SELECT 
	turnout_subquery.evt_rin,
	turnout_subquery.unit_id,
	evt_cc_data.jurisdiction,
	turnout_subquery.stn_jurisdiction,
	turnout_subquery.station,
	dc_data.prime_or_backup,
	evt_cc_data.final_case_type,
	evt_cc_data.occ_date,
	--SUBSTRING(final_case_type FROM 3 FOR 1) AS priority_pull,
	/* determines the priority of the call based on previous coding (third value in the string for final_case_type)
	or just assigns the value */
	--CASE WHEN (evt_cc_data.occ_date < '2020-09-29' AND SUBSTRING(final_case_type FROM 3 FOR 1) = 'A') THEN 3
	--WHEN (evt_cc_data.occ_date < '2020-09-29' AND SUBSTRING(final_case_type FROM 3 FOR 1) = 'B') THEN 2
	--WHEN (evt_cc_data.occ_date < '2020-09-29' AND SUBSTRING(final_case_type FROM 3 FOR 1) = 'C') THEN 1
	--WHEN (evt_cc_data.occ_date < '2020-09-29' AND SUBSTRING(final_case_type FROM 3 FOR 1) = 'D') THEN 1
	--WHEN (evt_cc_data.occ_date < '2020-09-29' AND SUBSTRING(final_case_type FROM 3 FOR 1) = 'E') THEN 1
	--ELSE 1 END AS priority,
	
	MAX(turnout_subquery.duration) AS turnout_time,   -- The subquery duplicates some values, MAX function removes any duplicates
	MAX(response_subquery.response_time) AS response_time, -- The subquery duplicates some values, MAX function removes any duplicates
	evt_cc_data.cleared_by,
	event.zone

FROM 
		/* First subquery, which filters over the status_times table to calculate 
		 the turnout time for each vehicle that responded to each call */
		(SELECT 
		  	evt_rin,
		  	stn_jurisdiction,
	      	stn AS station,
		  	unit_id,
		  	status_code,
		  	status_time,
		 	(LEAD(status_time) OVER 
		  		(PARTITION BY (evt_rin, unit_id) ORDER BY status_time, status_code) - status_time) AS duration
	
		FROM status_times

		WHERE stn_jurisdiction = 'SF'
			--AND evt_rin = 'CLBF21000000128'  
			AND(status_code = 'DP' OR status_code = 'E')


		LIMIT 10 ) AS turnout_subquery

RIGHT JOIN (
	SELECT 
		sub1.evt_rin,
		sub1.unit_id,
		SUM(sub1.duration) AS response_time

	FROM 
	/* Second subquery, which filters over the status_times table again to calculate 
	 the response time for each vehicle that responded to each call. This is what will be
	 joined onto the turnout subquery (it's doing the same query twice, but using
	 the different response status codes to calculate different time intervals) */
		(SELECT 
			evt_rin,
			stn_jurisdiction,
			stn AS station,
			unit_id,
			status_code,
			status_time,
			(LEAD(status_time) OVER 
				(PARTITION BY (evt_rin, unit_id) ORDER BY status_time, status_code) - status_time) AS duration
	
			FROM status_times

			WHERE stn_jurisdiction = 'SF'
			 --AND evt_rin = 'CLBF21000000128'  
			AND(status_code = 'DP' OR status_code = 'S') ) AS sub1

		GROUP BY sub1.evt_rin, sub1.unit_id) AS response_subquery 
		
ON (response_subquery.evt_rin = turnout_subquery.evt_rin AND response_subquery.unit_id = turnout_subquery.unit_id)

/* The remaining joins are straightforward; these tables only have one record
 for each call and/or vehicle, and I'm using that to pull in some other fields
 at the top */

INNER JOIN dc_data ON (turnout_subquery.evt_rin = dc_data.evt_rin AND turnout_subquery.unit_id = dc_data.unit_id)
INNER JOIN event ON (turnout_subquery.evt_rin = event.evt_rin)
INNER JOIN evt_cc_data ON (turnout_subquery.evt_rin = evt_cc_data.evt_rin)


WHERE turnout_subquery.evt_rin IS NOT NULL AND turnout_subquery.unit_id IS NOT NULL
AND evt_cc_data.final_case_type NOT LIKE 'HOLD%'
AND evt_cc_data.final_case_type NOT LIKE 'DUP%'

-- Grouping by the call, unit, etc. for the MAX functions above to work properly and not duplicate values
GROUP BY
		 turnout_subquery.evt_rin,
	 	 turnout_subquery.unit_id,
		 evt_cc_data.jurisdiction,
		 turnout_subquery.stn_jurisdiction,
		 turnout_subquery.station,
		 evt_cc_data.final_case_type,
		 evt_cc_data.occ_date,
		 dc_data.prime_or_backup,
		 evt_cc_data.cleared_by,
		 event.zone
		 
ORDER BY occ_date DESC) AS q;
