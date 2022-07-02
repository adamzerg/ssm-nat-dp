# Macau's city-wide Nucleic Acid Testing 2022

## Version Master

Master contorlling of all people NAT period, certain master data can change from time to time including period, location Sno code etc.

| Version Number | Period Start | Period End |
| ------- | ---------------- | ---------------- |
| 20220619| 2022-06-19 12:00 | 2022-06-21 12:00 |
| 20220623| 2022-06-23 09:00 | 2022-06-25 00:00 |
| 20220627| 2022-06-27 09:00 | 2022-06-28 18:00 |

## Location Master

Infomation of location, Sno code, longitude ,latitude, area divided

## Data Sources

### 1.APTMON
Scraped from https://eservice.ssm.gov.mo/aptmon/ch only when the site is available

### 2.RNA010
Downloaded from https://www.ssm.gov.mo/docs/stat/apt/RNA010.xlsx

## Run Steps

1. Decompress data from data.7z  
   - 00-ssm-nat-extract-data.R

2. Ensure a map key is ready for calling google API  
   - keys.json

3. Input version-master and regenerate location master if new perod is introduced  
   - 01-ssm-nat-location-master.R

4. Generate map into html  
   - 02-ssm-nat-dp-map.R
