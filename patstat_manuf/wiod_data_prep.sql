---------------------------------------------------------------
---------------------------------------------------------------
---------------------------------------------------------------
-- obtainin PCT patents
-- Gaétan de Rassenfosse , Hélène Dernis , Geert Boedt (2014)
-- An introduction to the Patstat database with example queries
-- section 2.2 and 2.3

-- obtaining PCT first priority applications 
drop table wiod_pct_appln;
select distinct t201.appln_id, t201.appln_filing_year
into wiod_pct_appln
from tls201_appln t201
left outer join tls204_appln_prior t204 on t201.appln_id = t204.appln_id
where t201.appln_filing_date > '2000-01-01'
and t201.appln_kind = 'W'
and t204.appln_id is NULL;

select count(*) from wiod_pct_appln; -- 185.231

-- obtaining published patents
drop table wiod_pct_t211;
select distinct t211.*, w201.appln_filing_year
into wiod_pct_t211
from tls211_pat_publn t211, wiod_pct_appln w201
where t211.appln_id = w201.appln_id
and t211.publn_auth = 'WO';

select count(*) from wiod_pct_t211; -- 189.400

-- obtainin inventors id.
drop table wiod_pct_t227;
select t227.*
into wiod_pct_t227
from tls227_pers_publn t227, wiod_pct_t211 w211
where t227.pat_publn_id = w211.pat_publn_id
and t227.invt_seq_nr <> 0;

select count(*) from wiod_pct_t227; -- 475.330

-- obtaining inventors info
drop table wiot_pct_t906;
select t906.*
into wiod_pct_t906
from wiod_pct_t227 w227, tls906_person t906
where w227.person_id = t906.person_id;

select count(*) from wiot_pct_t906; -- 475.330

\d wiod_pct_t211
\d wiod_pct_t227
\d wiod_pct_t906

-- now, joining all w221, w227, w906 to obtain appln_id, inventor_num-country, country, year
drop table wiod_pct_inv_num_country;
select w211.appln_id, count(w906.person_id) as inventor_num_per_country,
w906.person_ctry_code, w211.appln_filing_year
into wiod_pct_inv_num_country
from wiod_pct_t211 w211, wiod_pct_t227 w227, wiod_pct_t906 w906
where w211.pat_publn_id = w227.pat_publn_id
and w227.person_id = w906.person_id
group by w211.appln_id, w906.person_ctry_code, w211.appln_filing_year;

select count(*) from wiod_pct_inv_num_country; -- 189.633

select * from wiod_pct_inv_num_country order by appln_id limit 20;

-- adding two more columns; total_num_inventors_per_country; share_of_country
drop table wiod_pct_inv_num_country_2;
select appln_id, appln_filing_year, inventor_num_per_country, person_ctry_code,
sum(inventor_num_per_country) as total_inventor
into wiod_pct_inv_num_country_2
from wiod_pct_inv_num_country
group by appln_id, appln_filing_year, inventor_num_per_country, person_ctry_code;

select count(*) from wiod_pct_inv_num_country_2; -- 189.633

--create the last table on pct inventors with share of the country
drop table wiod_pct_inventor_country;
select *, inventor_num_per_country/total_inventor as country_share
into wiod_pct_inventor_country
from wiod_pct_inv_num_country_2;

select count(*) from wiod_pct_inventor_country; -- 189.633

select * from wiod_pct_inventor_country limit 20;

select count(distinct appln_id) from wiod_pct_inventor_country; --  175.358

select sum(country_share) from wiod_pct_inventor_country; -- 189.633

\d wiod_pct_inventor_country

select * from wiod_pct_inventor_country limit 20;

--- obtain the the number of PCT patents filled by countries-inventors in different IPC
-- ipc is 4 char; then choosse in tls209_appln_ipc.ipc_class_level = 'S'
drop table wiod_pct_inventor_country_t209;
select wic.*, t209.ipc_class_symbol
into wiod_pct_inventor_country_t209
from tls209_appln_ipc t209, wiod_pct_inventor_country wic
where t209.appln_id = wic.appln_id;

\d wiod_pct_inventor_country_t209

-- obtaining the first 4 ipc char from the ipc_class_symbol and
-- distinct as the patent can be classified in different sub fields
drop table wiod_pct_inventor_country_per_4ipc;
select distinct appln_id, appln_filing_year, inventor_num_per_country, person_ctry_code,
total_inventor, country_share, substring(ipc_class_symbol, 1, 4) as ipc4
into wiod_pct_inventor_country_per_4ipc
from wiod_pct_inventor_country_t209; -- 292.736

\d wiod_pct_inventor_country_per_4ipc

-- adding nace2 code
drop table wiod_pct_inventor_country_per_nace2;
select distinct wipc.appln_id, wipc.appln_filing_year, wipc.inventor_num_per_country,
wipc.person_ctry_code, wipc.total_inventor, wipc.country_share, wipc.ipc4,
t902.nace2_code, t902.nace2_descr, nace2_weight
into wiod_pct_inventor_country_per_nace2
from wiod_pct_inventor_country_per_4ipc wipc, tls902_ipc_nace2 t902
where wipc.ipc4 = t902.ipc; -- 292.736

\d tls902_ipc_nace2

-- summing up the country_share per country per year for each nace2
-- final table
drop table wiod_pct_yearly_country_industry;
select person_ctry_code, nace2_code, nace2_descr, nace2_weight, appln_filing_year,
sum(country_share) as sum_of_country_share
into wiod_pct_yearly_country_industry
from wiod_pct_inventor_country_per_nace2
group by person_ctry_code, nace2_code, nace2_descr, nace2_weight, appln_filing_year;


-- obtaining the last csv to be used in patstat_data_manip.R
\o patstat_pct_ctry_ind_year.csv
select * from wiod_pct_yearly_country_industry;
\o
