-- this is the first version. just use it as example sql which are not robust.
-- the aim was to do in a few sqls with many joins.


--- An entry with a value 1 to n represents an inventor, an entry with
--- the value 0 does not represent an inventor,
drop table publn_count_wiod;
select t201.appln_id, t211.pat_publn_id, count(distinct(t906.person_ctry_code)) as country_count,
max(t227.invt_seq_nr) as invt_count
into publn_count_wiod
from tls906_person t906, tls227_pers_publn t227, tls211_pat_publn t211, tls201_appln t201
where t906.person_id = t227.person_id
and t227.pat_publn_id = t211.pat_publn_id
and t227.invt_seq_nr <> 0
and t211.publn_first_grant = 1
and t201.appln_id = t211.appln_id
and t201.appln_filing_date > '2000-01-01'
and t201.doc_db_family_id <> 0
and t201.appln_kind = 'W'
and t211.publn_auth = 'WO'
group by t201.appln_id, t211.pat_publn_id;

---------------------------------------------------------------
---------------------------------------------------------------
---------------------------------------------------------------
-- starting from here
drop table wiod_pct_publn_count;
select distinct t201.appln_id, t211.pat_publn_id,
count(t906.person_id) as inventor_per_country, t906.person_ctry_code
into wiod_pct_publn_count
from tls906_person t906, tls227_pers_publn t227, tls211_pat_publn t211, tls201_appln t201
where t906.person_id = t227.person_id
and t227.pat_publn_id = t211.pat_publn_id
and t227.invt_seq_nr <> 0
and t201.appln_id = t211.appln_id
and t201.appln_filing_date > '2000-01-01'
and t211.publn_auth = 'WO'
group by t201.appln_id, t211.pat_publn_id, t906.person_ctry_code;

-- this table to be joined to the previous one
drop table wiod_pct_publn_count_next;
select appln_id, pat_publn_id, sum(inventor_per_country) as total_inventor
into wiod_pct_publn_count_next
from wiod_pct_publn_count
group by appln_id, pat_publn_id;

select * from wiod_pct_publn_count_next limit 10;
select * from wiod_pct_publn_count limit 10;

-- joining two tables
drop table wiod_pct_publn_count_extra;
select c.appln_id, c.pat_publn_id, c.inventor_per_country,
c.person_ctry_code, n.total_inventor
into wiod_pct_publn_count_extra
from wiod_pct_publn_count_next n left join wiod_pct_publn_count c on 
c.pat_publn_id = n.pat_publn_id;

-- checking the left join
select count(*) from wiod_pct_publn_count;
select count(*) from wiod_pct_publn_count_next;
select count(*) from wiod_pct_publn_count_extra;
-- seems ok

-- listing publications with inventors from different countries 
select *
from wiod_pct_publn_count_extra 
where inventor_per_country < total_inventor
limit 20;

-- final table with share of countries in a single patent
-- calculated with the number of inventors.
drop table wiod_pct_publn_count_final;
select appln_id, pat_publn_id, inventor_per_country, person_ctry_code, total_inventor,
inventor_per_country/total_inventor as country_share
into wiod_pct_publn_count_final
from wiod_pct_publn_count_extra;


select * from wiod_pct_publn_count_extra order by appln_id limit 20;

-- same appln_id have two pat_publn_id
-- appln_id = 6705
-- pat_publn_id = 277709334 and 6706

select * from wiod_pct_publn_count_final where pat_publn_id in (277709334, 6706);

select * from tls211_pat_publn
where pat_publn_id in (277709334, 6706);
-- I am removing all duplicate appln_id
