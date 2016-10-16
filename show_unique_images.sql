## script to query digikam database

select 
    count(uniquehash) as 'Total Images',
    count(distinct uniquehash) as 'Unique Images',
    count(uniquehash) - count(distinct uniquehash) as 'Duplicate Images'
from
    digikam_very_new.Images;

select
	creationDate as Created,
	count(creationDate) as Number
from
	digikam_very_new.ImageInformation
#where creationDate = '2012-05-22 14:44:24'
group by 
	creationDate
order by
	Number DESC;

select distinct
#	ii.imageid,#	i.name,
	i.album,
	a.relativepath
	
from ImageInformation ii
join Images i on ii.imageid = i.id
join Albums a on i.album = a.id
where ii.creationDate = '1999-03-26 15:47:56'
;
