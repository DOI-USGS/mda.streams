

#Get site ids to populate to SB
site_ids = init_nwis_sites(p_codes = c('00010', '00060', '00095', '00300'))

parent_sbitem = '5487139fe4b02acb4f0c8110'
session = authenticate_sb('lwinslow@usgs.gov')

for(i in 1:length(site_ids)){
	
	tmp = query_item_identifier('mda_streams','site_root', site_ids[i], session)
	if(nrow(tmp) > 0){
		warning('skipping site ', site_ids[i], ', alread exists SBid:', tmp[1,]$id)
		next
	}
	
	id = item_create(parent_id = parent_sbitem, title = site_ids[i], session = session)
	item_update_identifier(id, 'mda_streams','site_root', site_ids[i], session)
}

