./fuel_wx_ign.py \
	--name='ThomasFire' --outdir='./ThomasFire_Chris' \
	--center_lon=-119.3022 --center_lat=34.4000 \
	--do_fuel=True \
	--fuel_source='landfire' --fuel_version='2.4.0' \
	--do_wx=True \
	--wx_type='historical' --wx_start_time="2017-12-05 00:00" --wx_num_hours=24 \
	--do_ignition=True \
	--point_ignition=True --ignition_lon=-121.3 --ignition_lat=40.2 --ignition_radius=300.