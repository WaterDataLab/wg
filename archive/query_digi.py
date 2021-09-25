#%%
from devicecloud import DeviceCloud

dc = DeviceCloud('jlambie', 'TempPassword1234')

# show the MAC address of all devices that are currently connected
#
# This is done using Device Cloud DeviceCore functionality
print("== Connected Devices ==")
for device in dc.devicecore.get_devices():
    if device.is_connected():   
        print(device.get_mac())

#%%
# get the name and current value of all data streams having values
# with a floating point type
#
# This is done using Device Cloud stream functionality
strm = dc.streams.get_stream("history/00010000-00000000-03566100-72541226/cl1/cval")
#print (strm.get_data_ttl())
#print (strm.get_rollup_ttl())
print (strm.get_description())  
# %%
